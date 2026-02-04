import emacs.{type EmacsContext}
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import jsonrpc
import monadic.{type CommandResult}
import unit.{type ConfigUnit, type Dependency, EnvDep, FeatureDep, UnitDep}
import unit_state.{type UnitState}

pub fn execute_units(
  resolved_units: List(List(ConfigUnit)),
  ctx: EmacsContext,
) -> CommandResult(String) {
  let initial_state = unit_state.new()

  // Process all groups sequentially
  use final_state <- monadic.bind(process_all_groups(
    resolved_units,
    ctx,
    initial_state,
  ))

  // Format a final status message based on failed units
  let failed_units = set.to_list(final_state.failed_units)
  case failed_units {
    [] -> {
      io.println_error("All units loaded successfully")
      monadic.pure("")
    }
    units -> {
      let failed_names = string.join(units, ", ")
      io.println_error("Units loaded with some failures: " <> failed_names)
      monadic.pure("")
    }
  }
}

// Process all groups of units, continuing even if some units fail
fn process_all_groups(
  groups: List(List(ConfigUnit)),
  ctx: EmacsContext,
  state: UnitState,
) -> CommandResult(UnitState) {
  case groups {
    [] -> monadic.pure(state)
    [group, ..rest] -> {
      // Process current group
      use new_state <- monadic.bind(process_group(group, ctx, state))
      // Continue with remaining groups
      process_all_groups(rest, ctx, new_state)
    }
  }
}

// Process a group of units, returning a CommandResult of UnitState
fn process_group(
  units: List(ConfigUnit),
  ctx: EmacsContext,
  state: UnitState,
) -> CommandResult(UnitState) {
  case units {
    // No more units to process
    [] -> monadic.pure(state)
    [unit, ..rest] -> {
      // Check for failed dependencies first
      let failed_deps = get_failed_deps(unit, state)

      case failed_deps {
        [] -> {
          // Execute the unit and get a new state
          use execute_result <- monadic.bind(execute_unit_with_fallback(
            unit,
            ctx,
          ))

          // Update state based on execution result
          let new_state = case execute_result {
            Ok(msg) -> {
              case ctx.enable_debug {
                True ->
                  io.println_error(
                    "[SUCCESS] Unit " <> unit.name <> " executed. " <> msg,
                  )
                _ -> Nil
              }

              unit_state.mark_successful(state, unit.name)
            }
            Error(error) -> {
              io.println_error("[ERROR] Unit " <> unit.name <> " failed: " <> error)
              unit_state.mark_failed(state, unit.name)
            }
          }

          // Continue with the rest of the units
          process_group(rest, ctx, new_state)
        }
        deps -> {
          // Unit has failed dependencies, skip it
          let failed_dep_names =
            list.map(deps, fn(dep) { "'" <> dep <> "'" })
            |> string.join(", ")

          io.println_error(
            "[ERROR] Skipping unit '"
            <> unit.name
            <> "' due to failed dependencies: "
            <> failed_dep_names,
          )
          let new_state = unit_state.mark_failed(state, unit.name)

          // Continue with remaining units
          process_group(rest, ctx, new_state)
        }
      }
    }
  }
}

fn execute_unit_with_fallback(
  unit: ConfigUnit,
  ctx: EmacsContext,
) -> CommandResult(Result(String, String)) {
  // First check prerequisites with error handling
  let safe_prereq =
    verify_runtime_deps(unit, ctx)
    |> monadic.continue_with(fn(err) { "Runtime dependency failure: " <> err })

  // Now use the safer prereq check that doesn't stop on errors
  use prereq_result <- monadic.bind(safe_prereq)

  case prereq_result {
    // execute_unit_with_fallback is called by monadic.bind
    // returning an Promise(Error) will stop the whole process
    // we are allowing partial failure, so the whole result is Promise(Ok)
    // and process_group will mark the unit as success or failure according to the inner result
    Error(err) -> {
      // Don't try loading the file if prerequisites failed
      Error(err) |> Ok |> monadic.lift
    }
    Ok(_) -> {
      case unit.code {
        None -> Ok("Empty body") |> Ok |> monadic.lift
        Some(body) -> {
          let result = jsonrpc.fetch_var(body, 5)

          promise.map(result, fn(res) {
            case res {
              Ok("true") -> Ok("")
              Ok(err) -> Error("Execution error: " <> err)
              Error(err) -> Error(err)
            }
            |> Ok
          })
        }
      }
    }
  }
}

// Get a list of dependencies that have failed
fn get_failed_deps(unit: ConfigUnit, state: UnitState) -> List(String) {
  unit.deps
  |> option.unwrap([])
  |> list.filter_map(fn(dep) {
    case dep {
      UnitDep(unit_config_name) -> {
        case unit_state.is_failed(state, unit_config_name) {
          True -> Ok(unit_config_name)
          False -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
}

// Verify runtime dependencies, returns a CommandResult
fn verify_runtime_deps(
  unit: ConfigUnit,
  ctx: EmacsContext,
) -> CommandResult(String) {
  case unit.deps {
    None -> monadic.pure("")
    Some(deps) -> verify_runtime_deps_loop(deps, unit, ctx)
  }
}

fn verify_runtime_deps_loop(
  deps: List(Dependency),
  unit: ConfigUnit,
  ctx: EmacsContext,
) -> CommandResult(String) {
  case deps {
    [] -> {
      case ctx.enable_debug {
        True -> io.println_error("Unit " <> unit.name <> " dependency checks passed")
        _ -> Nil
      }
      monadic.pure("")
    }
    [dep, ..rest] -> {
      // Check this dependency
      let check_result = case dep {
        EnvDep(env_var) -> verify_unit_env_dep(env_var, ctx)
        FeatureDep(feature) -> verify_unit_feature_dep(feature, ctx)
        _ -> monadic.pure("")
      }

      // Only continue checking if this passed
      use _ <- monadic.bind(check_result)
      verify_runtime_deps_loop(rest, unit, ctx)
    }
  }
}

// Verify environment variable dependency
fn verify_unit_env_dep(
  env_var: String,
  ctx: EmacsContext,
) -> CommandResult(String) {
  use value <- monadic.bind(ctx.eval("getenv", emacs.StringParam([env_var])))
  case value {
    "" | "null" -> monadic.fail_with("EnvDep failure: " <> env_var)
    _ -> {
      case ctx.enable_debug {
        True -> io.println_error("EnvDep pass: " <> env_var)
        _ -> Nil
      }
      monadic.pure("")
    }
  }
}

// Verify feature dependency
fn verify_unit_feature_dep(
  feature: String,
  ctx: EmacsContext,
) -> CommandResult(String) {
  use value <- monadic.bind(ctx.eval(
    "featurep",
    emacs.RawParam(["'" <> feature]),
  ))
  case value {
    "true" -> {
      case ctx.enable_debug {
        True -> io.println_error("FeatureDep pass: " <> feature)
        _ -> Nil
      }
      monadic.pure("")
    }
    _ -> {
      io.println_error("FeatureDep failure: " <> feature)
      monadic.fail_with("")
    }
  }
}
