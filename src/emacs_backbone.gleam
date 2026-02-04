import emacs.{type EmacsContext, EmacsContext, RawParam, StringParam}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import jsonrpc.{
  type Event, HandleErrorEvent, NotificationEvent, ParseErrorEvent,
  RequestEvent,
}
import gleam/javascript/promise
import monadic.{type CommandResult, bind, run_sequence}
import package_tracker
import pkg_macro
import unit_executor
import unit_macro
import unit_utils

// Define a package tracker type
pub type PackageTracker {
  PackageTracker(total: Int, installed: List(String), pending: List(String))
}

pub fn main() {
  let context =
    EmacsContext(
      enable_debug: False,
      installed_packages_count: 0,
      get: emacs.get(),
      call: emacs.call_no_return(),
      eval: emacs.eval_with_return(timeout_seconds: 5),
    )

  jsonrpc.setup_stdio_server(handler(context))
  io.println_error("Backbone stdio server started")
}

fn handler(context: EmacsContext) {
  fn(event: Event) -> Nil {
    case event {
      RequestEvent(id, method, params) -> {
        handle_request(context, id, method, params)
        Nil
      }
      NotificationEvent(method, params) -> {
        handle_notification(context, method, params)
        Nil
      }
      ParseErrorEvent(data) ->
        io.println_error("Failed to parse data: " <> data)
      HandleErrorEvent(method, error) ->
        io.println_error(
          "Failed to handle " <> method <> ", error: " <> error,
        )
    }
  }
}

fn handle_request(
  context: EmacsContext,
  id: Int,
  method: String,
  _params: Dynamic,
) -> Nil {
  case method {
    "init" -> {
      jsonrpc.update_init_start_time(timestamp.system_time())
      init(context, id)
      Nil
    }
    _ -> {
      unhandle(context, method)
      jsonrpc.send_error_response(id, -32601, "Unknown method: " <> method)
    }
  }
}

fn handle_notification(
  context: EmacsContext,
  method: String,
  params: Dynamic,
) -> Nil {
  case method {
    "shutdown" -> {
      io.println_error("Received shutdown notification, exiting gracefully")
      // Exit cleanly - Gleam/JS doesn't have a direct exit, but we can use the FFI
      jsonrpc.shutdown()
    }
    "package_installed" -> {
      let name_decoder = {
        use name <- decode.field("name", decode.string)
        decode.success(name)
      }
      case decode.run(params, name_decoder) {
        Ok(package_name) -> {
          let status = package_tracker.update(package_name)
          case status.all_installed {
            True -> {
              io.println_error(
                "All packages installed, continuing with configuration",
              )
              after_package_installation_handler(
                EmacsContext(
                  ..context,
                  installed_packages_count: status.total,
                ),
              )
            }
            False -> Nil
          }
        }
        Error(_) ->
          io.println_error(
            "Failed to decode package_installed params: "
            <> string.inspect(params),
          )
      }
    }
    _ -> unhandle(context, method)
  }
}

fn unhandle(_ctx: EmacsContext, func) {
  { "[Backbone] unsupported message: " <> func } |> emacs.message
}

fn init(ctx: EmacsContext, request_id: Int) -> CommandResult(String) {
  use enable_debug <- bind(ctx.get("emacs-backbone-enable-debug"))
  let enable_debug_flag = enable_debug == "true"
  jsonrpc.enable_debug(enable_debug_flag)
  let ctx = EmacsContext(..ctx, enable_debug: enable_debug_flag)

  use _ <- bind(
    run_sequence(case ctx.enable_debug {
      True -> compose_setup(ctx)
      False -> []
    }),
  )
  use _ <- bind(
    run_sequence([
      compose_env_injection(ctx),
      // Package installation runs asynchronously.
      compose_packages_installation(ctx),
    ]),
  )

  // Respond to the init request
  jsonrpc.send_response(request_id, "ok")
  monadic.pure("")
}

fn compose_setup(ctx: EmacsContext) -> List(CommandResult(String)) {
  [
    ctx.call(
      "popwin:popup-buffer",
      RawParam(["(get-buffer emacs-backbone-buffer-name)"]),
    ),
  ]
}

fn compose_env_injection(ctx: EmacsContext) -> CommandResult(String) {
  "Starting environment injection" |> io.println_error

  let result =
    ctx.call(
      "backbone-load-envvars-file",
      StringParam(["~/.config/backbone/env"]),
    )

  "environment injection completed" |> io.println_error
  result
}

/// pacakges.el use elpaca to install packages asynchronously
fn compose_packages_installation(ctx: EmacsContext) -> CommandResult(String) {
  "Starting package installation" |> io.println_error

  use packages_el_path <- bind(ctx.eval(
    "make-temp-file",
    StringParam(["emacs-backbone"]),
  ))

  let packages_el_path = packages_el_path |> string.replace("\"", "")

  // generate packages.el with notification hooks
  use package_list <- bind(pkg_macro.generate_macro_defined_packages(
    ctx,
    packages_el_path,
  ))

  // Initialize the package tracker in JavaScript
  package_tracker.initialize(package_list)

  // Load the packages.el file
  emacs.eval_with_return(timeout_seconds: 10 * 60)(
    "load-file",
    StringParam([packages_el_path]),
  )
}

fn after_package_installation_handler(ctx: EmacsContext) -> Nil {
  let result = {
    use enable_debug <- bind(ctx.get("emacs-backbone-enable-debug"))
    use _ <- bind({
      EmacsContext(..ctx, enable_debug: enable_debug == "true")
      |> compose_packages_configuration
    })
    compose_finish(ctx)
  }
  // Log errors from the async chain instead of silently swallowing them
  promise.map(result, fn(res) {
    case res {
      Error(err) ->
        io.println_error("[ERROR] Configuration phase failed: " <> err)
      _ -> Nil
    }
  })
  Nil
}

fn compose_packages_configuration(ctx: EmacsContext) -> CommandResult(String) {
  use config_units <- bind(unit_macro.fetch_units(ctx))

  let resolved = unit_utils.resolve_units(config_units, ctx.enable_debug)

  use _ <- bind(unit_executor.execute_units(resolved, ctx))

  monadic.pure("")
}

fn compose_finish(ctx: EmacsContext) -> CommandResult(String) {
  let time_elapsed =
    timestamp.difference(
      jsonrpc.get_init_start_time(),
      timestamp.system_time(),
    )
    |> duration.to_seconds
    |> float.to_string

  io.println_error("Finished in " <> time_elapsed <> "s")

  let message =
    "Backbone loaded "
    <> int.to_string(ctx.installed_packages_count)
    <> " packages in "
    <> time_elapsed
    <> "s"

  use _ <- bind(ctx.call(
    "run-with-timer",
    RawParam([
      "0.5",
      "nil",
      "(lambda () (message "
        <> { message |> json.string |> json.to_string }
        <> "))",
    ]),
  ))

  monadic.pure("")
}
