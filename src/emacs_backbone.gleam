import argv
import emacs.{type EmacsContext, EmacsContext, RawParam, StringParam}
import gleam/float
import gleam/int
import gleam/io
import gleam/javascript/array
import gleam/json
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import monadic.{type CommandResult, bind, run_sequence}
import package_tracker
import pkg_macro
import unit_executor
import unit_macro
import unit_utils
import websocket.{type Event, CommandEvent, HandleErrorEvent, ParseErrorEvent}

// Define a package tracker type
pub type PackageTracker {
  PackageTracker(total: Int, installed: List(String), pending: List(String))
}

pub fn main() {
  let assert [conductor_port, emacs_port] = argv.load().arguments

  let pws = websocket.setup_client(emacs_port)

  let context =
    EmacsContext(
      enable_debug: False,
      installed_packages_count: 0,
      emacs_port: emacs_port,
      pws: pws,
      get: websocket.get_emacs_var(emacs_port, _),
      call: emacs.call_no_return(pws),
      eval: emacs.eval_with_return(port: emacs_port, timeout_seconds: 5),
    )

  websocket.setup_server(conductor_port, handler(context))
  io.println("Server listening on port " <> conductor_port)
}

fn handler(context: EmacsContext) {
  fn(event: Event) -> Nil {
    case event {
      CommandEvent(command) -> {
        let assert [func, ..args] = command |> array.to_list

        handle_command_event(context, func, args)
        Nil
      }
      ParseErrorEvent(data) -> io.println_error("Failed to parse data:" <> data)
      HandleErrorEvent(command, error) ->
        {
          "Failed to handle command ["
          <> command |> array.to_list |> string.join(" ")
          <> "], error: "
          <> error
        }
        |> io.println_error
    }
  }
}

fn handle_command_event(
  context: EmacsContext,
  func: String,
  args: List(String),
) -> Nil {
  case func {
    "init" -> {
      websocket.update_init_start_time(timestamp.system_time())
      init(context)
      Nil
    }
    "package_installed" -> {
      let assert [package_name, ..] = args

      let status = package_tracker.update(package_name)
      case status.all_installed {
        True -> {
          io.println("All packages installed, continuing with configuration")
          after_package_installation_handler(
            EmacsContext(..context, installed_packages_count: status.total),
          )
        }
        False -> Nil
      }
    }
    _ -> unhandle(context, func)
  }
}

fn unhandle(ctx: EmacsContext, func) {
  { "[Backbone] unsupported message: " <> func } |> emacs.message(ctx.pws)
}

fn init(ctx: EmacsContext) -> CommandResult(String) {
  use enable_debug <- bind(ctx.get("emacs-backbone-enable-debug"))
  let enable_debug_flag = enable_debug == "true"
  websocket.enable_debug(enable_debug_flag)
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
  "Starting environment injection" |> io.println

  let result =
    ctx.call("backbone-load-envvars-file", StringParam(["~/.config/backbone/env"]))

  "environment injection completed" |> io.println
  result
}

/// pacakges.el use elpaca to install packages oasynchronously
fn compose_packages_installation(ctx: EmacsContext) -> CommandResult(String) {
  "Starting package installation" |> io.println

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
  emacs.eval_with_return(port: ctx.emacs_port, timeout_seconds: 10 * 60)(
    "load-file",
    StringParam([packages_el_path]),
  )
}

fn after_package_installation_handler(ctx: EmacsContext) -> Nil {
  {
    use enable_debug <- bind(ctx.get("emacs-backbone-enable-debug"))
    use _ <- bind({
      EmacsContext(..ctx, enable_debug: enable_debug == "true")
      |> compose_packages_configuration
    })
    compose_finish(ctx)
  }
  Nil
}

fn compose_packages_configuration(ctx: EmacsContext) -> CommandResult(String) {
  "Starting package configuration" |> io.println

  use config_units <- bind(unit_macro.fetch_units(ctx))

  config_units
  |> unit_utils.resolve_units(ctx.enable_debug)
  |> unit_executor.execute_units(ctx)

  io.println("Package configuration fishied")
  monadic.pure("")
}

fn compose_finish(ctx: EmacsContext) -> CommandResult(String) {
  let time_elapsed =
    timestamp.difference(
      websocket.get_init_start_time(),
      timestamp.system_time(),
    )
    |> duration.to_seconds
    |> float.to_string

  io.println("Finished in " <> time_elapsed <> "s")

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
