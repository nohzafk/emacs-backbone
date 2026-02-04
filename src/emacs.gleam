import gleam/json
import gleam/list
import gleam/string
import jsonrpc
import monadic.{type CommandResult}

pub type EmacsContext {
  EmacsContext(
    enable_debug: Bool,
    installed_packages_count: Int,
    get: fn(String) -> CommandResult(String),
    call: fn(String, Param) -> CommandResult(String),
    eval: fn(String, Param) -> CommandResult(String),
  )
}

// Basic operations
pub fn message(message) {
  jsonrpc.show_message("[Backbone] " <> message)
  Nil
}

pub fn get() {
  fn(var_name: String) -> CommandResult(String) {
    jsonrpc.fetch_var(var_name, 5)
  }
}

pub type Param {
  RawParam(args: List(String))
  StringParam(args: List(String))
}

fn format_args(param: Param) -> String {
  case param {
    RawParam(args) -> args |> string.join(" ")
    StringParam(args) ->
      args
      |> list.map(json.string)
      |> list.map(json.to_string)
      |> string.join(" ")
  }
}

/// call_no_return sends a notification to Emacs (fire-and-forget)
/// the message is sent via JSON-RPC notification for Emacs to proceed
/// it means that if the func call fails on Emacs side, the whole process will continue
///
/// on the other hand, eval_with_return sends a JSON-RPC request
/// and it waits for the return value, so we can have control over func call failure on Emacs side
pub fn call_no_return() {
  fn(op: String, param: Param) -> CommandResult(String) {
    let args = format_args(param)
    jsonrpc.eval_code("(" <> op <> " " <> args <> ")")
  }
}

pub fn eval_with_return(timeout_seconds timeout_seconds: Int) {
  fn(op: String, param: Param) -> CommandResult(String) {
    let args = format_args(param)
    jsonrpc.fetch_var("(" <> op <> " " <> args <> ")", timeout_seconds)
  }
}
