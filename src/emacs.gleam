import gleam/javascript/promise.{type Promise, await}
import gleam/json
import gleam/list
import gleam/string
import monadic.{type CommandResult}
import websocket.{type WebSocket}

pub type EmacsContext {
  EmacsContext(
    enable_debug: Bool,
    installed_packages_count: Int,
    emacs_port: String,
    pws: Promise(WebSocket),
    get: fn(String) -> CommandResult(String),
    call: fn(String, Param) -> CommandResult(String),
    eval: fn(String, Param) -> CommandResult(String),
  )
}

// Basic operations
pub fn message(message, pws) {
  {
    use ws <- await(pws)
    websocket.message_to_emacs(ws, "[Backbone] " <> message)
  }
  Nil
}

pub fn get(port: String) {
  fn(var_name: String) -> CommandResult(String) {
    websocket.get_emacs_var(port, var_name)
  }
}

pub type Param {
  RawParam(args: List(String))
  StringParam(args: List(String))
}

/// call_no_return will always return a Promise(OK("")) value
/// the message is send to a long-live websocket connect for emacs to proceed
/// it means that if the func call fails on Emacs side, the whole process will continue
///
/// on the other hand, eval_with_return use a new websocket connection for every call
/// and it waits for the return value, it means that we can have control over func call failure on Emacs side
pub fn call_no_return(pws) {
  fn(op: String, param: Param) -> CommandResult(String) {
    let args = case param {
      RawParam(args) -> args |> string.join(" ")
      StringParam(args) ->
        args
        |> list.map(json.string)
        |> list.map(json.to_string)
        |> string.join(" ")
    }
    {
      use ws <- await(pws)
      websocket.call_no_return(ws, "(" <> op <> " " <> args <> ")")
    }
  }
}

pub fn eval_with_return(port port: String, timeout_seconds timeout_seconds: Int) {
  fn(op: String, param: Param) -> CommandResult(String) {
    let args = case param {
      RawParam(args) -> args |> string.join(" ")
      StringParam(args) ->
        args
        |> list.map(json.string)
        |> list.map(json.to_string)
        |> string.join(" ")
    }

    websocket.eval_in_emacs_with_return(
      port,
      timeout_seconds,
      "(" <> op <> " " <> args <> ")",
    )
  }
}
