import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/javascript/array.{type Array}
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/string
import gleam/time/timestamp.{type Timestamp}

pub type WebSocket

pub type Event {
  CommandEvent(command: Array(String))
  ParseErrorEvent(data: String)
  HandleErrorEvent(command: Array(String), error: String)
}

@external(javascript, "./websocket_ffi.mjs", "awaitForever")
pub fn await_forever(a: Promise(a)) -> Nil

@external(javascript, "./websocket_ffi.mjs", "setupServer")
pub fn setup_server(port: String, handler: fn(Event) -> Nil) -> Nil

@external(javascript, "./websocket_ffi.mjs", "setupClient")
pub fn setup_client(port: String) -> Promise(WebSocket)

@external(javascript, "./websocket_ffi.mjs", "messageToEmacs")
fn message_to_emacs_js(ws: WebSocket, message: String) -> Promise(Dynamic)

@external(javascript, "./websocket_ffi.mjs", "callInEmacs")
fn call_no_return_js(ws: WebSocket, code: String) -> Promise(Dynamic)

@external(javascript, "./websocket_ffi.mjs", "getEmacsVar")
fn get_emacs_var_js(port: String, var_name: String) -> Promise(Dynamic)

@external(javascript, "./websocket_ffi.mjs", "evalInEmacsWithReturn")
fn eval_in_emacs_with_return_js(
  port: String,
  timeout_seconds: Int,
  code: String,
) -> Promise(Dynamic)

@external(javascript, "./websocket_ffi.mjs", "enableDebug")
pub fn enable_debug(flag: Bool) -> Nil

@external(javascript, "./websocket_ffi.mjs", "updateInitStartTime")
pub fn update_init_start_time(start_time: Timestamp) -> Nil

@external(javascript, "./websocket_ffi.mjs", "getInitStartTime")
pub fn get_init_start_time() -> Timestamp

// Combined decoder that produces our Result type
fn response_decoder() -> decode.Decoder(Result(String, String)) {
  use ok <- decode.field("ok", decode.bool)
  case ok {
    True -> {
      use value <- decode.field("value", decode.string)
      decode.success(Ok(value))
    }
    False -> {
      use error <- decode.field("error", decode.string)
      decode.success(Error(error))
    }
  }
}

fn convert_js_result(result: Dynamic) -> Result(String, String) {
  case decode.run(result, response_decoder()) {
    Ok(decoded) -> decoded
    Error(errors) ->
      {
        "Failed to decode response: "
        <> string.join(
          errors
            |> list.map(fn(e) {
              "Expected " <> e.expected <> " but found " <> e.found
            }),
          ", ",
        )
      }
      |> Error
  }
}

pub fn message_to_emacs(
  ws: WebSocket,
  message: String,
) -> Promise(Result(String, String)) {
  message_to_emacs_js(ws, message) |> promise.map(convert_js_result)
}

pub fn call_no_return(
  ws: WebSocket,
  code: String,
) -> Promise(Result(String, String)) {
  call_no_return_js(ws, code) |> promise.map(convert_js_result)
}

pub fn get_emacs_var(
  port: String,
  var_name: String,
) -> Promise(Result(String, String)) {
  get_emacs_var_js(port, var_name) |> promise.map(convert_js_result)
}

pub fn eval_in_emacs_with_return(
  port: String,
  timeout_seconds: Int,
  code: String,
) -> Promise(Result(String, String)) {
  eval_in_emacs_with_return_js(port, timeout_seconds, code)
  |> promise.map(convert_js_result)
}
