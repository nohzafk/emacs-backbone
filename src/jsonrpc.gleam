import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/string
import gleam/time/timestamp.{type Timestamp}

pub type Event {
  RequestEvent(id: Int, method: String, params: Dynamic)
  NotificationEvent(method: String, params: Dynamic)
  ParseErrorEvent(data: String)
  HandleErrorEvent(method: String, error: String)
}

@external(javascript, "./jsonrpc_ffi.mjs", "setupStdioServer")
pub fn setup_stdio_server(handler: fn(Event) -> Nil) -> Nil

@external(javascript, "./jsonrpc_ffi.mjs", "sendResponse")
pub fn send_response(id: Int, result: String) -> Nil

@external(javascript, "./jsonrpc_ffi.mjs", "sendErrorResponse")
pub fn send_error_response(id: Int, code: Int, message: String) -> Nil

@external(javascript, "./jsonrpc_ffi.mjs", "enableDebug")
pub fn enable_debug(flag: Bool) -> Nil

@external(javascript, "./jsonrpc_ffi.mjs", "updateInitStartTime")
pub fn update_init_start_time(start_time: Timestamp) -> Nil

@external(javascript, "./jsonrpc_ffi.mjs", "getInitStartTime")
pub fn get_init_start_time() -> Timestamp

// FFI bindings to Gleam-friendly wrappers that accept primitive args
@external(javascript, "./jsonrpc_ffi.mjs", "showMessage")
fn show_message_js(message: String) -> Promise(Dynamic)

@external(javascript, "./jsonrpc_ffi.mjs", "evalCode")
fn eval_code_js(code: String) -> Promise(Dynamic)

@external(javascript, "./jsonrpc_ffi.mjs", "fetchVar")
fn fetch_var_js(expr: String, timeout_ms: Int) -> Promise(Dynamic)

// Response decoder for {ok: bool, value?: string, error?: string}
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

// --- Typed wrappers ---

/// Send a show-message notification to Emacs (fire-and-forget)
pub fn show_message(message: String) -> Promise(Result(String, String)) {
  show_message_js(message) |> promise.map(convert_js_result)
}

/// Send eval-code notification to Emacs (fire-and-forget)
pub fn eval_code(code: String) -> Promise(Result(String, String)) {
  eval_code_js(code) |> promise.map(convert_js_result)
}

/// Send a fetch-var request to Emacs and await the response
pub fn fetch_var(
  expr: String,
  timeout_seconds: Int,
) -> Promise(Result(String, String)) {
  fetch_var_js(expr, timeout_seconds * 1000)
  |> promise.map(convert_js_result)
}
