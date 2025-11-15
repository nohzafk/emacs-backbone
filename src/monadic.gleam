import gleam/javascript/promise.{type Promise, await, resolve}
import gleam/list

pub type CommandResult(a) =
  Promise(Result(a, String))

pub fn pure(value: a) -> CommandResult(a) {
  Ok(value) |> resolve
}

pub fn bind(
  cmd: CommandResult(a),
  f: fn(a) -> CommandResult(b),
) -> CommandResult(b) {
  await(cmd, fn(result) {
    case result {
      Ok(value) -> f(value)
      Error(err) -> Error(err) |> resolve
    }
  })
}

pub fn map(cmd: CommandResult(a), f: fn(a) -> b) -> CommandResult(b) {
  bind(cmd, fn(value) { f(value) |> pure })
}

pub fn fail_with(error: String) -> CommandResult(a) {
  Error(error) |> resolve
}

pub fn lift(res: Result(a, String)) -> CommandResult(a) {
  res |> resolve
}

/// transforms a CommandResult to always continue regardless of errors
pub fn continue_with(
  cmd: CommandResult(a),
  on_error: fn(String) -> b,
) -> CommandResult(Result(a, b)) {
  promise.map(cmd, fn(result) {
    case result {
      Ok(value) -> Ok(Ok(value))
      Error(err) -> Ok(Error(on_error(err)))
    }
  })
}

fn run_sequence_loop(
  remaining_actions: List(CommandResult(a)),
  results: List(a),
) -> CommandResult(List(a)) {
  case remaining_actions {
    [] -> Ok(list.reverse(results)) |> resolve
    [action, ..rest] -> {
      use result <- bind(action)
      run_sequence_loop(rest, [result, ..results])
    }
  }
}

pub fn run_sequence(actions: List(CommandResult(a))) -> CommandResult(List(a)) {
  run_sequence_loop(actions, [])
}
