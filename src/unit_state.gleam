import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string

pub type UnitState {
  UnitState(failed_units: Set(String), successful_units: Set(String))
}

pub fn new() -> UnitState {
  UnitState(failed_units: set.new(), successful_units: set.new())
}

pub fn mark_failed(state: UnitState, unit_name: String) -> UnitState {
  io.println("[DEBUG] Marking unit '" <> unit_name <> "' as failed")
  UnitState(..state, failed_units: set.insert(state.failed_units, unit_name))
}

pub fn mark_successful(state: UnitState, unit_name: String) -> UnitState {
  UnitState(
    ..state,
    successful_units: set.insert(state.successful_units, unit_name),
  )
}

pub fn has_failed_dependency(state: UnitState, deps: List(String)) -> Bool {
  deps
  |> list.any(fn(dep) {
    let has_failed = set.contains(state.failed_units, dep)
    case has_failed {
      True ->
        io.println("[DEBUG] Dependency '" <> dep <> "' is in failed state")
      False -> Nil
    }
    has_failed
  })
}

pub fn is_failed(state: UnitState, unit_name: String) -> Bool {
  set.contains(state.failed_units, unit_name)
}

pub fn is_successful(state: UnitState, unit_name: String) -> Bool {
  set.contains(state.successful_units, unit_name)
}

pub fn debug_state(state: UnitState) {
  io.println(
    "[DEBUG] Failed units: "
    <> string.join(set.to_list(state.failed_units), ", "),
  )
  io.println(
    "[DEBUG] Successful units: "
    <> string.join(set.to_list(state.successful_units), ", "),
  )
}
