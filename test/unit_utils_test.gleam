import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import unit.{ConfigUnit, UnitDep}
import unit_utils

pub fn resolve_units_reports_missing_after_unit_test() {
  let units = [
    ConfigUnit(name: "embark", deps: None, code: None),
    ConfigUnit(
      name: "embark-consult",
      deps: Some([UnitDep("embark"), UnitDep("consult")]),
      code: None,
    ),
  ]

  let result = unit_utils.resolve_units(units, False)

  result |> should.be_error
  let assert Error(message) = result
  string.contains(
    does: message,
    contain: "config-unit 'embark-consult' references missing :after unit(s): consult",
  )
  |> should.equal(True)
  string.contains(
    does: message,
    contain: "`:after` must reference other config-unit! names, not package! names.",
  )
  |> should.equal(True)
}

pub fn resolve_units_accepts_valid_after_units_test() {
  let units = [
    ConfigUnit(name: "minibuffer-settings", deps: None, code: None),
    ConfigUnit(name: "embark", deps: None, code: None),
    ConfigUnit(
      name: "embark-consult",
      deps: Some([UnitDep("embark"), UnitDep("minibuffer-settings")]),
      code: None,
    ),
  ]

  let result = unit_utils.resolve_units(units, False)

  result |> should.be_ok
  let assert Ok(levels) = result
  levels |> list.flatten |> list.length |> should.equal(3)
}
