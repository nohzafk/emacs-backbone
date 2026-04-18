import gleam/json
import gleam/option.{None, Some}
import gleeunit/should
import unit.{type ConfigUnit, EnvDep, ExecutableDep, FeatureDep, UnitDep}
import unit_macro

fn decode_unit(json_string: String) -> ConfigUnit {
  let assert Ok(value) =
    json.parse(from: json_string, using: unit_macro.unit_decoder())
  value
}

pub fn unit_decoder_bare_unit_test() {
  let result = decode_unit("{\"name\":\"u\",\"body\":\"(progn t)\"}")

  result.name |> should.equal("u")
  result.deps |> should.equal(None)
  result.code |> should.equal(Some("(progn t)"))
}

pub fn unit_decoder_all_dep_kinds_test() {
  let payload =
    "{\"name\":\"u\","
    <> "\"requires\":[\"avy\"],"
    <> "\"after\":[\"bootstrap\"],"
    <> "\"env\":[\"API_KEY\"],"
    <> "\"executable\":[\"git\"],"
    <> "\"body\":\"(progn t)\"}"

  let result = decode_unit(payload)

  result.name |> should.equal("u")
  result.code |> should.equal(Some("(progn t)"))
  // Order matches the flatten in unit_macro: features, after, env, executable
  result.deps
  |> should.equal(
    Some([
      FeatureDep("avy"),
      UnitDep("bootstrap"),
      EnvDep("API_KEY"),
      ExecutableDep("git"),
    ]),
  )
}

pub fn unit_decoder_env_only_test() {
  let result =
    decode_unit("{\"name\":\"u\",\"env\":[\"FOO\",\"BAR\"]}")

  result.deps |> should.equal(Some([EnvDep("FOO"), EnvDep("BAR")]))
}

pub fn unit_decoder_executable_only_test() {
  let result =
    decode_unit(
      "{\"name\":\"u\",\"executable\":[\"rg\",\"fd\"]}",
    )

  result.deps
  |> should.equal(Some([ExecutableDep("rg"), ExecutableDep("fd")]))
}

pub fn unit_decoder_rejects_missing_name_test() {
  let result = json.parse(from: "{}", using: unit_macro.unit_decoder())

  let assert Error(_) = result
}
