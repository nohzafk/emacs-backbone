import emacs.{type EmacsContext, StringParam}
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import monadic.{type CommandResult, bind, fail_with, pure}
import unit.{type ConfigUnit, ConfigUnit, FeatureDep, UnitDep}

/// Create a decoder for ConfigUnit type
pub fn unit_decoder() -> decode.Decoder(ConfigUnit) {
  use name <- decode.field("name", decode.string)
  use features <- decode.optional_field(
    "requires",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use after <- decode.optional_field(
    "after",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use code <- decode.optional_field(
    "body",
    None,
    decode.optional(decode.string),
  )

  let feature_deps = features |> option.unwrap([]) |> list.map(FeatureDep)
  let unit_deps = after |> option.unwrap([]) |> list.map(UnitDep)

  let deps = case [feature_deps, unit_deps] |> list.flatten {
    [] -> None
    _ as value -> Some(value)
  }

  ConfigUnit(name: name, deps: deps, code: code) |> decode.success
}

/// Fetch config units defined in Emacs using the config-unit! macro
pub fn fetch_units(ctx: EmacsContext) -> CommandResult(List(ConfigUnit)) {
  // Get the packages from Emacs
  use js_data <- bind(ctx.eval("emacs-backbone-get-units", StringParam([])))

  // Try to parse the JavaScript data
  let decode_result =
    json.parse(
      from: js_data,
      using: decode.optional(decode.list(of: unit_decoder())),
    )

  case decode_result {
    Ok(package_list) -> pure(package_list |> option.unwrap([]))
    Error(errs) -> {
      let error_msg =
        "Failed to decode list of packages: " <> string.inspect(errs)
      io.println_error("[ERROR] " <> error_msg)
      io.println_error("[DEBUG] Raw data: " <> js_data)
      fail_with(error_msg)
    }
  }
}
