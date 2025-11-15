import emacs.{type EmacsContext, StringParam}
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import monadic.{type CommandResult, bind, fail_with, pure}
import pkg.{type Pkg, type Recipe, Branch, Pkg, Recipe, Tag, Version}
import pkg_utils
import simplifile

/// Create a decoder for Recipe type
fn recipe_decoder() -> decode.Decoder(Recipe) {
  use host <- decode.field("host", decode.string)
  use repo <- decode.field("repo", decode.string)
  use branch <- decode.optional_field(
    "branch",
    None,
    decode.optional(decode.string),
  )
  use tag <- decode.optional_field("tag", None, decode.optional(decode.string))
  use version <- decode.optional_field(
    "ref",
    None,
    decode.optional(decode.string),
  )
  use files <- decode.optional_field(
    "files",
    None,
    decode.optional(decode.list(decode.string)),
  )

  use wait <- decode.optional_field(
    "wait",
    None,
    decode.bool |> decode.map(Some),
  )

  let ref = case branch, tag, version {
    Some(v), _, _ -> Branch(v)
    _, Some(v), _ -> Tag(v)
    _, _, Some(v) -> Version(v)
    _, _, _ -> Branch("main")
  }

  Recipe(host: host, repo: repo, ref: ref, files: files, wait: wait)
  |> decode.success
}

/// Create a decoder for Pkg type
pub fn pkg_decoder() -> decode.Decoder(Pkg) {
  use name <- decode.field("name", decode.string)
  use recipe <- decode.optional_field(
    "recipe",
    None,
    decode.optional(recipe_decoder()),
  )
  use no_comp <- decode.optional_field(
    "no-compilation",
    None,
    decode.optional(decode.bool),
  )
  use deps <- decode.optional_field(
    "deps",
    None,
    decode.optional(decode.list(decode.string)),
  )

  Pkg(name: name, recipe: recipe, deps: deps, no_compilation: no_comp)
  |> decode.success
}

/// Fetch packages defined in Emacs using the package! macro
pub fn fetch_packages(ctx: EmacsContext) -> CommandResult(List(Pkg)) {
  // Get the packages from Emacs
  use js_data <- bind(ctx.eval("emacs-backbone-get-packages", StringParam([])))

  // Try to parse the JavaScript data
  let decode_result =
    json.parse(from: js_data, using: decode.list(of: pkg_decoder()))

  case decode_result {
    Ok(package_list) -> pure(package_list)
    Error(errs) -> {
      let error_msg =
        "Failed to decode list of packages: " <> string.inspect(errs)
      io.println_error("[ERROR] " <> error_msg)
      io.println_error("[DEBUG] Raw data: " <> js_data)
      fail_with(error_msg)
    }
  }
}

/// Generate packages.el from macro-defined packages
/// make sure that package-macro.el is loaded
pub fn generate_macro_defined_packages(
  ctx: EmacsContext,
  packages_el_path: String,
) -> CommandResult(List(String)) {
  // Fetch packages defined via the package! macro
  use pkgs <- bind(fetch_packages(ctx))

  // Generate packages.el from the fetched packages
  case pkg_utils.genearte_packages(pkgs, packages_el_path, ctx.enable_debug) {
    Ok(_) -> {
      // Return the list of package names
      let package_names = pkgs |> list.map(fn(pkg) { pkg.name })
      monadic.pure(package_names)
    }
    Error(err) -> {
      let error_msg =
        "Failed to generate "
        <> packages_el_path
        <> " "
        <> simplifile.describe_error(err)
      io.println_error(error_msg)
      fail_with(error_msg)
    }
  }
}
