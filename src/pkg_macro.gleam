import emacs.{type EmacsContext, StringParam, message}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import monadic.{type CommandResult, bind, fail_with, pure}
import pkg.{
  type Pkg, type Recipe, Branch, Local, LocalRecipe, Pkg, Remote, RemoteRecipe,
  Tag, Version,
}
import pkg_utils
import simplifile

/// Create a decoder for local recipe (path and optional files)
fn local_recipe_decoder() -> decode.Decoder(Recipe) {
  use local_path <- decode.field("local", decode.string)
  use files <- decode.optional_field(
    "files",
    None,
    decode.optional(decode.list(decode.string)),
  )
  Local(LocalRecipe(path: local_path, files: files)) |> decode.success
}

/// Create a decoder for remote Recipe type
fn remote_recipe_decoder() -> decode.Decoder(Recipe) {
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

  // Require explicit branch/tag/ref for remote repos - no default
  case branch, tag, version {
    Some(v), _, _ -> {
      Remote(RemoteRecipe(
        host: host,
        repo: repo,
        ref: Branch(v),
        files: files,
        wait: wait,
      ))
      |> decode.success
    }
    _, Some(v), _ -> {
      Remote(RemoteRecipe(
        host: host,
        repo: repo,
        ref: Tag(v),
        files: files,
        wait: wait,
      ))
      |> decode.success
    }
    _, _, Some(v) -> {
      Remote(RemoteRecipe(
        host: host,
        repo: repo,
        ref: Version(v),
        files: files,
        wait: wait,
      ))
      |> decode.success
    }
    _, _, _ -> {
      // Fail explicitly when no branch/tag/ref is provided
      decode.failure(Remote(RemoteRecipe(
        host: host,
        repo: repo,
        ref: Branch(""),
        files: files,
        wait: wait,
      )), "Remote recipe for repo '" <> repo <> "' must specify one of: :branch, :tag, or :ref")
    }
  }
}

/// Create a decoder for Recipe type (tries local first, then remote)
fn recipe_decoder() -> decode.Decoder(Recipe) {
  decode.one_of(local_recipe_decoder(), [remote_recipe_decoder()])
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

/// Validate that remote packages have explicit branch/tag/ref
/// Returns Result - Ok(Nil) if valid, Error with message if invalid
fn validate_remote_packages(
  packages: List(Pkg),
  ctx: EmacsContext,
) -> CommandResult(Nil) {
  let invalid =
    packages
    |> list.filter_map(fn(pkg) {
      case pkg.recipe {
        Some(Remote(RemoteRecipe(repo: repo, ref: Branch(""), ..))) ->
          Ok(#(pkg.name, repo))
        _ -> Error(Nil)
      }
    })

  case invalid {
    [] -> pure(Nil)
    errors -> {
      let error_details =
        errors
        |> list.map(fn(pair) {
          let #(name, repo) = pair
          "  - " <> name <> " (repo: " <> repo <> ")"
        })
        |> string.join("\n")
      let error_msg =
        "Remote packages must specify :branch, :tag, or :ref:\n"
        <> error_details

      io.println_error("[ERROR] " <> error_msg)
      message(error_msg, ctx.pws)

      fail_with(
        "Cannot proceed: remote packages missing branch/tag/ref: "
        <> { errors |> list.map(fn(p) { p.0 }) |> string.join(", ") },
      )
    }
  }
}

/// Validate that local package paths exist
/// Returns Result - Ok(Nil) if valid, Error with message if invalid
fn validate_local_packages(
  packages: List(Pkg),
  ctx: EmacsContext,
) -> CommandResult(Nil) {
  let invalid =
    packages
    |> list.filter_map(fn(pkg) {
      case pkg.recipe {
        Some(Local(LocalRecipe(path: path, ..))) -> {
          case ctx.enable_debug {
            True -> io.println("[DEBUG] Validating local package '" <> pkg.name <> "' at: " <> path)
            False -> Nil
          }

          case simplifile.is_directory(path) {
            Ok(True) -> {
              case ctx.enable_debug {
                True -> io.println("[DEBUG] Local package '" <> pkg.name <> "' path exists: " <> path)
                False -> Nil
              }
              Error(Nil)
            }
            _ -> {
              io.println_error("[ERROR] Local package '" <> pkg.name <> "' path does not exist: " <> path)
              Ok(#(pkg.name, path))
            }
          }
        }
        _ -> Error(Nil)
      }
    })

  case invalid {
    [] -> pure(Nil)
    errors -> {
      let error_details =
        errors
        |> list.map(fn(pair) {
          let #(name, path) = pair
          "  - " <> name <> ": " <> path
        })
        |> string.join("\n")
      let error_msg = "Local package paths do not exist:\n" <> error_details

      io.println_error("[ERROR] " <> error_msg)
      message(error_msg, ctx.pws)

      fail_with(
        "Cannot proceed: local package paths do not exist: "
        <> { errors |> list.map(fn(p) { p.0 }) |> string.join(", ") },
      )
    }
  }
}

/// Extract package name from decode error for better error messages
fn get_decode_error_hint(js_data: String, errs: json.DecodeError) -> String {
  case extract_package_name_from_error(js_data, errs) {
    Ok(name) ->
      "\n\n[ERROR] Package '" <> name <> "' has invalid recipe." <>
      "\n        Remote packages (:repo) must specify :branch, :tag, or :ref"
    Error(_) -> ""
  }
}

/// Try to extract package name from error path
fn extract_package_name_from_error(
  js_data: String,
  errs: json.DecodeError,
) -> Result(String, Nil) {
  use idx_str <- result.try(extract_index_from_error(errs))
  use idx <- result.try(int.parse(idx_str) |> result.replace_error(Nil))
  use pkgs <- result.try(
    json.parse(js_data, decode.list(decode.dynamic))
    |> result.replace_error(Nil),
  )
  use pkg <- result.try(list.drop(pkgs, idx) |> list.first)

  // Decode the name field from the dynamic package
  let name_decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }
  decode.run(pkg, name_decoder) |> result.replace_error(Nil)
}

/// Extract package index from error string
fn extract_index_from_error(errs: json.DecodeError) -> Result(String, Nil) {
  let err_str = string.inspect(errs)

  // Extract index from path like: path: ["3", "local"]
  case string.split(err_str, "path: [") {
    [_, rest] if rest != "" -> {
      case string.split(rest, "\"") {
        [_, idx_str, ..] -> {
          let trimmed = string.trim(idx_str)
          case trimmed {
            "" -> Error(Nil)
            idx -> Ok(idx)
          }
        }
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
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
      let hint = get_decode_error_hint(js_data, errs)
      let error_msg = "Failed to decode list of packages: " <> string.inspect(errs) <> hint

      io.println_error("[ERROR] " <> error_msg)
      case ctx.enable_debug {
        True -> io.println_error("[DEBUG] Raw data: " <> js_data)
        False -> Nil
      }
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

  // Validate local package paths
  use _ <- bind(validate_local_packages(pkgs, ctx))

  // Validate remote packages have explicit branch/tag/ref
  use _ <- bind(validate_remote_packages(pkgs, ctx))

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
