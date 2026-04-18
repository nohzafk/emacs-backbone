import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/javascript/array.{type Array}
import gleam/list
import gleam/option
import pkg.{type Pkg}

// Type for package tracker status
pub type PackageTracker {
  PackageTracker(
    all_installed: Bool,
    pending: List(String),
    installed: List(String),
    total: Int,
  )
}

// External functions for package tracking
@external(javascript, "./package_tracker_ffi.mjs", "initializePackageTracker")
fn initialize_package_tracker_js(
  packages: Array(String),
  deps: Array(#(String, Array(String))),
) -> Bool

@external(javascript, "./package_tracker_ffi.mjs", "updatePackageTracker")
fn update_package_tracker_js(package_name: String) -> Dynamic

@external(javascript, "./package_tracker_ffi.mjs", "getPackageTrackerStatus")
fn get_package_tracker_js() -> Dynamic

@external(javascript, "./package_tracker_ffi.mjs", "getFailedPackages")
fn get_failed_packages_js() -> Array(String)

pub fn initialize(packages: List(Pkg)) -> Bool {
  let names = packages |> list.map(fn(p) { p.name }) |> array.from_list
  let deps_entries =
    packages
    |> list.map(fn(p) {
      let deps = p.deps |> option.unwrap([]) |> array.from_list
      #(p.name, deps)
    })
    |> array.from_list
  initialize_package_tracker_js(names, deps_entries)
}

pub fn update(package_name: String) -> PackageTracker {
  let result = update_package_tracker_js(package_name)
  parse_tracker_status(result)
}

pub fn get_package_tracker() -> PackageTracker {
  let result = get_package_tracker_js()
  parse_tracker_status(result)
}

/// Returns packages that never reported `package_installed`, plus any
/// packages that transitively depend on them via `:deps`. Meaningful only
/// after `packages_finished`.
pub fn get_failed_packages() -> List(String) {
  get_failed_packages_js() |> array.to_list
}

// Helper to parse the JavaScript tracker status
fn parse_tracker_status(value: Dynamic) -> PackageTracker {
  let tracker_decoder = {
    use all_installed <- decode.field("allInstalled", decode.bool)
    use pending <- decode.field("pending", decode.list(decode.string))
    use installed <- decode.field("installed", decode.list(decode.string))
    use total <- decode.field("total", decode.int)
    decode.success(PackageTracker(
      all_installed: all_installed,
      pending: pending,
      installed: installed,
      total: total,
    ))
  }

  case decode.run(value, tracker_decoder) {
    Ok(tracker) -> tracker
    Error(_) ->
      PackageTracker(all_installed: False, pending: [], installed: [], total: 0)
  }
}
