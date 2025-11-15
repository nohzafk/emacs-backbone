import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/javascript/array.{type Array}

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
pub fn initialize_package_tracke_js(packages: Array(String)) -> Bool

@external(javascript, "./package_tracker_ffi.mjs", "updatePackageTracker")
fn update_package_tracker_js(package_name: String) -> Dynamic

@external(javascript, "./package_tracker_ffi.mjs", "getPackageTrackerStatus")
fn get_package_tracker_js() -> Dynamic

// Wrapper function with proper return type
pub fn initialize(packages: List(String)) {
  packages |> array.from_list |> initialize_package_tracke_js
}

pub fn update(package_name: String) -> PackageTracker {
  let result = update_package_tracker_js(package_name)
  parse_tracker_status(result)
}

// Wrapper function with proper return type
pub fn get_package_tracker() -> PackageTracker {
  let result = get_package_tracker_js()
  parse_tracker_status(result)
}

// Helper to parse the JavaScript tracker status
fn parse_tracker_status(value: Dynamic) -> PackageTracker {
  // Create a decoder for the PackageTrackerStatus
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

  // Run the decoder and handle any errors
  case decode.run(value, tracker_decoder) {
    Ok(tracker) -> tracker
    Error(_) ->
      PackageTracker(all_installed: False, pending: [], installed: [], total: 0)
  }
}
