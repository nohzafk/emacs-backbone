import gleam/option.{type Option, None, Some}

pub type ConfigUnit {
  ConfigUnit(name: String, deps: Option(List(Dependency)), code: Option(String))
}

pub type Dependency {
  // depends on a provided feature
  FeatureDep(feature: String)
  // depends on another unit
  UnitDep(name: String)
  // depends on environment variable
  EnvDep(var_name: String)
  // TODO: depends on external check
  // ExternalDep(check_script: String)
}

pub fn unit(name: String) {
  ConfigUnit(name: name, deps: None, code: None)
}

pub fn deps_on(unit: ConfigUnit, other: String) {
  ConfigUnit(
    ..unit,
    deps: Some([UnitDep(other), ..{ unit.deps |> option.unwrap([]) }]),
  )
}

pub fn deps_on_env(unit: ConfigUnit, env_var: String) {
  ConfigUnit(
    ..unit,
    deps: Some([EnvDep(env_var), ..{ unit.deps |> option.unwrap([]) }]),
  )
}

pub fn deps_on_feature(unit: ConfigUnit, feature: String) {
  ConfigUnit(
    ..unit,
    deps: Some([FeatureDep(feature), ..{ unit.deps |> option.unwrap([]) }]),
  )
}
