import gleam/option.{type Option, None, Some}

pub type Ref {
  Branch(String)
  Tag(String)
  Version(String)
}

pub type Recipe {
  Recipe(
    host: String,
    repo: String,
    ref: Ref,
    files: Option(List(String)),
    wait: Option(Bool),
  )
}

pub type Pkg {
  Pkg(
    name: String,
    recipe: Option(Recipe),
    deps: Option(List(String)),
    no_compilation: Option(Bool),
  )
}

pub fn simple_pacakge(name name) {
  Pkg(name: name, recipe: None, deps: None, no_compilation: None)
}

pub fn github_package(name name, repo repo, ref ref) {
  Pkg(
    name: name,
    recipe: Recipe(
      host: "github",
      repo: repo,
      ref: ref,
      files: None,
      wait: Some(False),
    )
      |> Some,
    deps: None,
    no_compilation: Some(False),
  )
}

pub fn deps_on(pkg: Pkg, dep: String) {
  Pkg(..pkg, deps: [dep, ..pkg.deps |> option.unwrap([])] |> Some)
}

pub fn files(pkg: Pkg, files: List(String)) {
  Pkg(
    ..pkg,
    recipe: pkg.recipe
      |> option.map(fn(recipe) { Recipe(..recipe, files: Some(files)) }),
  )
}

pub fn no_compilation(pkg: Pkg, flag: Bool) {
  Pkg(..pkg, no_compilation: Some(flag))
}
