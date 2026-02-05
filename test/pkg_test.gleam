import gleam/option.{None, Some}
import gleeunit/should
import pkg.{
  Branch, Local, LocalRecipe, Pkg, Remote, Tag, Version,
}

// ============================================================================
// Package Construction Tests
// ============================================================================

pub fn simple_package_test() {
  let p = pkg.simple_pacakge(name: "magit")

  p.name |> should.equal("magit")
  p.recipe |> should.equal(None)
  p.deps |> should.equal(None)
  p.no_compilation |> should.equal(None)
}

pub fn github_package_with_branch_test() {
  let p = pkg.github_package(
    name: "shell-maker",
    repo: "xenodium/shell-maker",
    ref: Branch("main"),
  )

  p.name |> should.equal("shell-maker")
  p.no_compilation |> should.equal(Some(False))

  let assert Some(Remote(recipe)) = p.recipe
  recipe.host |> should.equal("github")
  recipe.repo |> should.equal("xenodium/shell-maker")
  recipe.ref |> should.equal(Branch("main"))
  recipe.wait |> should.equal(Some(False))
}

pub fn github_package_with_tag_test() {
  let p = pkg.github_package(
    name: "vertico",
    repo: "minad/vertico",
    ref: Tag("v1.0.0"),
  )

  let assert Some(Remote(recipe)) = p.recipe
  recipe.ref |> should.equal(Tag("v1.0.0"))
}

pub fn github_package_with_version_test() {
  let p = pkg.github_package(
    name: "consult",
    repo: "minad/consult",
    ref: Version("1.5"),
  )

  let assert Some(Remote(recipe)) = p.recipe
  recipe.ref |> should.equal(Version("1.5"))
}

// ============================================================================
// Package Modifier Tests
// ============================================================================

pub fn deps_on_single_test() {
  let p =
    pkg.simple_pacakge(name: "agent-shell")
    |> pkg.deps_on("shell-maker")

  p.deps |> should.equal(Some(["shell-maker"]))
}

pub fn deps_on_multiple_test() {
  let p =
    pkg.simple_pacakge(name: "lsp-bridge")
    |> pkg.deps_on("yasnippet")
    |> pkg.deps_on("markdown-mode")

  // deps_on prepends, so order is reversed
  let assert Some(deps) = p.deps
  deps |> should.equal(["markdown-mode", "yasnippet"])
}

pub fn files_modifier_test() {
  let p =
    pkg.github_package(
      name: "gleam-ts-mode",
      repo: "gleam-lang/gleam-mode",
      ref: Branch("main"),
    )
    |> pkg.files(["gleam-ts-*.el"])

  let assert Some(Remote(recipe)) = p.recipe
  recipe.files |> should.equal(Some(["gleam-ts-*.el"]))
}

pub fn no_compilation_modifier_test() {
  let p =
    pkg.simple_pacakge(name: "lsp-bridge")
    |> pkg.no_compilation(True)

  p.no_compilation |> should.equal(Some(True))
}

pub fn chained_modifiers_test() {
  let p =
    pkg.github_package(
      name: "lsp-bridge",
      repo: "manateelazycat/lsp-bridge",
      ref: Branch("master"),
    )
    |> pkg.deps_on("yasnippet")
    |> pkg.deps_on("markdown-mode")
    |> pkg.files(["*.el", "*.py", "acm", "core", "langserver"])
    |> pkg.no_compilation(True)

  // Verify all modifications applied
  p.name |> should.equal("lsp-bridge")

  let assert Some(deps) = p.deps
  deps |> should.equal(["markdown-mode", "yasnippet"])

  p.no_compilation |> should.equal(Some(True))

  let assert Some(Remote(recipe)) = p.recipe
  recipe.files
  |> should.equal(Some(["*.el", "*.py", "acm", "core", "langserver"]))
}

// ============================================================================
// Local Package Tests
// ============================================================================

pub fn local_recipe_construction_test() {
  // Test constructing a local recipe directly
  let local_recipe = LocalRecipe(
    path: "/Users/randall/projects/my-package",
    files: Some(["*.el"]),
  )

  local_recipe.path |> should.equal("/Users/randall/projects/my-package")
  local_recipe.files |> should.equal(Some(["*.el"]))
}

pub fn pkg_with_local_recipe_test() {
  let p =
    Pkg(
      name: "consult-snapfile",
      recipe: Some(Local(LocalRecipe(
        path: "/Users/randall/projects/consult-snapfile/emacs",
        files: None,
      ))),
      deps: None,
      no_compilation: None,
    )

  p.name |> should.equal("consult-snapfile")
  let assert Some(Local(recipe)) = p.recipe
  recipe.path |> should.equal("/Users/randall/projects/consult-snapfile/emacs")
}

// ============================================================================
// Ref Type Tests
// ============================================================================

pub fn ref_branch_equality_test() {
  let ref1 = Branch("main")
  let ref2 = Branch("main")
  let ref3 = Branch("master")

  ref1 |> should.equal(ref2)
  ref1 |> should.not_equal(ref3)
}

pub fn ref_tag_equality_test() {
  let ref1 = Tag("v1.0.0")
  let ref2 = Tag("v1.0.0")
  let ref3 = Version("1.0.0")

  ref1 |> should.equal(ref2)
  // Tag and Version are different types even with similar values
  ref1 |> should.not_equal(ref3)
}
