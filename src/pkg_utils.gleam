import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import pkg.{type Pkg, type Recipe, Branch, Local, Remote, Tag, Version}
import resolver.{type Node, type NodeMap, Node}
import simplifile

pub fn genearte_packages(
  pacakges: List(Pkg),
  path: String,
  enable_debug: Bool,
) -> Result(Nil, simplifile.FileError) {
  let assert Ok(#(resolved_pkgs, node_map)) =
    pacakges
    |> list.map(pkg_to_node)
    |> resolver.resolve_dependencies(enable_debug)

  resolved_pkgs
  |> nodes_to_pacakges(node_map)
  |> generate_packages_el_with_notifications(path)
}

fn generate_packages_el_with_notifications(
  packages: List(List(Pkg)),
  path: String,
) -> Result(Nil, simplifile.FileError) {
  // Helper function to convert a single Pkg to its Elisp representation
  let pkg_to_elisp = fn(pkg: Pkg) -> String {
    let recipe_str = case pkg.recipe {
      None -> ""
      Some(recipe) -> recipe_to_elisp(recipe, pkg)
    }

    let straight_config = case recipe_str |> string.trim {
      "" -> "\n  :ensure t"
      _ as parts -> "\n  :ensure (" <> parts <> ")"
    }

    // TODO: Add support for other Pkg fields like :disable, :pin, etc. later

    "(use-package " <> pkg.name <> straight_config <> "
  :config
  ;; Send notification when package is installed
  (emacs-backbone--call \"package_installed\" \"" <> pkg.name <> "\")" <> ")"
  }

  let packages_definition =
    packages
    |> list.map(fn(pkgs) {
      pkgs |> list.map(pkg_to_elisp) |> string.join("\n\n")
    })
    |> string.join(
      "

(elpaca-process-queues)
;; ========

",
    )

  { packages_definition <> "

(elpaca-process-queues)

" }
  |> simplifile.write(to: path)
}

/// Convert a Recipe to its Elisp representation for Elpaca
fn recipe_to_elisp(recipe: Recipe, pkg: Pkg) -> String {
  case recipe {
    Local(path) -> {
      // For local packages, use :repo with local path (no :host)
      // Per elpaca docs: ":repo is a local file path or remote URL when :host is not used"
      let no_compilation_el = case pkg.no_compilation {
        Some(True) -> " :build (:not elpaca--byte-compile)"
        _ -> ""
      }
      ":repo \"" <> path <> "\"" <> no_compilation_el
    }
    Remote(r) -> {
      let host_el = ":host " <> r.host
      let repo_el = ":repo \"" <> r.repo <> "\""
      let ref_el = case r.ref {
        Branch(branch) -> ":branch \"" <> branch <> "\""
        Version(version) -> ":ref \"" <> version <> "\""
        Tag(tag) -> ":tag \"" <> tag <> "\""
      }
      let files_el = case r.files {
        None -> ""
        Some(files) ->
          "\n  :files ("
          <> {
            files
            |> list.map(fn(file) { "\"" <> file <> "\"" })
            |> string.join(" ")
          }
          <> ")"
      }
      let no_compilation_el = case pkg.no_compilation {
        Some(True) -> "\n  :build (:not elpaca--byte-compile)"
        _ -> ""
      }
      { [host_el, repo_el, ref_el] |> string.join(" ") }
      <> files_el
      <> no_compilation_el
    }
  }
}

fn pkg_to_node(pkg: Pkg) -> Node(Pkg) {
  case pkg.deps {
    None -> Node(name: pkg.name, deps: None, value: pkg)
    Some(pkg_deps) -> {
      Node(name: pkg.name, deps: pkg_deps |> Some, value: pkg)
    }
  }
}

fn nodes_to_pacakges(nodes_group: List(List(Node(Pkg))), node_map: NodeMap(Pkg)) {
  nodes_group
  |> list.map(fn(nodes) {
    nodes
    |> list.map(fn(node) {
      let assert Ok(node) = node_map |> dict.get(node.name)
      node.value
    })
  })
}
