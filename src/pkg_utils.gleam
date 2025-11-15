import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import pkg.{type Pkg, Branch, Tag, Version}
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
    let recipe = case pkg.recipe {
      None -> ""
      Some(recipe) -> {
        let host_el = ":host " <> recipe.host
        let repo_el = ":repo \"" <> recipe.repo <> "\""
        let ref_el = case recipe.ref {
          Branch(branch) -> ":branch \"" <> branch <> "\""
          Version(version) -> ":ref \"" <> version <> "\""
          Tag(tag) -> ":tag \"" <> tag <> "\""
        }
        let files_el = case recipe.files {
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
        // Use the fields from the bound 'recipe'
        { [host_el, repo_el, ref_el] |> string.join(" ") }
        <> files_el
        <> no_compilation_el
      }
    }

    let straight_config = case recipe |> string.trim {
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
