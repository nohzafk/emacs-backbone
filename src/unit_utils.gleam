import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string
import resolver.{type Node, type NodeMap, Cycle, MissingPackage, Node}
import unit.{type ConfigUnit, UnitDep}

fn unit_to_node(unit: ConfigUnit) -> Node(ConfigUnit) {
  case unit.deps {
    None -> Node(name: unit.name, deps: None, value: unit)
    Some(unit_deps) -> {
      // Filter and map only UnitDep dependencies
      let unit_only_deps =
        unit_deps
        |> list.filter_map(fn(dep) {
          case dep {
            UnitDep(unit_config_name) -> Ok(unit_config_name)
            _ -> Error(Nil)
          }
        })

      Node(
        name: unit.name,
        deps: case unit_only_deps {
          [] -> None
          deps -> Some(deps)
        },
        value: unit,
      )
    }
  }
}

fn nodes_to_units(
  nodes_group: List(List(Node(ConfigUnit))),
  node_map: NodeMap(ConfigUnit),
) {
  nodes_group
  |> list.map(fn(nodes) {
    nodes
    |> list.map(fn(node) {
      let assert Ok(node) = node_map |> dict.get(node.name)
      node.value
    })
  })
}

fn validate_unit_after_dependencies(units: List(ConfigUnit)) -> Result(Nil, String) {
  let known_units =
    units
    |> list.map(fn(unit) { unit.name })
    |> set.from_list

  let invalid_units =
    units
    |> list.filter_map(fn(unit) {
      let missing_after =
        unit.deps
        |> option.unwrap([])
        |> list.filter_map(fn(dep) {
          case dep {
            UnitDep(name) -> {
              case set.contains(known_units, name) {
                True -> Error(Nil)
                False -> Ok(name)
              }
            }
            _ -> Error(Nil)
          }
        })
        |> list.sort(string.compare)

      case missing_after {
        [] -> Error(Nil)
        names -> Ok(#(unit.name, names))
      }
    })

  case invalid_units {
    [] -> Ok(Nil)
    entries -> {
      let details =
        entries
        |> list.map(fn(entry) {
          let #(unit_name, missing_after) = entry
          "  - config-unit '" <> unit_name <> "' references missing :after unit(s): "
          <> string.join(missing_after, ", ")
        })
        |> string.join("\n")

      Error(
        "Invalid config-unit dependency graph.\n"
        <> details
        <> "\n`:after` must reference other config-unit! names, not package! names."
        <> "\nUse an existing config unit such as 'minibuffer-settings', or remove the invalid :after entry."
      )
    }
  }
}

pub fn resolve_units(
  packages: List(ConfigUnit),
  enable_debug: Bool,
) -> Result(List(List(ConfigUnit)), String) {
  use _ <- result.try(validate_unit_after_dependencies(packages))

  case
    packages
    |> list.map(unit_to_node)
    |> resolver.resolve_dependencies(enable_debug)
  {
    Ok(#(resolve_units, node_map)) ->
      Ok(resolve_units |> nodes_to_units(node_map))
    Error(Cycle) ->
      Error(
        "Invalid config-unit dependency graph: cycle detected in :after dependencies."
      )
    Error(MissingPackage(_)) ->
      Error(
        "Invalid config-unit dependency graph: unresolved :after dependencies remain."
      )
  }
}
