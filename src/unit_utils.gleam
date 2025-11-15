import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import resolver.{type Node, type NodeMap, Node}
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

pub fn resolve_units(packages: List(ConfigUnit), enable_debug: Bool) {
  let assert Ok(#(resolve_units, node_map)) =
    packages
    |> list.map(unit_to_node)
    |> resolver.resolve_dependencies(enable_debug)

  resolve_units |> nodes_to_units(node_map)
}
