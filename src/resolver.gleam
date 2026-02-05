import gleam/bool
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type Node(a) {
  Node(name: String, deps: Option(List(String)), value: a)
}

/// Type alias for the dependency graph: Dict(NodeName, Set(DependencyName))
type DependencyGraph =
  dict.Dict(String, Set(String))

/// Type alias for mapping node names to Node records
pub type NodeMap(a) =
  dict.Dict(String, Node(a))

/// Error type for dependency resolution
pub type ResolveError {
  // Represents a detected cycle
  Cycle
  // Represents a dependency on a non-existent package
  MissingPackage(List(String))
}

/// Recursively collect and add all dependencies to the node map
fn collect_dependencies(node_map: NodeMap(a), node: Node(a)) -> NodeMap(a) {
  case dict.has_key(node_map, node.name) {
    True -> node_map
    False -> dict.insert(node_map, node.name, node)
  }
}

/// Build the node map with all node and their dependencies
fn get_node_map(nodes: List(Node(a))) -> NodeMap(a) {
  list.fold(nodes, dict.new(), fn(acc_map, node) {
    collect_dependencies(acc_map, node)
  })
}

fn debug_nodes_with_deps(nodes: List(Node(a))) -> Nil {
  let nodes_with_deps =
    nodes
    |> list.filter(fn(node) { node.deps |> option.is_some })

  use <- bool.guard(when: list.is_empty(nodes_with_deps), return: Nil)
  nodes_with_deps
  |> list.each(fn(node) {
    let deps = option.unwrap(node.deps, [])
    io.println_error("  " <> node.name <> " depends on: " <> string.join(deps, ", "))
  })
}

/// Builds the initial dependency graph, reverse graph, and package map
fn build_graph(
  nodes: List(Node(a)),
  enable_debug: Bool,
) -> Result(#(DependencyGraph, NodeMap(a)), ResolveError) {
  io.println_error("Building dependency graph...")

  let node_map = get_node_map(nodes)

  case enable_debug {
    False -> Nil
    True -> {
      // Debug the package map to see what's included
      {
        "Node map contains " <> string.inspect(dict.size(node_map)) <> " nodes:"
      }
      |> io.println_error

      dict.keys(node_map)
      |> list.each(fn(node_name) { io.println_error("  " <> node_name) })

      io.println_error("Nodes with dependencies:")
      debug_nodes_with_deps(nodes)
    }
  }

  // Now build the graph
  let graph =
    list.try_fold(dict.keys(node_map), dict.new(), fn(acc, node_name) {
      let curr_graph = acc

      // Get the package
      let assert Ok(node) = dict.get(node_map, node_name)

      // Get dependency names
      let dep_names = node.deps |> option.unwrap([]) |> set.from_list

      // Verify all dependencies exist in the package map
      let missing_deps =
        set.fold(dep_names, [], fn(missing, dep_name) {
          case dict.has_key(node_map, dep_name) {
            True -> missing
            False -> [dep_name, ..missing]
          }
        })
      case missing_deps {
        // Add package and its dependencies to the main graph
        [] -> dict.insert(curr_graph, node_name, dep_names) |> Ok
        _ -> {
          {
            node_name
            <> " has missing dependencies: "
            <> string.join(missing_deps, ", ")
          }
          |> io.println_error

          Error(missing_deps)
        }
      }
    })

  case graph {
    Error(missing_deps) -> MissingPackage(missing_deps) |> Error
    Ok(graph) -> {
      // Make sure all dependency packages are also in the graph as nodes
      // This is important because the algorithm needs each package to be in the graph
      let final_graph =
        dict.fold(node_map, graph, fn(g, name, _) {
          case dict.has_key(g, name) {
            // Already in the graph
            True -> g
            // Add with empty dependencies
            False -> dict.insert(g, name, set.new())
          }
        })

      // final value
      Ok(#(final_graph, node_map))
    }
  }
}

// Private recursive helper for Kahn's algorithm
fn kahn_recursive_step(
  graph_size graph_size: Int,
  graph graph: DependencyGraph,
  current_in_degree current_in_degree: Dict(String, Int),
  current_level_nodes current_level_nodes: List(String),
  sorted_levels sorted_levels: List(List(String)),
  visited_count visited_count: Int,
) -> Result(List(List(String)), ResolveError) {
  // {
  //   "  Processing level "
  //   <> string.inspect(list.length(sorted_levels))
  //   <> " with "
  //   <> string.inspect(list.length(current_level_nodes))
  //   <> " nodes"
  // }
  // |> io.println
  // {
  //   "  Visited "
  //   <> string.inspect(visited_count)
  //   <> " of "
  //   <> string.inspect(graph_size)
  //   <> " nodes"
  // }
  // |> io.println

  case current_level_nodes {
    // Base case: No nodes in the current level to process
    [] -> {
      // Use passed graph_size
      case visited_count == graph_size {
        // Success! Reverse accumulated levels
        True -> Ok(sorted_levels)
        // Not all nodes visited -> Cycle detected
        False -> {
          {
            "  Visited only "
            <> string.inspect(visited_count)
            <> " of "
            <> string.inspect(graph_size)
            <> " nodes, but no more nodes to process! Cycle detected"
          }
          |> io.println_error

          Error(Cycle)
        }
      }
    }
    _ -> {
      // Process current level
      let current_level_size = list.length(current_level_nodes)
      let new_visited_count = visited_count + current_level_size
      let new_sorted_levels = [current_level_nodes, ..sorted_levels]

      // Calculate next level's nodes and updated in-degrees
      let #(next_in_degree, nodes_with_decremented_in_degree) =
        list.fold(
          current_level_nodes,
          #(current_in_degree, set.new()),
          fn(acc, node) {
            let #(degrees, affected_nodes) = acc

            // Get the dependencies of this node
            let assert Ok(dependencies) = dict.get(graph, node)

            // For each dependency, decrement its in-degree
            set.fold(
              dependencies,
              #(degrees, affected_nodes),
              fn(inner_acc, dep) {
                let #(inner_degrees, inner_affected) = inner_acc

                // Decrement the in-degree of the dependency
                let updated_degrees =
                  dict.upsert(inner_degrees, dep, fn(maybe_degree) {
                    let current_degree = option.unwrap(maybe_degree, 0)
                    current_degree - 1
                  })

                // Add this dependency to the set of affected nodes
                let updated_affected = set.insert(inner_affected, dep)

                // Return the updated accumulator
                #(updated_degrees, updated_affected)
              },
            )
          },
        )

      // Find nodes that now have in-degree 0
      let next_level_nodes =
        set.fold(nodes_with_decremented_in_degree, [], fn(nodes, node) {
          // Get the current in-degree of this node
          case dict.get(next_in_degree, node) {
            Ok(0) -> [node, ..nodes]
            Ok(_) -> nodes
            Error(_) -> {
              io.println_error(
                "Warning: Node " <> node <> " not found in in-degree map",
              )
              nodes
            }
          }
        })

      // {
      //   "\n  Next level has "
      //   <> string.inspect(list.length(next_level_nodes))
      //   <> " nodes"
      //   <> "\n"
      //   <> "  -----------------------------"
      // }
      // |> io.println

      kahn_recursive_step(
        graph_size,
        graph,
        next_in_degree,
        // Use the calculated next level nodes
        next_level_nodes,
        new_sorted_levels,
        new_visited_count,
      )
    }
  }
}

/// Performs topological sort using Kahn's algorithm.
/// Returns a list of lists, where each inner list contains package names
/// that can be processed at the same level (concurrently).
fn topological_sort(
  graph: DependencyGraph,
) -> Result(List(List(String)), ResolveError) {
  // Calculate initial in-degrees for all nodes
  let in_degree = {
    // Start with all nodes having in-degree 0
    let all_nodes = dict.keys(graph)
    let initial_in_degrees =
      list.fold(all_nodes, dict.new(), fn(acc, node) {
        dict.insert(acc, node, 0)
      })

    // Now update in-degrees based on dependencies
    // For each node in the graph
    dict.fold(graph, initial_in_degrees, fn(acc_degrees, _node, dependencies) {
      // For each dependency of that node
      set.fold(dependencies, acc_degrees, fn(current_degrees, dep_name) {
        // Increment the in-degree of the dependency
        dict.upsert(current_degrees, dep_name, fn(maybe_count) {
          option.unwrap(maybe_count, 0) + 1
        })
      })
    })
  }

  // Find initial nodes with in-degree 0
  let initial_level_nodes =
    dict.filter(in_degree, fn(_, degree) { degree == 0 })
    |> dict.keys

  // Initial call to the recursive helper function
  kahn_recursive_step(
    graph_size: dict.size(graph),
    graph: graph,
    current_in_degree: in_degree,
    current_level_nodes: initial_level_nodes,
    // Initial empty sorted levels
    sorted_levels: [],
    // Initial visited count
    visited_count: 0,
  )
}

fn debug_sorted_nodes(sorted_nodes: List(List(Node(a)))) -> Nil {
  sorted_nodes
  |> list.index_map(fn(nodes, idx) {
    io.println_error(
      "  " <> string.inspect(idx) <> ": "
      <> { nodes |> list.map(fn(node) { node.name }) |> string.join(" ") },
    )
  })
  Nil
}

pub fn resolve_dependencies(
  nodes: List(Node(a)),
  enable_debug: Bool,
) -> Result(#(List(List(Node(a))), NodeMap(a)), ResolveError) {
  // 1. Build the initial graph, reverse graph, and package map
  use #(graph, node_map) <- result.try(build_graph(nodes, enable_debug))

  // 2. Perform topological sort (returns levels)
  use sorted_levels <- result.try(topological_sort(graph))

  // 3. Map sorted names back to Node records
  let sorted_nodes =
    sorted_levels
    |> list.map(fn(node_names) {
      node_names
      |> list.map(fn(node_name) {
        let assert Ok(node) = dict.get(node_map, node_name)
        node
      })
    })

  // Summary of results
  io.println_error("Dependency resolution complete")
  case enable_debug {
    False -> Nil
    True -> {
      io.println_error(
        "  Total nodes: "
        <> string.inspect(sorted_levels |> list.flatten |> list.length),
      )
      io.println_error(
        "  Number of dependency levels: "
        <> string.inspect(sorted_nodes |> list.length),
      )
      debug_sorted_nodes(sorted_nodes)
    }
  }

  #(sorted_nodes, node_map) |> Ok
}
