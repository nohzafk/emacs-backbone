import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import resolver.{type Node, Cycle, MissingPackage, Node}

// Helper to create a simple node
fn node(name: String) -> Node(String) {
  Node(name: name, deps: None, value: name)
}

// Helper to create a node with dependencies
fn node_with_deps(name: String, deps: List(String)) -> Node(String) {
  Node(name: name, deps: Some(deps), value: name)
}

// Helper to extract names from resolved levels
fn level_names(levels: List(List(Node(String)))) -> List(List(String)) {
  levels
  |> list.map(fn(level) { level |> list.map(fn(n) { n.name }) |> list.sort(string.compare) })
}

import gleam/string

// ============================================================================
// Basic Resolution Tests
// ============================================================================

pub fn resolve_single_node_test() {
  let nodes = [node("a")]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result
  levels |> list.flatten |> list.length |> should.equal(1)
}

pub fn resolve_independent_nodes_test() {
  // Three nodes with no dependencies - all should be at same level
  let nodes = [node("a"), node("b"), node("c")]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // All 3 nodes present
  levels |> list.flatten |> list.length |> should.equal(3)
  // Single level (all independent)
  levels |> list.length |> should.equal(1)
}

pub fn resolve_linear_dependency_chain_test() {
  // c depends on b, b depends on a
  // Expected order: a -> b -> c (3 levels)
  let nodes = [
    node("a"),
    node_with_deps("b", ["a"]),
    node_with_deps("c", ["b"]),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // Should have 3 levels
  levels |> list.length |> should.equal(3)

  // Verify order: a first, then b, then c
  let names = level_names(levels)
  names |> should.equal([["a"], ["b"], ["c"]])
}

pub fn resolve_diamond_dependency_test() {
  // Diamond: d depends on b and c, both b and c depend on a
  //     a
  //    / \
  //   b   c
  //    \ /
  //     d
  let nodes = [
    node("a"),
    node_with_deps("b", ["a"]),
    node_with_deps("c", ["a"]),
    node_with_deps("d", ["b", "c"]),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // Should have 3 levels: [a], [b, c], [d]
  levels |> list.length |> should.equal(3)

  let names = level_names(levels)
  // Level 0: a
  names |> list.first |> should.equal(Ok(["a"]))
  // Level 1: b and c (in any order)
  let assert [_, level1, _] = names
  level1 |> list.sort(string.compare) |> should.equal(["b", "c"])
  // Level 2: d
  names |> list.last |> should.equal(Ok(["d"]))
}

pub fn resolve_multiple_roots_test() {
  // Two independent trees
  //   a      d
  //   |      |
  //   b      e
  //   |
  //   c
  let nodes = [
    node("a"),
    node_with_deps("b", ["a"]),
    node_with_deps("c", ["b"]),
    node("d"),
    node_with_deps("e", ["d"]),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // All 5 nodes should be present
  levels |> list.flatten |> list.length |> should.equal(5)
}

// ============================================================================
// Error Handling Tests
// ============================================================================

pub fn detect_simple_cycle_test() {
  // a depends on b, b depends on a
  let nodes = [node_with_deps("a", ["b"]), node_with_deps("b", ["a"])]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(Cycle)
}

pub fn detect_three_node_cycle_test() {
  // a -> b -> c -> a
  let nodes = [
    node_with_deps("a", ["b"]),
    node_with_deps("b", ["c"]),
    node_with_deps("c", ["a"]),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_error
  let assert Error(err) = result
  err |> should.equal(Cycle)
}

pub fn detect_missing_dependency_test() {
  // a depends on non-existent "missing"
  let nodes = [node_with_deps("a", ["missing"])]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_error
  let assert Error(MissingPackage(missing)) = result
  missing |> should.equal(["missing"])
}

pub fn detect_multiple_missing_dependencies_test() {
  // a depends on two non-existent packages
  let nodes = [node_with_deps("a", ["missing1", "missing2"])]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_error
  let assert Error(MissingPackage(missing)) = result
  missing |> list.length |> should.equal(2)
}

// ============================================================================
// Complex Scenarios
// ============================================================================

pub fn resolve_complex_graph_test() {
  // More complex dependency graph:
  //   a   b
  //   |\ /|
  //   | X |
  //   |/ \|
  //   c   d
  //    \ /
  //     e
  let nodes = [
    node("a"),
    node("b"),
    node_with_deps("c", ["a", "b"]),
    node_with_deps("d", ["a", "b"]),
    node_with_deps("e", ["c", "d"]),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // Should have 3 levels
  levels |> list.length |> should.equal(3)

  // Level 0: a and b (roots)
  let names = level_names(levels)
  let assert Ok(level0) = list.first(names)
  level0 |> list.sort(string.compare) |> should.equal(["a", "b"])

  // Level 1: c and d
  let assert [_, level1, _] = names
  level1 |> list.sort(string.compare) |> should.equal(["c", "d"])

  // Level 2: e
  names |> list.last |> should.equal(Ok(["e"]))
}

pub fn resolve_preserves_node_values_test() {
  // Ensure the resolver preserves node values through resolution
  let nodes = [
    Node(name: "a", deps: None, value: "value_a"),
    Node(name: "b", deps: Some(["a"]), value: "value_b"),
  ]

  let result = resolver.resolve_dependencies(nodes, False)

  result |> should.be_ok
  let assert Ok(#(levels, _node_map)) = result

  // Find node "a" and verify its value
  let all_nodes = list.flatten(levels)
  let assert Ok(node_a) = list.find(all_nodes, fn(n) { n.name == "a" })
  node_a.value |> should.equal("value_a")

  let assert Ok(node_b) = list.find(all_nodes, fn(n) { n.name == "b" })
  node_b.value |> should.equal("value_b")
}
