# Emacs Backbone

External orchestration program (Gleam) that manages Emacs configuration through JSON-RPC over stdio, providing deterministic, dependency-aware package installation inspired by NixOS principles.

## Quick Start

```bash
gleam build              # Build Gleam backend
gleam test               # Run tests
```

## Project Structure

```
src/                       # Gleam backend
  emacs_backbone.gleam     # Entry point
  jsonrpc.gleam            # JSON-RPC communication (stdio)
  resolver.gleam           # Dependency resolution (topological sorting)
  pkg.gleam                # Package types and core logic
  pkg_macro.gleam          # Package macro extraction
  pkg_utils.gleam          # Package utilities
  unit.gleam               # Config unit types
  unit_macro.gleam         # Config unit macro extraction
  unit_executor.gleam      # Config unit execution
  unit_state.gleam         # Config unit state management
  unit_utils.gleam         # Config unit utilities
  emacs.gleam              # Emacs interaction helpers
  monadic.gleam            # Monadic utilities
  package_tracker.gleam    # Package installation tracking
lisp/                      # Core Emacs Lisp framework
  backbone.el              # JSON-RPC client & lifecycle
  backbone-early-init.el   # Early init helpers
  macro-package.el         # package! macro
  macro-config-unit.el     # config-unit! macro
  compose.el               # Main composition logic
  package-manager.el       # Elpaca integration
  helper.el                # Shared utilities
init.el                    # Loads compose.el and user config
early-init.el              # Early initialization
bin/
  backbone-env             # Shell script to export env vars for Emacs
test/                      # Tests
  *_test.gleam             # Gleam unit tests
  e2e/                     # End-to-end tests (Emacs Lisp)
```

## Key Concepts

**Two declarative macros** replace traditional Emacs configuration:

- `package!` - Declare packages with explicit dependencies (`:deps`, `:repo`, `:host`)
- `config-unit!` - Ordered configuration blocks (`:requires` for features, `:after` for execution order)

**Dependency graph resolution**: Gleam backend extracts declarations, builds a dependency graph, then executes in topologically sorted order.

**Two-directory design**:
- Core system: `~/.config/emacs` (this repo)
- User config: `~/.config/backbone/config.org` (literate config, tangled to config.el)

## Development Workflow

1. Edit Gleam source in `src/`
2. Edit Emacs Lisp in `lisp/`
3. Build with `gleam build`
4. Test with `gleam test`
5. User configs go in `~/.config/backbone/`

## Interactive Commands (Emacs)

- `M-x emacs-backbone-start` - Initialize backbone
- `M-x emacs-backbone-tangle-config` - Tangle config.org to config.el
- `M-x emacs-backbone-reload-config` - Full reload

## Tech Stack

- **Gleam** (JavaScript target via Bun) - Backend orchestrator
- **Emacs Lisp** - Macros and client
- **JSON-RPC 2.0** - Bidirectional communication over stdio
- **Elpaca** - Package manager integration

