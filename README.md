# Emacs Backbone

An external orchestration program (written in Gleam) that manages Emacs configuration through bidirectional WebSocket communication, providing deterministic, dependency-aware package installation and configuration management inspired by NixOS principles.

## Motivation

After over a decade of using Emacs, I grew frustrated with a persistent problem: **my configuration would break unexpectedly**, and I'd have to debug it before I could actually get work done.

**The root cause: no true dependency resolution**

Traditional Emacs configuration tools don't actually resolve dependencies. `with-eval-after-load` is just a convenience macro that defers code execution until a library loads - it doesn't guarantee execution order across multiple deferred blocks. Similarly, use-package's `:after` keyword delays loading until dependencies are available, but doesn't provide deterministic ordering. There's no built-in way to declare "configure B only after A is fully ready" and have the system guarantee it.

This leads to non-deterministic behavior where the same configuration could behave differently across restarts or machines. Race conditions and timing issues create bugs that are nearly impossible to track down.

**The lazy loading dilemma**

I've been a Doom Emacs user for years with heavily customized config. Doom's lazy loading is excellent for fast startup, but I kept hitting the same frustrating pattern:

1. Make a mistake in my config or custom elisp
2. Error only appears when that feature loads - could be hours into my session
3. Hard to debug because the context is lost

The problem isn't lazy loading itself - it's that **errors hide until runtime**. Traditional approaches optimize for startup speed but defer error discovery.

**The fail-fast philosophy**

That's why Emacs Backbone takes the opposite approach: load everything at startup in deterministic order and encounter all errors immediately. Fix them right away while the context is fresh. Once Emacs starts successfully, you can be confident nothing will break hours into your session. If you comment out a package, its config-unit automatically skips - no manual cleanup needed.

This trades a few extra seconds at startup for reliability. Since Emacs is a long-lived process that runs for days or weeks, I'd rather fail fast at startup than risk hours of lost work from a mid-session crash.

**What Emacs Backbone provides**

Inspired by NixOS principles, Emacs Backbone treats configuration as a dependency graph that's resolved deterministically using topological sorting:

- **True dependency resolution** - Declare dependencies explicitly, the system figures out the order
- **Deterministic execution** - Same declarations always produce the same order
- **Immediate feedback** - All configuration errors surface at startup
- **Reproducible behavior** - Your config works the same way every time, on every machine

Every package and configuration unit declares its dependencies explicitly, and the system ensures they execute in the correct order - every single time.

**Why Gleam?**

I happened to be learning Gleam when this idea crystallized, and it turned out to be the perfect choice. Gleam's strong static type system and compiler give me confidence that **once it compiles, it will work**. The compiler catches potential issues before runtime, which is exactly the kind of reliability I was seeking for my Emacs configuration. This compile-time safety complements the deterministic runtime behavior - together they ensure that configuration problems are caught early and behavior remains consistent.

## Overview

Emacs Backbone separates the configuration framework from user configuration, similar to Doom Emacs:

- **Core System** (`~/.config/emacs`): Gleam backend, Emacs Lisp framework, macros, and orchestration logic
- **User Configuration** (`~/.config/backbone`): Your personal `config.org`, package declarations, and config units

This separation allows you to:
- Version control your user config independently from the core
- Receive core updates without affecting your personal configuration
- Share your configuration with others easily

## Core Concepts

The heart of Emacs Backbone lies in two declarative macros that replace traditional Emacs configuration patterns:

### `package!` - Declarative Package Management

The `package!` macro declares packages with explicit dependencies, eliminating load-order guesswork:

```elisp
;; Simple package declaration
(package! magit)

;; Package from a specific repository
(package! evil
  :repo "emacs-evil/evil"
  :host "github")

;; Package with dependencies - Emacs Backbone ensures correct installation order
(package! evil-collection
  :repo "emacs-evil/evil-collection"
  :deps (evil))

;; Local package with subdirectories
(package! lsp-bridge
  :local "~/projects/lsp-bridge"
  :files ("*.el" "*.py" "acm" "core" "langserver")
  :no-compilation t
  :deps (markdown-mode yasnippet))
```

**Key features:**
- **Dependency resolution**: Use `:deps` to declare package dependencies - the system guarantees they install first
- **Repository flexibility**: Install from GitHub, GitLab, or any git host with `:repo` and `:host`
- **Local packages**: Use `:local` for packages from local filesystem, with `:files` to include subdirectories
- **Version pinning**: Use `:branch`, `:tag`, or `:ref` for reproducible builds
- **No manual ordering needed**: Declare packages anywhere in your config - dependencies are resolved automatically

### `config-unit!` - Dependency-Ordered Configuration

The `config-unit!` macro replaces `use-package`, `with-eval-after-load`, and scattered configuration blocks with explicit, ordered units:

```elisp
;; Basic configuration unit
(config-unit! evil-config
  :requires (evil)  ; Only execute when evil feature is available
  :config
  (evil-mode 1)
  (setq evil-want-C-u-scroll t))

;; Configuration with ordering dependencies
(config-unit! evil-collection-config
  :requires (evil-collection)
  :after (evil-config)  ; Guaranteed to run after evil-config
  :config
  (evil-collection-init))

;; Complex dependency graph - automatically resolved
(config-unit! my-keybindings
  :after (evil-config evil-collection-config which-key-config)
  :config
  (global-set-key (kbd "C-c g") 'magit-status))
```

**Key features:**
- **`:requires`** - Feature availability check (like `featurep`)
- **`:after`** - Execution ordering (unlike `eval-after-load`, this is **guaranteed**)
- **`:config`** - Configuration body that runs in deterministic order
- **Topological sorting** - Complex dependency graphs are automatically resolved
- **Circular dependency detection** - Fails fast if your configuration has cycles

### How They Work Together

1. **Declaration phase**: You declare `package!` and `config-unit!` in any order in your `config.org`
2. **Dependency resolution**: Gleam backend extracts all declarations and builds a dependency graph
3. **Package installation**: Packages install in dependency order (packages with no deps first)
4. **Configuration execution**: After all packages install, config units execute in topologically sorted order
5. **Guaranteed determinism**: Same declarations always produce the same execution order

This replaces the traditional Emacs configuration chaos with a **predictable, reproducible system**.

## Architecture

### Two-Directory Structure

```
~/.config/emacs/                           # Core system (this repository)
├── src/                                   # Gleam backend
│   ├── emacs_backbone.gleam              # Entry point
│   ├── websocket.gleam                    # WebSocket communication
│   ├── resolver.gleam                     # Dependency resolution
│   └── ...
├── lisp/                                  # Core Emacs Lisp framework
│   ├── backbone.el                       # WebSocket client & lifecycle
│   ├── macro-package.el                   # package! macro
│   ├── macro-config-unit.el               # config-unit! macro
│   └── ...
├── init.el                                # Core initialization
├── early-init.el                          # Early initialization
├── gleam.toml                             # Gleam project config
└── package.json                           # Bun/JavaScript dependencies

~/.config/backbone/                 # User configuration (you create this)
├── early-init.el                          # Your early initialization (optional)
├── config.org                             # Your literate configuration
├── config.el                              # Generated from config.org
├── config/                                # Your helper utilities
└── clis/                                  # Your CLI configurations
```

### Key Features

- **Dependency Resolution**: Topological sorting ensures packages and config units load in correct order
- **Literate Configuration**: Write your config in Org mode (`config.org`) with tangling support
- **Declarative Packages**: Use `package!` macro for reproducible package management (Elpaca integration)
- **Config Units**: Use `config-unit!` macro for dependency-ordered configuration blocks
- **Async Communication**: Bidirectional WebSocket protocol between Gleam and Emacs

## Installation

### Prerequisites

- **Emacs** 29+ (with native compilation support recommended)
- **Gleam** (for the backend orchestrator)
- **Bun** (JavaScript runtime for WebSocket FFI)

#### Installing Bun

macOS and Linux:
```bash
curl -fsSL https://bun.sh/install | bash
```

Windows:
```powershell
powershell -c "irm bun.sh/install.ps1 | iex"
```

For more installation options, see [Bun installation docs](https://bun.sh/docs/installation).

### Core Installation

1. **Clone the core repository:**

   ```bash
   git clone git@github.com:nohzafk/emacs-backbone.git ~/.config/emacs
   cd ~/.config/emacs
   ```

2. **Install dependencies:**

   ```bash
   bun install
   ```

3. **Build the Gleam backend:**

   ```bash
   gleam build
   ```

### User Configuration Setup

1. **Create your user configuration directory:**

   ```bash
   mkdir -p ~/.config/backbone
   ```

2. **Create your configuration:**

   Create `~/.config/backbone/config.org` with your Emacs configuration using `package!` and `config-unit!` macros. For example:

   ```org
   * Package Declarations
   #+begin_src emacs-lisp
   (package! evil
     :repo "emacs-evil/evil")

   (package! which-key
     :repo "justbur/emacs-which-key")
   #+end_src

   * Configuration Units
   #+begin_src emacs-lisp
   (config-unit! evil-setup
     :requires (evil)
     :config
     (evil-mode 1))

   (config-unit! which-key-setup
     :requires (which-key)
     :after (evil-setup)
     :config
     (which-key-mode 1))
   #+end_src
   ```

3. **Tangle your configuration:**

   ```bash
   emacs --batch -l org ~/.config/backbone/config.org -f org-babel-tangle
   ```

   This generates `~/.config/backbone/config.el`.

4. **Start Emacs:**

   ```bash
   emacs
   ```

   The core will automatically load your configuration from `~/.config/backbone/`.

### Example Configuration

For a complete, real-world example of an Emacs Backbone configuration, see:

**[nohzafk's literate-darwin config](https://github.com/nohzafk/literate-darwin/blob/main/modules/emacs/backbone/config.org)**


## Configuration

### Customization Variables

#### `emacs-backbone-user-directory`
**Default:** `~/.config/backbone`

Directory containing your user configuration. Customize if you want to use a different location:

```elisp
(setq emacs-backbone-user-directory "~/my-custom-config")
```

#### `emacs-backbone-gleam-executable`
**Default:** Auto-detected (`/opt/homebrew/bin/gleam` or from PATH)

Path to the Gleam executable:

```elisp
(setq emacs-backbone-gleam-executable "/usr/local/bin/gleam")
```

#### `emacs-backbone-enable-debug`
**Default:** `nil`

Enable verbose logging and debugging output:

```elisp
(setq emacs-backbone-enable-debug t)
```

## Usage

### Interactive Commands

- `M-x emacs-backbone-start` - Initialize Emacs Backbone (auto-invoked in init.el)
- `M-x emacs-backbone-exit` - Gracefully shutdown Emacs Backbone
- `M-x emacs-backbone-tangle-config` - Tangle `config.org` to `config.el`
- `M-x emacs-backbone-reload-config` - Full reload: tangle + reinitialize

### Keybindings

- `C-h r e` - Tangle configuration
- `C-h r r` - Reload configuration

## Development

### Building the Project

```bash
gleam build              # Build the Gleam backend
gleam run                # Run (requires backbone_port and emacs_port args)
gleam test               # Run tests
bun install              # Install dependencies
```

### Project Structure

- `src/` - Gleam backend source code
- `lisp/` - Core Emacs Lisp framework
- `test/` - Gleam tests
- `bin/` - Utility scripts

### Testing

```bash
gleam test               # Run Gleam backend tests
```

## Troubleshooting

### Config not found

If you see "Emacs Backbone user config not found", ensure:
1. `~/.config/backbone/config.el` exists
2. You've tangled your `config.org` file
3. The `emacs-backbone-user-directory` variable points to the correct location

### Gleam process won't start

Check:
1. Gleam is installed: `which gleam`
2. Bun is installed: `which bun`
3. The `emacs-backbone-gleam-executable` variable is correct
4. You've run `bun install` in the core directory

### Package installation fails

1. Check the `*emacs-backbone*` buffer for errors
2. Enable debug mode: `(setq emacs-backbone-enable-debug t)`
3. Verify your package declarations syntax in `config.el`
