# Repository Guidelines

## Project Structure & Module Organization
- Top-level dotfiles live at repo root (e.g., `bashrc`, `gitconfig`, `inputrc`, `pythonrc`) and are linked into `~` via Dotbot.
- Installation config is `install.conf.yaml`; it defines symlinks and shell hooks.
- `setup` is the main bootstrap script for WSL/Debian/Ubuntu; `install` runs Dotbot directly.
- Shell helpers and plugins are under `shell/`.
- Emacs configuration lives in `emacs.d/` (includes vendor packages and caches).
- Dotbot itself is vendored as a git submodule in `dotbot/`.

## Build, Test, and Development Commands
- `./setup` runs the default dotfiles phase (uv + Python + symlink install).
- `./setup --all` runs all phases (dotfiles, extras, GCM, meld, Codex).
- `./setup --extras` installs WSLg GUI deps and Emacs GUI.
- `./setup --gcm` configures Windows Git Credential Manager for WSL.
- `./setup --meld` installs Meld and configures git difftool/mergetool.
- `./install` runs Dotbot with `install.conf.yaml` (use for repeated linking).
- `git submodule update --init --recursive` refreshes vendored modules (also run by `setup`).

## Coding Style & Naming Conventions
- Bash scripts use `#!/usr/bin/env bash`, strict modes, and 2-space indentation.
- YAML uses 2-space indentation; keep keys aligned with existing patterns.
- Emacs Lisp follows standard 2-space indentation.
- File naming follows existing dotfile conventions: lowercase, underscores (e.g., `gitignore_global`).
- Prefer ASCII-only edits unless a file already uses Unicode.

## Testing Guidelines
- No repo-level test runner is defined.
- Dotbot includes its own tests under `dotbot/test/`; run them only if modifying the submodule.

## Commit & Pull Request Guidelines
- Commit messages are short, imperative, and capitalized (e.g., “Add codex install”, “Update setup”).
- PRs should include a concise summary, mention affected platforms (WSL/Debian/Ubuntu), and link related issues when applicable.

## Security & Configuration Tips
- `install.conf.yaml` executes shell commands; keep additions minimal and review for side effects.
- `setup` may install packages and modify `~/.bashrc`; note behavior changes in PR descriptions.
