# Contributing to goFlux

Thank you for your interest in contributing to goFlux! This document outlines guidelines for contributing, particularly regarding commit messages which help automate our release process.

## Commit Message Format

Starting now, we follow **conventional commits** to enable automatic versioning and changelog generation via release-please. This helps maintain a clear release history and makes it easy to understand what changed between versions.

> **Note**: This is a new standard. Historical goFlux commits used informal messages (e.g., "new function X", "fix issue Y"). Going forward, please use the format below.

### Format

```
<type>: <subject>
```

### Commit Types

- **`feat:`** — New functions, import support for instruments, or new features
  - Example: `feat: add import function for GasmetPD`
  - Example: `feat: add auto.deadband function`
  - → Bumps **minor** version (e.g., 0.2.0 → 0.3.0)

- **`fix:`** — Bug fixes in calculations, functions, or data import
  - Example: `fix: resolve flux calculation error in HM model`
  - Example: `fix: correct chamber volume calculation`
  - → Bumps **patch** version (e.g., 0.2.0 → 0.2.1)

- **`docs:`** — Documentation, roxygen comments, README, vignettes
  - Example: `docs: update installation instructions`
  - Example: `docs: clarify flux calculation methodology in roxygen`
  - → No version bump

- **`refactor:`** — Code restructuring or cleanups (no functional changes)
  - Example: `refactor: simplify import workflow`
  - Example: `refactor: extract chamber identification logic`
  - → No version bump

- **`perf:`** — Performance improvements or optimizations
  - Example: `perf: optimize flux calculation loop`
  - → No version bump

- **`test:`** — Test additions or updates
  - Example: `test: add unit tests for autoID function`
  - → No version bump

- **`chore:`** — Maintenance, dependencies, CI/CD, build scripts
  - Example: `chore: update R dependencies`
  - Example: `chore: add gitignore entries`
  - → No version bump

### Rules

1. **Start with the type** in lowercase: `feat:`, `fix:`, `docs:`, etc. (not `Feat:` or `FEAT:`)
2. **Keep the subject line concise** — aim for under 50 characters
3. **Use imperative mood** — write "add feature" not "adds feature" or "added feature"
4. **Don't end with a period**
5. **Reference issues when applicable** — e.g., `fix: resolve flux calculation error (closes #123)`
6. **One commit = one type** — if your work spans multiple types, split into multiple commits (e.g., first `feat: ...`, then `docs: ...`)

### Examples

✅ **Good** (mirrors goFlux development)
```
feat: add import function for GasmetPD
feat: add auto.deadband function
fix: correct chamber volume calculation in import.LI8200
fix: resolve flux calculation error in HM model
docs: clarify flux calculation methodology
docs: add examples to best.flux roxygen
refactor: extract chamber identification logic
chore: update R dependencies in DESCRIPTION
```

❌ **Bad** (avoid these)
```
Updated code
Fixed bugs related to fluxes
New features
wip
Add import function for GasmetPD (missing type)
feat Add import function for GasmetPD. (missing colon, wrong punctuation)
```

## Workflow

1. **Create a feature branch**: `git checkout -b feature/your-feature-name`
2. **Make your changes** to source code or documentation
3. **Commit with a conventional message**: 
   ```bash
   git commit -m "feat: add import function for your instrument"
   git commit -m "fix: correct flux calculation bug"
   ```
4. **Push to your branch**: `git push origin feature/your-feature-name`
5. **Create a pull request** on GitHub
6. **Once merged to `main`**, release-please automatically:
   - Creates a release PR with version bump based on commits
   - Updates `DESCRIPTION`, `NAMESPACE`, and generates `CHANGELOG.md`
   - Maintainer reviews and merges the release PR
   - GitHub creates a release with the new version tag

## When in Doubt

- Check recent goFlux pulls and releases: https://github.com/Qepanna/goFlux/releases
- Review the `CHANGELOG.md` to see how past changes were documented
- Ask in a GitHub issue if unsure about commit formatting
