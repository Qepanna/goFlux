# Contributing to goFlux

Thank you for your interest in contributing to goFlux! This document outlines guidelines for contributing, particularly regarding commit messages which help automate our release process.

## Commit Message Format

We follow **conventional commits** to enable automatic versioning and changelog generation. This helps maintain a clear release history and makes it easy to understand what changed between versions.

### Format

```
<type>: <subject>
```

### Commit Types

- **`feat:`** — A new feature or functionality
  - Example: `feat: add import function for GasmetPD`
  - → Bumps **minor** version (e.g., 0.2.0 → 0.3.0)

- **`fix:`** — A bug fix
  - Example: `fix: resolve flux calculation error in HM model`
  - → Bumps **patch** version (e.g., 0.2.0 → 0.2.1)

- **`docs:`** — Documentation changes only (README, guides, roxygen comments)
  - Example: `docs: update installation instructions`
  - → No version bump

- **`refactor:`** — Code refactoring without feature changes
  - Example: `refactor: simplify import workflow`
  - → No version bump

- **`perf:`** — Performance improvements
  - Example: `perf: optimize flux calculation loop`
  - → No version bump

- **`test:`** — Test additions or updates
  - Example: `test: add unit tests for autoID function`
  - → No version bump

- **`chore:`** — Maintenance tasks (dependencies, CI, build scripts)
  - Example: `chore: update R dependencies`
  - → No version bump

### Rules

- Start with lowercase type (e.g., `feat:` not `Feat:`)
- Keep subject line under 50 characters
- Use imperative mood ("add feature" not "adds feature" or "added feature")
- Don't end with a period
- If the commit fixes a GitHub issue, reference it: `fix: resolve flux calculation error (closes #123)`

### Examples

✅ **Good**
```
feat: add import function for GasmetPD
fix: correct chamber volume calculation
docs: clarify flux calculation methodology
refactor: extract chamber identification logic
```

❌ **Bad**
```
Updated code
Fixed bugs related to fluxes
New features
WIP: still working on this
```

## Workflow

1. Fork or create a feature branch: `git checkout -b feature/your-feature-name`
2. Make your changes
3. Commit with a conventional message: `git commit -m "feat: add your feature"`
4. Push to your branch
5. Create a pull request (PR)
6. Once merged to `main`, release-please will automatically:
   - Create a release PR with version bump
   - Update `DESCRIPTION`, `NAMESPACE`, and `CHANGELOG.md`
   - You review and merge the release PR
   - GitHub creates a release with the new version

## Questions?

If you have questions about commits or contributions, please open an issue or contact the maintainers.
