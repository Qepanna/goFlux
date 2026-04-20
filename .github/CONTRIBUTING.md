# Contributing to goFlux

Thank you for your interest in contributing to goFlux! This document outlines guidelines for contributing, particularly regarding commit messages which help automate our release process.

We use **conventional commits** to enable automatic versioning and changelog generation via release-please. This helps maintain a clear release history and makes it easy to understand what changed between versions.

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

### Rules for Commit Messages

1. **Start with the type** in lowercase: `feat:`, `fix:`, `docs:`, etc. (not `Feat:` or `FEAT:`)
2. **Keep the subject line concise** — aim for under 50 characters
3. **Use imperative mood** — write "add feature" not "adds feature" or "added feature"
4. **Don't end with a period**
5. **Reference issues when applicable** — e.g., `fix: resolve flux calculation error (closes #123)`

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

## Workflow: From Development to Release

### During Development (on your feature branch)

Don't stress about commit formatting yet! You can use informal messages while experimenting:

1. **Create a feature branch**: `git checkout -b feature/your-feature-name`
2. **Make your changes** to source code or documentation
3. **Commit freely with informal messages**:
   ```bash
   git commit -m "wip: testing new import logic"
   git commit -m "trying different approach"
   git commit -m "debugging issue"
   ```
4. **Push to your branch**: `git push origin feature/your-feature-name`

### Before Merging (clean up your commits)

5. **Create a pull request** on GitHub with a clear description
6. **Before merging to `master`**, ensure your commits are clean and properly formatted:
   - **Option A (Recommended)**: Use GitHub's **"Squash and merge"** button and write a single proper commit message
     - This turns all your experimental commits into one clean `feat:` or `fix:` commit
   - **Option B**: Manually rebase/amend your commits locally to use conventional format

### After Merge to `master`

7. **Release-please checks commits since the latest reachable `goFlux-X.Y.Z` tag.**
   - It only runs release creation when it finds commit subjects starting with:
     - `feat:`
     - `fix:`
   - Supported forms include:
     - `feat: add import function for GasmetPD`
     - `fix: correct chamber volume calculation`
     - `feat(autoID): improve peak detection`
     - `fix(HM.flux)!: change default fitting behavior`

8. **What does NOT trigger an automated release PR (by policy):**
   - `docs: ...`
   - `chore: ...`
   - `refactor: ...`
   - `test: ...`
   - `perf: ...`

9. **When a release is triggered:**
   - release-please creates a release PR with version bump
   - updates `DESCRIPTION` and `CHANGELOG.md`/release notes
   - maintainer reviews and merges
   - GitHub creates the release tag

**Key Point**: You can use informal commits during development, but before merge use a clean commit message. Only `feat:` or `fix:` prefixes are intended to trigger automated release PRs in this repository.

## When in Doubt

- Check recent goFlux pulls and releases: https://github.com/Qepanna/goFlux/releases
- Review the `CHANGELOG.md` to see how past changes were documented
- Ask in a GitHub issue if unsure about commit formatting 
