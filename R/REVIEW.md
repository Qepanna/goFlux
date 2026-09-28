# goAquaFlux — code review, fixes, and recommendations

This reviews the aquatic extension to `goFlux` (floating-chamber GHG fluxes with
methane ebullition). Each finding is tagged by severity. Every change I made to
the source is marked inline in the `.R` files with `## FIX:`, `## NEW:` or
`## NOTE:` so you can diff against your originals and keep your targeted-edit
workflow.

`goFlux.R` and `goFlux-package.R` were reviewed but left **unchanged** — they are
the upstream engine and are sound. A couple of minor observations on them are at
the end. All revised files parse under R 4.3, and the core algorithms
(`find.bubbles`, window selection, ebullition/total math) were validated on
synthetic incubations with a known step (details at the end).

---

## 1. Critical bugs (wrong results or crashes)

**C1 — Water-vapour dilution correction was silently disabled (`goAquaFlux.diffusive`).**
`goAquaFlux` converts the water column to a mole fraction (`H2O_mol = ppm/1e6`)
and then the diffusive step called `goFlux(..., H2O_col = "H2O_mol")`. But
`goFlux` divides its `H2O_col` by `1e6` again — so the correction was applied to
`ppm/1e12`, i.e. effectively zero. This is exactly the doubt in your comment
`# here a doubt if using H2O_col = "H2O_mol" is correct`. It is wrong, and it
biases every diffusive flux (the dilution correction never happened).
*Fix:* `goAquaFlux` now also carries the original ppm as `H2O_ppm`, and the
diffusive step passes `H2O_col = "H2O_ppm"`, letting `goFlux` do its single
conversion. If no water column exists, the correction is disabled quietly.

**C2 — Crash for any non-bubble gas when bubbles are present (`goAquaFlux.diffusive`).**
`.select_diffusive_window()` called `.has_abrupt_change(..., window = window, ...)`,
but that helper's parameter is `window_size`, not `window`. R raises
`unused argument (window = 30)`. This path fires whenever `gastype != bubble_gas`
and bubbles were detected — i.e. the *core multi-gas use case* you care about
(ebullition affecting CO₂/N₂O windows). *Fix:* corrected the argument name;
hardened the helper against `lm` failures and NA slopes.

**C3 — `message` field returned the base function, not a value (`goAquaFlux.total`
and `goAquaFlux.ebullition`).** The local variable `message` was only assigned
inside the "suspicious"/"inconsistent" branch. On the normal path,
`return(list(message = message))` resolved `message` to `base::message` (a
function) and stuffed it into the result list. *Fix:* initialise `msg <-
NA_character_` up front and never shadow `base::message`. (Validated: the field
is now character `NA`.)

**C4 — Documentation attached to the wrong function (`goAquaFlux.R`).** The big
roxygen block sat immediately above the `.bind_with_id` helper, so roxygen2 would
attach the docs *and `@export`* to `.bind_with_id`, leaving `goAquaFlux`
undocumented and unexported. *Fix:* moved `.bind_with_id` to the end of the file
so the block documents `goAquaFlux`.

**C5 — `goAquaFlux.total(flux.term=...)` had a required, unused, never-passed
argument.** It happened not to error only because of lazy evaluation. *Fix:*
removed it; the hard-coded `1.2` is now the documented `tolerance` argument.

---

## 2. Correctness / robustness issues

**R1 — Inconsistent return shapes.** `goAquaFlux.ebullition` returned
`inconsistent` in some branches and `flag_inconsistent` in others, and omitted
`message` on the main path; the `goAquaFlux` fallback list used yet another key
(`inconsistent`). *Fix:* one internal `.out()` helper guarantees identical
fields on every exit; the `goAquaFlux` fallback list now matches.

**R2 — Progress bar never closed (`goAquaFlux`).** `txtProgressBar` was opened but
`close(pb)` was missing. *Fix:* added.

**R3 — `return_df` was declared and documented but never used (`goAquaFlux`).** The
function always returned the 3-table list. *Fix:* wired up — `TRUE` (default)
returns the tidy list; `FALSE` returns the raw per-incubation list for power
users. Docs updated to describe the actual output.

**R4 — `first_bubble_time` could report a non-bubble time (`goAquaFlux.diffusive`).**
It returned the diffusive *stop* time, which for non-bubble gases can be
`max(Etime)` (no bubble at all). *Fix:* the reported `first_bubble_time` is now
the actual first detected bubble start (or `NA`); the internal windowing time is
separate.

**R5 — "Full series" dropped its last observation.** When no truncation was
intended, `df[df$Etime < max(Etime)]` still cut the final point. *Fix:* return the
full `df` when no abrupt change is found. (Validated: 401 → 401 rows.)

**R6 — Self-referential default `criteria = criteria` (`goAquaFlux.diffusive`).**
Works only because `goAquaFlux` always passes it; calling the function directly
without `criteria` triggers a "promise already under evaluation" error. *Fix:*
gave it the real default criteria vector.

**R7 — Two-point SE counted NAs (`goAquaFlux.ebullition`).** `n0/nf` used
`sum(idx)` (window width) while the means used `na.rm = TRUE`, so the SE could be
mismatched. *Fix:* count non-missing values only.

**R8 — Plotting crashes with no bubbles (`flux.plot.aqua`).** `first_bubble` and
`df_diff` were only defined on the bubble path, but were read unconditionally
later. The "input is a data.frame" fallback was also dead (`!is.list(df)` is
never TRUE for a data.frame) and referenced an undefined `flux.results`. *Fix:*
always initialise `first_bubble`/`df_diff`; test `is.data.frame()` first; return
the plot list explicitly.

---

## 3. Documentation & packaging

**D1 — Wrong units in `goAquaFlux` docs.** `Area` was documented as m² and
`offset` as mm, but the underlying `goFlux` math requires **cm²** and **cm**
(`Vtot = Vcham + Area*offset/1000`, and the 10,000 cm²→m² factor). Corrected.

**D2 — Missing `@importFrom` tags → `R CMD check` failures.** `find.bubbles` used
`mad/median/var/sd/quantile/approx` (all `stats`) with no imports; the plot used
`geom_rect`, `ggtitle` and `.data` with no imports. Added the tags at the
function level, where roxygen2 aggregates them into `NAMESPACE`. You do **not**
need to hand-edit `goFlux-package.R`; just re-run `roxygen2::roxygenise()`.

**D3 — Invalid/mismatched roxygen tags.** `goAquaFlux.total` used `@internal`
(not a tag) → changed to `@keywords internal`. Internal-function `@examples`
referenced undefined objects and would run during checks → wrapped in
`\dontrun{}`. `flux.plot.aqua` documented `@param flux.results` while the arg is
`flux.results.ls` → aligned. Added the previously-undocumented
`use_bubble_detection`, `bubble.method` and `bubble.args` params.

---

## 4. Processing-strategy improvements

**P1 — `find.bubbles` detection robustness (new `method = "diff"`).** The existing
`"mad"`/`"variance"` metrics run on the *level* of the signal, so a steep but
purely **diffusive** ramp inflates rolling dispersion and can trigger false
positives. The new `"diff"` method rolls the **variance of the first
differences**: a constant diffusive slope → near-constant increments → low
differenced variance, while an ebullition step → one large increment → a clear
spike. (Note: rolling *MAD* of increments does **not** work — MAD ignores a lone
outlier by design; that is why the diff method uses variance. I caught this in
testing.) Default stays `"mad"` to preserve your validated behaviour; `"diff"` is
opt-in and recommended for high-diffusion incubations.

**P2 — Magnitude anchored at the true jump.** The step-dummy previously sat at the
chunk's leading edge; symmetric detection windows start *before* the real step,
biasing magnitude low. Now the dummy is anchored at the largest positive
increment inside each chunk. (Validated: both methods recover magnitude 300.0 vs
a true 300, and slope 2.0 vs a true 2.0.)

**P3 — Bubble detector is now tunable from `goAquaFlux`.** Added `bubble.method`
and `bubble.args` (a named list forwarded to `find.bubbles`, overriding
defaults). Beginners get sensible defaults; experts get every knob (`k`,
`min_magnitude`, `min_snr`, `var.quantile`, …) without changing internal code.

**P4 — Diffusive window logic generalised to all gases.** For the bubble gas the
window ends at the first bubble (ebullition perturbs it by definition). For other
gases the window is only cut where a bubble **coincides with an abrupt local
slope change**; otherwise the full series is used. This directly addresses
"ebullition possibly impacting other gases" without needlessly discarding data.

---

## 5. Larger recommendations (not yet applied — they change structure/behaviour)

These are worth doing but I left them for you to decide, since you validate before
committing and prefer targeted edits.

1. **De-duplicate the ~400-line validation/prep block.** `goAquaFlux` copies
   nearly all of `goFlux`'s argument checking and data cleaning. Extract two
   internal helpers — e.g. `.validate_chamber_inputs()` and
   `.prep_chamber_split()` — and call them from both. One source of truth, far
   less drift risk. It's mechanical but touches both functions, so it deserves
   its own PR with side-by-side output tests.

2. **Parallelise the incubation loop.** The main loop is serial. `pbapply` is
   already a dependency; wrapping the per-incubation work in `pblapply(...,
   cl = cl)` with an optional `cl`/`ncores` argument would scale to large
   datasets. Note that calling `goFlux()` per incubation re-runs its full
   validation and prints a mini progress bar each time — consider a lightweight
   internal fitting path (LM/HM directly) for the diffusive step to avoid that
   overhead, or wrap the call in `SimDesign::quiet()`.

3. **Ebullition attribution for non-CH₄ gases.** Currently ebullition flux is only
   computed when `gastype == bubble_gas`; other gases get diffusion only. For
   gases that partition into bubbles (some CO₂), you could scale the CH₄ bubble
   volumes by a per-gas bubble concentration (measured or assumed) to attribute a
   small ebullitive component. This is a modelling decision — flag it explicitly
   in the docs either way so users know CO₂/N₂O ebullition is treated as zero.

4. **Expose the abrupt-change thresholds** (`abrupt.window`, `abrupt.threshold`,
   `abrupt.min.points`) through `goAquaFlux` as well — they are currently only
   reachable in `goAquaFlux.diffusive`.

5. **Comment numbering.** The in-code step comments in the main loop read 1, 2, 5,
   4, 3. Execution order is fine (detect → ebullition → diffusion → total); just
   renumber for readers.

---

## 6. Notes on `goFlux.R` (left unchanged)

- `Pcham`/`Tcham`/`H2O` handling and the LM/HM/kappa-max logic look correct.
- Minor: the final per-row warning loops (`nb.obs < warn.length`, `< 3`) iterate
  over `flux_results` with base `for`; fine, just O(n). No action needed.
- The `H2O_mol` re-derivation inside `goFlux` is the reason for bug **C1** — no
  change needed in `goFlux` itself, the fix belongs upstream in `goAquaFlux`.

---

## 7. Testing checklist (run against real data before committing)

1. **Water correction actually changes CO₂ flux.** Run one incubation with the
   real H₂O column and again with `H2O_col = NULL`; diffusive CO₂ flux should now
   differ (before the fix it did not).
2. **Multi-gas with bubbles no longer crashes.** `goAquaFlux(IDed, "CO2dry_ppm")`
   on data where CH₄ bubbles exist — previously errored (C2).
3. **Bubble-gas window truncates; other gases keep more data.** Check
   `flux_summary$n_obs.diffusion` for CH₄ vs CO₂ on the same incubations.
4. **`return_df = FALSE`** returns the raw list; **`TRUE`** the 3 tables.
5. **`bubble.method = "diff"`** vs `"mad"` on a few high-diffusion incubations;
   compare detected events and magnitudes on your reference lakes.
6. **Suspicious-flag path:** an incubation where summed bubble magnitude is large
   should set `flag_suspicious`/`flag_inconsistent` and emit a warning — confirm
   the message is readable text (not a function).
7. **Plotting with and without bubbles** — both should render (R8).

---

## 8. File-by-file change summary

| File | Key changes |
|---|---|
| `goAquaFlux.R` | C4 (roxygen/helper moved), C1 (carry `H2O_ppm`), R1 (fallback shape), R2 (`close(pb)`), R3 (`return_df`), D1 (units), P3 (`bubble.method`/`bubble.args`), thread `bubble_gas` to diffusive |
| `goAquaFlux.diffusive.R` | C1 (`H2O_ppm` to goFlux), C2 (arg-name crash), R4 (`first_bubble_time`), R5 (full series), R6 (`criteria` default), full roxygen |
| `goAquaFlux.ebullition.R` | C3 (`msg` init), R1 (uniform return via `.out`), R7 (NA counts), `@importFrom stats var`, full roxygen |
| `goAquaFlux.total.R` | C3 (`msg` init), C5 (drop `flux.term`, add `tolerance`), full roxygen |
| `find.bubbles.R` | D2 (stats imports), P1 (`method="diff"`), P2 (jump-anchored magnitude), default `window.size`, `\dontrun` example |
| `flux.plot.aqua.R` | R8 (undefined vars, dead branch, explicit return), D2/D3 (imports, param name) |
| `demo_goAquaFlux.R` | console-clear fix; shows `bubble.method`/`return_df` |
