# stateR — Package Family Architecture

Status as of 2026-08-20. Supersedes earlier `ReportR` / `ELReportR` naming.

Flags follow repo convention: `[CONFIRMED]` = decided by Nathan.
`[PROPOSED — CONFIRM]` = suggested, not yet ratified.

---

## 1. Family

`stateR` is the family name. **There is no `stateR` package.**

A meta-package is explicitly not planned; the `tidyverse`-style umbrella
pattern is already ruled out for `Imports:`.

```
stateR  (family name — no package)
│
├── stateSpecs           schema + reader + validation; spec instances;
│                        jurisdiction policy; suppression; cut-score methods
│
├── stateSummative       four lines + reporting layer (provisional)
│   └── R/report-*.R     generic-input only; extraction candidate
│
├── stateELP             duration in status, exit trajectories, ELP outcomes
├── stateAccountability  indicator recomputation, rule/growth simulation
└── stateScoring         drift, DIF, equating, replication, rounding, TIF
```

Two packages exist today (`stateSpecs`, `stateSummative`). Three are planned.

| Package | Data model | Cadence | Status |
|---|---|---|---|
| `stateSpecs` | schema + spec/policy files | data: per state publication; code: per bugfix | `[CONFIRMED]` name; absorbed reader + suppression |
| `stateSummative` | year × student × grade × subject × program | annual | `[PROPOSED — CONFIRM]` name; package = ReportR renamed |
| `stateELP` | `el_long` | annual | `[CONFIRMED]` name; = ELReportR |
| `stateAccountability` | school × year × indicator × weight | per statute change | `[CONFIRMED]` name and concept |
| `stateScoring` | item × administration (+ responses if held) | forensic / as needed | `[PROPOSED — CONFIRM]` name; build last |

### Naming grammar

Prefix-`state`, camelCase. The `R` lives only on the family name.
Future members are `stateThing`, never `thingR`.

### Namespace notes

`stateR` is available on CRAN, but the neighborhood is crowded:

- `statar` (CRAN, active) — Stata-inspired data tools. One letter off.
- `states` (CRAN, active) — country-year panels; owns the phrase
  "state panel data" meaning *countries*.
- `StatER` (GitHub, dormant) — case-insensitively identical.
- Broadly, `state*` in software means state machines / state management.

Implication: any CRAN Title/Description must disambiguate explicitly
(e.g. "U.S. State Education Agency"), and must avoid the phrase
"state panel data".

Definitive re-check before locking:

```r
ap <- rownames(available.packages(repos = "https://cloud.r-project.org"))
grep("^stat", ap, ignore.case = TRUE, value = TRUE)
```

---

## 2. Dependency rules

```
stateSpecs
     ↑
stateSummative · stateELP · stateAccountability · stateScoring
```

1. **Everything imports `stateSpecs`.** Nothing else is imported in-family.
2. **No sideways imports.** No domain package imports another domain package.
   R enforces this as a hard build failure on cycles, not a style warning.
3. **Data crosses, code doesn't.** Where one domain's output feeds another
   (e.g. EL exit flags into accountability simulation), it crosses as a
   student-level file read as ordinary input. The consumer knows nothing
   about what produced it. Pipelines still run in sequence; the dependency
   arrow is never drawn.

Rule of thumb: if you are about to add a domain package to another domain
package's `Imports:`, what you actually want is a data handoff, or the
shared thing belongs in `stateSpecs`.

---

## 3. `stateSpecs`

Holds four things:

- **Spec schema** — the two-layer model. `tests` (atomic units: vendor,
  level scheme, per-grade scale ranges and cuts) + `map` (resolver keyed by
  content area and grade). Resolves mixed-vendor, EOC-not-grade-bound, and
  domain-sampled HS configurations.
- **Spec instances** — per assessment, e.g. `NJ_elp_2023`.
- **Jurisdiction policy** — scoped to the agency, not to any one assessment:
  `min_cell`, EL exit / growth / timeline rules, reporting conventions.
- **Reader + validation + cut-score methods.**

### Three declaration layers

Distinguish carefully; "schema" has been used ambiguously:

| Layer | Describes | Scope |
|---|---|---|
| Data schema | shape of the incoming student file | family-wide |
| Spec schema | assessment configuration (tests + map) | per assessment |
| Jurisdiction policy | suppression, exit rules, reporting conventions | per agency |

### Placement rules established

- LOSS/HOSS belong **per-grade in cut entries**, not program-level.
- EL exit / growth / timeline belong in **`policy`**, not the ELP
  measurement block.
- `min_cell` is **jurisdiction policy**, not spec — a state suppresses
  identically across summative, ELP, and accountability.

### Cut-score methods `[PROPOSED — CONFIRM]`

Logistic regression and decision consistency live here, on the logic that
everything about cut scores belongs together: the spec represents cuts,
validates cuts, resolves scores through cuts, and derives candidate cuts.
Domain-neutral by construction — takes a numeric predictor and a binary
outcome, returns a threshold plus classification consistency. Serves both
kindergarten-readiness cut comparison and ELP exit-criterion work.

Dependency footprint stays small: `glm` is base; Livingston-Lewis and
Rudner are light.

### Known cost of the collapse

`stateSpecs` now carries two cadences — spec data changes when a state
publishes new cuts; reader code changes when a resolver bug is fixed.
Merged, a cuts update risks shipping a code release.

**Mitigation:** version them independently. Every spec file carries its own
`schema_version`; the reader declares which versions it supports. A cuts
update is then a data-only patch. This is what makes **D5 load-bearing**.

---

## 4. `stateSummative` reporting layer

Reporting is folded into `stateSummative` **provisionally**, not
permanently. Rationale: ReportR already has working Quarto / officer /
PowerPoint / Word code, and the second consumer is anticipated rather than
real. Extraction gate is unchanged — nothing extracts until two packages
actually need it.

### The `report-*.R` contract

Files live at `R/report-*.R`. This is a **file naming convention only** —
R sources every file in `R/` into the same package namespace; the prefix
has no meaning to the build system.

Contract: reporting functions take **generic inputs only** — data frames,
results lists. No summative-specific structures, no proficiency vocabulary,
no grade-span logic, no assumptions about `el_results` vs. a summative
results object.

### Enforcement

The prefix cannot enforce the contract. A test must:

1. Construct a minimal generic input — a bare data frame, or a stub results
   list carrying none of the summative structure.
2. Pass it to every `report-*` function.
3. Assert it renders.

Anything that has quietly acquired a summative dependency fails immediately
rather than at extraction time.

### Tripwire

When `stateELP` first needs to render, there are exactly two exits:
**extract** reporting into its own package, or **duplicate** it.
`stateELP` importing `stateSummative` is not on the table.

If the contract held, extraction is `git mv R/report-*.R` plus tests,
`DESCRIPTION`, and `NAMESPACE` edits — roughly an hour. If summative
knowledge leaked into the theme and table layer, extraction is a rewrite.

### Suppression

Results objects keep `n` and stay **unsuppressed**. Suppression happens at
the **render boundary**, applying the `min_cell` value declared in
jurisdiction policy, with the implementation living in `stateSpecs` so it
exists once. This keeps complementary-suppression logic — where a hidden
cell is recoverable from the margins — in a single place.

---

## 5. Family pattern: module 00 reconciliation

Each domain package's module 00 reconciles against the published number.
Nothing downstream runs until it passes.

| Package | Must reproduce |
|---|---|
| `stateSummative` | the state's reported proficiency rates |
| `stateELP` | reported exit counts |
| `stateAccountability` | published school grades, school by school |
| `stateScoring` | vendor-reported scale scores from archived parameters |

Accountability has the hardest bar and the most dangerous failure mode: a
simulated distribution always looks plausible, so baseline failure is
silent. Exact recovery of the published baseline is a precondition for any
counterfactual.

---

## 6. Open schema decisions

| | Decision | Status |
|---|---|---|
| **D1** | Two-layer model (tests + map) as canonical | **Settled.** `NJ_elp_2023.R` ported |
| **D2** | Policy inline vs. separate referenced file | **Settled by implication — separate.** Falls out of the jurisdiction policy layer. `[PROPOSED — CONFIRM]`: never confirmed explicitly |
| **D3** | One registry file per type vs. per jurisdiction-year | **Open, blocking.** Likely *both*: specs by type, policy by jurisdiction-year |
| **D4** | Record umbrella marketing name in schema | **Open, unblocked.** Name is now `stateR`; this is recording it |
| **D5** | Align schema version string with Damian's AMR namespace | **Open, blocking.** Now the compatibility contract between spec data and reader code |

D3 and D5 gate reader code. Both are cheap now — a filing convention and a
string format — and expensive once specs exist for several states and
Claude Code sessions have generated resolver logic against a default.

---

## 7. MS TAC analysis mapping

Where the Mississippi TAC plan sections land. Useful as a scope test.

- **stateSummative** — 1.1, 1.3 (counting side), 1.5, 2.1, 2.2, 3.1, 3.2,
  4.1, 4.3, 5.1, 5.3, 7.3
- **stateELP** — 4.2
- **stateAccountability** — 5.2, 6.1, 6.3
- **stateScoring** — 1.2, 1.3 (explanation side), 1.4, 6.2, 7.1, 7.2, 7.4
- **stateSpecs** — 6.4 (cut-score methods)

Two things worth noting in that mapping:

- **5.2 moved out of the Aggregate line** into accountability.
- **1.3 deliberately splits** across two packages: `stateSummative` counts
  students at LOSS/HOSS and flags the pattern; `stateScoring` explains it
  with test information functions and raw-to-scale mapping at the tails.

---

## 8. Unresolved

- **`stateScoring` scope.** Most SEAs don't hold item response data — many
  hold item parameters only as PDF appendices. The package must assume its
  inputs are usually *absent*, and expose an up-front capability check that
  reports which analyses are possible before anything runs. Open question
  is whether item-level work is in scope for the family at all, or stays
  hand-rolled per engagement.
- **`stateSummative` name.** ACCESS is also a summative assessment, so the
  contrast with `stateELP` isn't airtight. Accepted on the grounds that SEA
  usage reliably means the content-area accountability test.
- **`NOTES.md` item:** `stateSpecs` will need an administration-level
  metadata layer (jurisdiction × year × content area × vendor × form)
  before it can resolve parameter files. Third layer alongside tests and
  map. Not opened.

---

## 9. Next step

Write the spec schema as a **standalone JSON file, with no R around it.**

Whatever resists declarative expression is the true scope of the reader.
If that comes back large — especially if resolution logic keeps wanting
conditionals the schema can't hold — the collapse of the reader into
`stateSpecs` was wrong, and it fails within a day rather than three
modules in.

Settle D3 and D5 first; both are inputs to that file.
