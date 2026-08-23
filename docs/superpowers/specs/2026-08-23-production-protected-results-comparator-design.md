# Production Protected-Results Comparator Design

## Purpose

The checked-in protected-results comparator currently exercises only synthetic
fixtures. It has no production extraction path, so an Objective 0–4 runtime
cannot satisfy its artifact contract. This design adds one production bundle
builder and extends the existing comparator contract so a protected base and a
candidate can be compared by scientific domain without placing private data in
Git or treating expected Objective 4 changes as unrelated regressions.

The immediate use case is the Objective 4 AAO remediation branch. The design is
general enough for later protected analysis changes and cohort growth, but it
does not create a new publication path and does not alter any estimator or
analysis output.

## Decisions

### One production extraction path

Create `scripts/tools/build_production_comparison_bundle.R`. The CLI consumes:

- `--runtime-root`: a completed isolated Objective 0–4 runtime;
- `--contract`: the production extraction/comparison contract;
- `--output-root`: a private runtime bundle root; and
- `--report`: a PHI-free extraction report.

The builder validates the entire contract and all source artifacts before it
writes the bundle. It then creates only the relative artifacts declared by the
contract. Missing, duplicate, ambiguous, malformed, or unsupported source
artifacts stop the build. It never substitutes a nearby file, an older runtime,
or a broader result.

The bundle remains beneath `runtime/runs/<task>/`. Patient identifiers may be
present in private cohort-comparison artifacts, but neither bundle contents nor
identifier-derived hashes enter Git or the sanitized reports.

### Separate synthetic and production contracts

Keep `docs/maintenance/important_results_contract.yaml` as the synthetic
comparator regression contract. Add
`docs/maintenance/production_results_comparison_contract.yaml` for actual
analysis runs. The production contract is the single source of truth for:

- objective and scientific-domain ownership;
- exact source path beneath a completed runtime;
- extraction mode and declared output path/type;
- comparison expectation; and
- whether an entry is required for PR clearance.

Production entries use one of three expectations:

- `must_equal`: any semantic difference blocks clearance;
- `must_change`: semantic equality blocks clearance because the protected
  scientific remediation is expected to affect that artifact; or
- `may_change`: equality or difference is allowed, but the status remains
  visible for review.

The expectation is declared before the actual-data comparison. Candidate
values never modify the contract.

### Growth and scope policy

The contract protects declared semantics rather than freezing the current data
shape:

- no entry hard-codes today’s cohort size, event count, or number of rows;
- cohort projections use `candidate_superset`: every base stable ID must remain
  present with the same protected values, while newly added candidate IDs are
  permitted and reported as `growth_added`; removals or changed shared rows
  fail;
- row order is canonicalized by the stable ID before comparison;
- declared protected columns must be present and comparable, while additional
  non-protected columns are ignored by that projection and reported as
  `untracked_columns` for intentional contract review;
- new protected columns, artifact classes, or objective domains require a
  small contract amendment and a regression test, not a code-path rewrite; and
- only artifacts explicitly listed as protected are compared. Incidental logs,
  timestamps, image bytes, caches, and unrelated new outputs are not pulled
  into the comparator.

This gives future data growth a stable path without making the protection
vacuous: shared-row semantics, removals, declared endpoint fields, and all
required protected artifacts remain fail-closed.

### Comparator schema evolution

Extend `scripts/tools/compare_important_results.R` to support the production
contract while preserving the current synthetic behavior through the same
comparison engine. A comparison without an explicit expectation has the
single default `must_equal`; there is no alternate fallback path.

The sanitized report records only:

- contract and comparator versions;
- artifact ID, objective, domain, expectation, and result status;
- sanitized reason codes; and
- overall clearance status.

It never records compared values, cell contents, patient identifiers, or
identifier-derived hashes. `must_equal` mismatches and `must_change` matches
return nonzero. Expected differences and permitted differences are reported
without exposing their contents.

## Production artifact boundary

The builder creates a small semantic bundle rather than copying every runtime
file. Every protected claim is represented once at its closest deterministic
source.

### Objective 0

- `must_equal`: full/restricted/GKSRS cohort membership under the
  `candidate_superset` growth policy;
- `must_equal`: imported source fields and non-MFS derived fields needed by
  Objectives 1–3;
- `must_change`: incident post-treatment MFS eligibility/time/event projection;
- `may_change`: Objective 0 validation summaries that describe the intended
  endpoint-contract change.

All row-bearing Objective 0 projections use the same growth policy. RDS
projections sort by the stable study ID, require unique IDs, preserve
column names and typed missingness, and write private ordered cohort JSON. The
contract enumerates every protected column; regex or "all except" selection is
not allowed, but unprotected source columns may be added without changing this
projection.

### Objective 1

`must_equal` entries cover the key efficacy event-support, survival-rate,
effect-summary, and tumor-height result workbooks for the cohorts in which they
are produced. The contract does not require every generated Objective 1 file;
it protects a deliberately small set of stable, reader-relevant outputs.
Objective 1 continues to use the preserved raw metastasis and PFS facts, so a
declared Objective 1 difference is an unrelated regression.

### Objective 2

`must_equal` entries cover one stable primary summary/effect artifact for each
declared safety endpoint: visual acuity, retinopathy, neovascular glaucoma, and
serous retinal detachment. These outputs are outside the Objective 4
remediation scope; auxiliary diagnostics are not copied merely to increase
coverage.

### Objective 3

`must_equal` entries cover the PFS-2 cohort-support, treatment-summary, model,
and explicit skip artifacts when those artifacts are declared for the run. PFS
behavior was deliberately preserved by the baseline-metastasis correction.

### Objective 4

- `must_change`: the full-cohort exploratory no-GEP workbook containing the
  corrected nested-CV/IPCW performance;
- `must_change`: the incident-MFS sensitivity/eligibility summary;
- `may_change`: full-cohort GEP MFS/MSS consolidated summaries and validation
  narratives affected by endpoint or wording corrections;
- `must_equal`: Objective 4 source-category counts and imported GEP facts that
  are not derived from the remediated estimator.

Plot PNG bytes are not compared. The contract selects the workbooks or JSON
payloads from which the protected plots and labels are generated. This avoids
false differences from image metadata while still comparing the plotted
scientific values and displayed labels.

## Full-run requirement

Base and candidate comparisons use clean isolated runtimes and run every
objective required by the production contract. This AAO remediation contract
requires Objectives 0–4; a future contract may name a narrower or broader
scope. A partial run cannot satisfy a contract that requires its missing
objective. Each runtime must contain a completed log for the required scope,
run manifest, Git SHA, paired raw-workbook and `renv.lock` fingerprints,
analytic-RDS fingerprints, seed contract, and output inventory.

The current protected base SHA and current raw-workbook fingerprint are recorded
in the validation ledger for this run, not frozen as permanent contract values.
Future data growth is valid when the paired base/candidate runs use the same new
input fingerprint and the declared growth policy passes.

## Data flow

1. Run the complete base pipeline in its detached worktree and isolated
   runtime.
2. Run the complete candidate pipeline in the remediation worktree and a
   separate isolated runtime.
3. Build a production comparison bundle from each completed runtime using the
   same immutable production contract.
4. Compare the two bundles with the existing protected comparator.
5. Require every `must_equal` entry to match and every `must_change` entry to
   differ; list `may_change` outcomes for review.
6. Run the separate AAO accepted-abstract gate against the candidate workbook.
7. Record only aggregate results and sanitized reason codes in the tracked
   validation ledger.

The AAO gate remains the authority for presentation review. The production
comparator establishes regression isolation; it does not decide whether a
changed Objective 4 claim is scientifically acceptable.

## Failure behavior

The builder and comparator fail closed for:

- incomplete or warning-only runs without a validated completed full-run log;
- raw-workbook or lockfile mismatch between paired runs;
- a source path outside the declared runtime root;
- missing artifacts or duplicate/ambiguous matches for a declared source;
- missing or duplicated stable IDs;
- missing declared columns, sheets, formulas, or displayed labels in a
  protected projection;
- unsupported extraction/comparison types;
- `must_equal` differences;
- `must_change` equality; or
- any report path that could expose private contents.

Warnings and skipped secondary analyses remain explicit protected artifacts
when the contract declares them. The builder does not convert a skip into a
success or silently omit an infeasible result. Unlisted auxiliary outputs are
outside the comparison by design and are not treated as protected evidence.

## Testing

Synthetic tests must cover:

- deep contract validation and safe relative paths;
- identical full bundles;
- one difference in each supported semantic artifact type;
- `must_equal`, `must_change`, and `may_change` result/exit semantics;
- missing, duplicate, unexpected, and ambiguous source artifacts;
- stable-ID sorting and row-order invariance;
- candidate growth additions and removal/change failures;
- duplicate/missing study IDs and missing declared columns;
- typed missingness and displayed-string preservation;
- workbook sheet order, dimensions, formulas, values, and formats;
- incomplete run logs and mismatched raw/lock fingerprints;
- PHI-free extraction and comparison reports;
- manifest inclusion so CI cannot omit the new tests; and
- a bounded-contract test proving that an unlisted auxiliary file does not
  silently become protected.

Actual-data verification then runs the complete base and candidate pipelines,
builds both bundles, runs the protected comparator, reruns the AAO gate, and
updates `docs/validation/objective4-aao-validation-remediation.md` with exact
commands, aggregate statuses, and artifact hashes.

## Acceptance criteria

The protected PR is comparator-cleared only when:

- both complete actual-data runs use identical raw and lock fingerprints;
- every required production artifact is extracted without fallback;
- all Objective 1–3 `must_equal` entries match;
- all Objective 0 `must_equal` source/cohort/non-MFS entries match;
- declared incident-MFS and corrected Objective 4 `must_change` entries differ;
- every other `may_change` result is listed for scientific review;
- the sanitized report contains no patient data or identifier-derived hashes;
- the AAO gate and manual scientific adjudication remain separately recorded;
  and
- the full portable suite and exact-head CI pass.

This clearance does not itself authorize publication or presentation refresh.
Those remain blocked until the revised endpoint-specific interpretation and
corrected values receive written investigator approval.

## Alternatives rejected

- **Owner waiver:** faster, but it would leave unrelated-result equality
  untested in the claim-changing PR.
- **Whole-tree file hashes:** easy to produce but confounded by timestamps,
  logs, image metadata, expected Objective 4 changes, and future output growth.
- **Fixture-shaped fabricated artifacts:** would make the existing command run
  without establishing production scientific equivalence.
