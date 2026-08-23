# Production Protected-Results Comparator Design

## Purpose

The checked-in protected-results comparator already provides the comparison
engine and `publish_outputs.R` already defines the allowed reader-facing runtime
artifact surface. The missing piece is a production contract that names a small
set of those existing artifacts and distinguishes expected Objective 4 changes
from unrelated regressions. This design adds that contract and a small
expectation extension to the existing comparator; it does not add a second
bundle or extraction framework.

The immediate use case is the Objective 4 AAO remediation branch. The design is
general enough for later protected analysis changes and cohort growth, but it
does not create a new publication path and does not alter any estimator or
analysis output.

## Decisions

### Reuse the existing artifact surface

Do not add a production bundle builder. Add
`docs/maintenance/production_results_comparison_contract.yaml` whose paths are
relative to an isolated runtime and are selected from the existing
`publish_outputs.R` allowlist. The contract names only a small set of stable,
reader-facing workbooks/text/JSON artifacts for Objectives 0–4. The existing
comparator reads those files directly from each paired runtime.

This reuses the current artifact registry, workbook semantic comparison, safe
relative-path handling, sanitized reports, and runtime isolation. Patient data
remain only in the private runtime files. No patient rows, identifiers, or
identifier-derived hashes are copied to Git or reports.

### Separate synthetic and production contracts

Keep `docs/maintenance/important_results_contract.yaml` as the synthetic
comparator regression contract. Add
`docs/maintenance/production_results_comparison_contract.yaml` for actual
analysis runs. The production contract is the single source of truth for:

- objective and scientific-domain ownership;
- exact source path beneath a completed runtime;
- declared relative artifact path and comparison type;
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

### Data growth policy

The contract does not hard-code cohort size, event counts, or row counts. Base
and candidate are always run against the same current raw workbook, so future
patients are included in both sides automatically. The paired input fingerprint
must match; the comparator then protects the resulting declared artifacts
without a separate row-level projection or a second growth framework.

New reader-facing artifact types are added only when they become important
enough to protect, by a small contract entry and test. Incidental logs,
timestamps, image bytes, caches, and unlisted auxiliary outputs remain outside
the comparison by design.

### Minimal comparator extension

Extend `scripts/tools/compare_important_results.R` to support the production
contract while preserving the current synthetic behavior through the same
comparison engine. A comparison without an explicit expectation has the
single default `must_equal`; there is no alternate comparison path.

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

The contract selects a small semantic set rather than comparing every runtime
file. Every protected claim is represented once at its closest deterministic
source.

### Objective 0

`must_change` covers the existing full-cohort validation workbook because the
baseline-MFS eligibility and source reconciliation are intentionally corrected.
The contract does not compare raw RDS rows or create a second cohort projection.

### Objective 1

`must_equal` entries cover a deliberately small set of existing efficacy
event-support, survival-rate, effect-summary, and tumor-height workbooks.
Objective 1 continues to use the preserved raw metastasis and PFS facts, so a
declared Objective 1 difference is an unrelated regression.

### Objective 2

`must_equal` entries cover one existing primary summary/effect artifact for the
declared safety endpoints. These outputs are outside the Objective 4 remediation
scope; auxiliary diagnostics are not added merely to increase coverage.

### Objective 3

`must_equal` entries cover the existing PFS-2 cohort-support, treatment-summary,
model, and explicit skip artifacts selected by the current publish allowlist.
PFS behavior was deliberately preserved by the baseline-metastasis correction.

### Objective 4

- `must_change`: the full-cohort exploratory no-GEP workbook containing the
  corrected nested-CV/IPCW performance;
- `may_change`: the incident-MFS sensitivity/eligibility summary;
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
objective. The validation ledger records the completed log for the required
scope, Git SHA, paired raw-workbook and `renv.lock` fingerprints, analytic-RDS
fingerprints, seed contract, and output inventory. The comparator itself owns
artifact semantics; it does not duplicate the run-provenance system.

The current protected base SHA and current raw-workbook fingerprint are recorded
in the validation ledger for this run, not frozen as permanent contract values.
Future data growth is valid when the paired base/candidate runs use the same new
input fingerprint and the declared growth policy passes.

## Data flow

1. Run the complete base pipeline in its detached worktree and isolated
   runtime.
2. Run the complete candidate pipeline in the remediation worktree and a
   separate isolated runtime.
3. Compare the declared artifacts in the two completed runtimes with the
   existing protected comparator and the same immutable production contract.
4. Require every `must_equal` entry to match and every `must_change` entry to
   differ; list `may_change` outcomes for review.
5. Run the separate AAO accepted-abstract gate against the candidate workbook.
6. Record only aggregate results and sanitized reason codes in the tracked
   validation ledger.

The AAO gate remains the authority for presentation review. The production
comparator establishes regression isolation; it does not decide whether a
changed Objective 4 claim is scientifically acceptable.

## Failure behavior

The existing comparator fails closed for:

- a source path outside the declared runtime root;
- missing declared artifacts;
- malformed JSON, text, or workbook files;
- workbook sheet, dimension, formula, value, or displayed-format changes;
- unsupported comparison types;
- `must_equal` differences;
- `must_change` equality.

Warnings and skipped secondary analyses remain explicit protected artifacts
when the contract declares them. The comparator does not convert a skip into a
success or silently omit an infeasible result. Unlisted auxiliary outputs are
outside the comparison by design and are not treated as protected evidence.

## Testing

Synthetic tests must cover:

- deep contract validation and safe relative paths;
- identical declared runtime artifacts;
- one difference in each supported semantic artifact type;
- `must_equal`, `must_change`, and `may_change` result/exit semantics;
- missing declared artifacts and malformed files;
- typed missingness and displayed-string preservation;
- workbook sheet order, dimensions, formulas, values, and formats;
- paired-run provenance records with mismatched raw/lock fingerprints;
- PHI-free comparison reports;
- preservation of current `publish_outputs.R` artifact allowlisting; and
- manifest inclusion so CI cannot omit the new tests.

Actual-data verification then runs the complete base and candidate pipelines,
compares the declared runtime artifacts, reruns the AAO gate, and updates
`docs/validation/objective4-aao-validation-remediation.md` with exact commands,
aggregate statuses, and artifact hashes.

## Acceptance criteria

The protected PR is comparator-cleared only when:

- both complete actual-data runs use identical raw and lock fingerprints;
- every required production artifact is present at its declared path;
- all Objective 1–3 `must_equal` entries match;
- the declared Objective 0 and Objective 4 `must_change` entries differ;
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
