# Centralized Run Logging Design

## Problem

`scripts/load_all.R` loads the project and unconditionally configures file logging when `USE_LOGS` is true. Sourcing the project therefore creates `run_log_<timestamp>.txt` and a matching JSONL file for tests, tools, publishing, and interactive inspection, even when no analysis run begins. Empty and partial files are indistinguishable from attempted analyses by filename alone.

`setup_logging()` in `scripts/utils/logging_utilities.R` is the single logger implementation, but run lifecycle ownership is not centralized. Full-run phase markers are emitted inside `main_execution()`, while logger creation and connection cleanup are controlled elsewhere.

## Required End State

`load_all.R` loads functions and configures console behavior without creating file logs. One explicit lifecycle API owns file-log creation, active-run state, completion metadata, and connection cleanup.

The API supports these explicit run types:

- `full_analysis`
- `cohort_analysis`
- `objective_analysis`
- `tool`
- `test`
- `publish`

Only `full_analysis` runs are eligible as publication provenance. Nested objective and cohort calls reuse the active logger rather than starting another file logger. Tools, tests, publishers, and interactive sessions never create analysis-run logs implicitly.

## Lifecycle API

Introduce one public entrypoint pair, with implementation names finalized during the next pass:

```r
run_context <- start_run_log(
    run_type,
    runtime_root,
    source_revision,
    configuration,
    output_roots
)

finish_run_log(
    run_context,
    status,
    datasets_analyzed,
    warnings,
    errors,
    produced_files
)
```

The active context is stored centrally and exposes a read-only accessor for nested calls. Starting a nested run while a logger is active attaches child context to the current run. Starting a second top-level file logger is an error.

`finish_run_log()` records terminal status and closes both text and JSONL connections on success, warnings-completed, error, and interrupt paths. Entrypoints use `on.exit()` to guarantee finalization. Connection ownership is explicit; finalizers are a last-resort safeguard, not the normal cleanup path.

## Provenance Record

The central run record contains:

- run identifier and run type;
- start and finish timestamps;
- terminal status: `success`, `warnings`, `error`, or `interrupted`;
- canonical runtime root;
- source revision and dirty-state indicator;
- analysis configuration relevant to output generation;
- package environment identifier or lockfile digest;
- selected cohorts and objectives;
- datasets analyzed;
- declared output roots;
- produced-file paths, sizes, modification times, and checksums;
- warning and error counts.

Text logs remain human-readable. JSONL remains the structured event stream. Both are projections of the same active run context and share a run identifier.

## Entrypoint Migration

1. Remove logger file creation from `load_all.R`; retain console-only logger setup if required.
2. Wrap `main_execution()` with a `full_analysis` lifecycle.
3. Wrap standalone cohort and objective commands with their explicit run types only when no active run exists.
4. Mark tool, test, and publish entrypoints with their own run types when file logging is requested; default them to console-only operation.
5. Make nested analysis functions consume the active context without opening connections.
6. Update the publisher to read structured lifecycle metadata and accept only completed `full_analysis` runs.
7. Remove the old implicit setup path after all entrypoints migrate. Do not retain a compatibility branch or a duplicate logging path.

## Publication Eligibility

A provenance log is publishable only when its central metadata reports:

- `run_type == "full_analysis"`;
- terminal status `success` or `warnings`;
- all three canonical datasets analyzed;
- the declared runtime root matches the files being published;
- every selected output and analytic-data file is included in, or no newer than, the recorded output provenance.

Publishers copy only the human-readable text log to the snapshot root. JSONL remains a runtime diagnostic artifact.

## Tests

- Sourcing `load_all.R` creates no text or JSONL file.
- Full, cohort, objective, tool, test, and publish entrypoints record the correct run type.
- Nested calls reuse one run identifier and one connection pair.
- A second top-level logger is rejected.
- Success, warnings, error, and interrupt paths finalize metadata and close connections.
- Publishers and tools do not create analysis-run logs while inspecting the project.
- Only completed full runs pass publication provenance selection.
- Text and JSONL events share run identity and terminal status.
- Runtime root, source revision, configuration, environment, and produced-file provenance are recorded.

## Documentation Migration

Update repository instructions, technical documentation, CLI help, and test helpers in the same focused logging pass. Describe only the explicit lifecycle API and current entrypoint behavior; remove references to logger creation as a project-load side effect.
