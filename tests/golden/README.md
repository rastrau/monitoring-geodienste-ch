# Golden-artefact equivalence harness

This directory contains a small harness that lets us refactor the analysis
pipeline (towards a `{targets}`-based DAG) while guaranteeing that the
refactored pipeline produces **the same data** as the current one.

## Layout

```
tests/golden/
├── README.md              # this file
├── capture_golden.R       # captures golden artefacts from the CURRENT pipeline
├── check_equivalence.R    # compares a NEW pipeline against the golden set
└── artefacts/             # generated; gitignored by default
    ├── _metadata.rds
    ├── df_current_cleaned.rds
    ├── df2_canton_aggregates.rds
    ├── df_canton_summary.rds
    ├── df_changes.rds
    ├── df_timeseries.rds
    ├── plt_data_prop_all.rds
    ├── plt_data_prop_wo_nd.rds
    ├── plt_data_prop_wo_nduc.rds
    ├── plt_data_missingdata.rds
    ├── plt_data_comparison.rds
    ├── plt_data_comparison_subset.rds
    ├── lineplot_number.rds
    └── lineplot_open_score.rds
```

## Workflow

1. **Freeze inputs.** Make sure the contents of `data/` are exactly what you
   want the comparison to be based on. In particular, do NOT run
   `preprocess.py` between capture and check — that would refresh
   `data/geodienste-ch.csv` and invalidate the comparison.

2. **Capture the golden set from the current pipeline** (on a clean
   working tree, ideally on `main`):

   ```sh
   LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 Rscript tests/golden/capture_golden.R
   ```

   The UTF-8 locale is required because `analyse.R` and `functions.R`
   contain non-ASCII characters (German umlauts in plot labels and
   `topic_title` mappings); running R under the `C` locale produces a
   parse error.

   This sources `analyse.R` and `time-series.R` in a fresh session and
   snapshots a small, well-defined set of intermediate data frames plus the
   data backing each plot. Plot objects themselves are not snapshotted
   (they carry environment references that are not stable across sessions);
   we snapshot `plot$data` and `ggplot2::layer_data(plot, i)` instead.

3. **Refactor.** Build the new pipeline (e.g. with `{targets}`) on a
   branch. Keep the existing scripts in place until equivalence is
   demonstrated, so the comparison stays meaningful.

4. **Check equivalence against the new pipeline:**

   ```sh
   LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 Rscript tests/golden/check_equivalence.R
   ```

   Edit `load_candidate()` inside that script to return objects produced by
   the new pipeline (e.g. `targets::tar_read(canton_aggregates)`). The script
   exits non-zero if anything differs.

## What is compared

| Artefact name                  | Origin in current code                      |
| ------------------------------ | ------------------------------------------- |
| `df_current_cleaned`           | `analyse.R` (line 27, after `clean_data()`) |
| `df2_canton_aggregates`        | `analyse.R` `df2` (with canton-level joins) |
| `df_canton_summary`            | `analyse.R` `df_canton`                     |
| `df_changes`                   | `analyse.R` change-detection table          |
| `df_timeseries`                | `time-series.R` `df_timeseries`             |
| `plt_data_prop_all` … `lineplot_open_score` | data backing each plot       |

## Comparison semantics

- Frames are coerced to plain tibbles and re-sorted on a stable key before
  comparison, so changes in row ordering do not flag as a difference.
- Numeric comparisons use a tolerance of `1e-10` (configurable per call) to
  absorb harmless floating-point noise from aggregation reordering.
- `waldo::compare()` is used when available for readable diffs; otherwise
  the script falls back to `all.equal()`.

## Notes / caveats

- The `df_changes` artefact depends on which historic CSV is picked as
  "4 weeks ago" (`csv_files[5]` in `analyse.R`). If the refactor changes
  the selection rule (recommended), expect this artefact to differ — that
  is a deliberate behavioural change and should be discussed before the
  golden set is updated.
- `functions.R:86` contains `contract_required_wms = replace_na(contract_required_data, FALSE)`
  which looks like a copy/paste bug (it should plausibly reference
  `contract_required_wms`). The golden capture records whatever the current
  code does; any deliberate fix will surface as a diff and must be approved.
- The `artefacts/` subdirectory is generated and should be regenerated
  whenever inputs change. Recommended `.gitignore` addition (not applied
  automatically):

  ```
  tests/golden/artefacts/
  ```
