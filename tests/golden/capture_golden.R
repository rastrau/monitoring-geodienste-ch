# tests/golden/capture_golden.R
#
# Capture canonical ("golden") intermediate artefacts produced by the CURRENT
# pipeline (analyse.R + time-series.R + functions.R), so that later refactors
# (e.g. onto {targets}) can be verified to produce identical outputs.
#
# How to use:
#   1. Make sure data/geodienste-ch.csv and the archive CSVs in data/ are
#      the inputs you want to freeze.
#   2. From the repo root, run:
#        Rscript tests/golden/capture_golden.R
#   3. RDS files will be written to tests/golden/artefacts/.
#
# This script does NOT modify any source file. It only sources the existing
# scripts in a fresh R session and snapshots selected objects.
#
# Determinism notes:
# - Frames are sorted on a stable key before being written, so that any
#   refactor that changes row order (but not row content) still compares
#   equal.
# - We snapshot the data backing each plot (layer_data + the plot's $data)
#   instead of the plot object itself, because ggplot/plotly objects carry
#   environment references that are not stable across sessions.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
  library(ggplot2)
})

out_dir <- here("tests", "golden", "artefacts")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stable_save <- function(obj, name, sort_cols = NULL) {
  # Coerce to plain tibble/data.frame for stable comparison; drop grouping.
  if (is.data.frame(obj)) {
    obj <- dplyr::ungroup(obj)
    obj <- tibble::as_tibble(obj)
    if (!is.null(sort_cols)) {
      sort_cols <- intersect(sort_cols, names(obj))
      if (length(sort_cols) > 0) {
        obj <- obj[do.call(order, lapply(sort_cols, function(c) obj[[c]])), ,
                   drop = FALSE]
        rownames(obj) <- NULL
      }
    }
  }
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path, version = 2)
  message("Wrote ", path)
  invisible(path)
}

snapshot_plot_data <- function(plt, name, seed = 1L) {
  # Snapshot:
  #   - the data frame attached to the ggplot itself (plt$data)
  #   - the per-layer computed data (geom-ready frames)
  # Avoids snapshotting environments, which would be unstable.
  #
  # We set an RNG seed before evaluating layer_data() because some plots use
  # position_jitter(), which would otherwise produce non-deterministic x/y
  # values and spurious diffs.
  set.seed(seed)
  if (inherits(plt, "plotly")) {
    # The pre-plotlyfy ggplot version is what we want; if only plotly was
    # captured, fall back to the plotly $x$data list.
    payload <- list(plotly_data = plt$x$data)
  } else {
    layer_dfs <- tryCatch(
      lapply(seq_along(plt$layers), function(i) ggplot2::layer_data(plt, i)),
      error = function(e) list(error = conditionMessage(e))
    )
    payload <- list(
      plot_data  = tibble::as_tibble(plt$data),
      layer_data = layer_dfs
    )
  }
  stable_save(payload, name)
}

# ---- Run the current pipeline in this session --------------------------------
# We source analyse.R and time-series.R as the Quarto report does. This
# populates df_current_cleaned, df, df2, df_canton, df_changes, df_timeseries
# and the plt_* objects in the global environment.
#
# Seed the RNG before sourcing so that ggplot's position_jitter() — invoked
# inside plot_comparison() and then baked into plotly objects by
# plotlyfy_w_zoom() — produces deterministic coordinates. Without this,
# plt_data_comparison and plt_data_comparison_subset would vary run to run.
set.seed(20240101L)

# config.R + functions.R are sourced transitively by analyse.R / time-series.R.
source(here("analyse.R"),     encoding = "UTF-8")
source(here("time-series.R"), encoding = "UTF-8")

# ---- Snapshot the canonical intermediate frames ------------------------------

# 1. Cleaned current data (wide form, after clean_data())
stable_save(
  df_current_cleaned, "df_current_cleaned",
  sort_cols = c("canton", "topic_title")
)

# 2. Per-canton aggregates (df2 in analyse.R), sorted deterministically
stable_save(
  df2, "df2_canton_aggregates",
  sort_cols = c("canton", "offering", "publication_type")
)

# 3. Canton-level summary used by the 2D comparison plot
stable_save(
  df_canton, "df_canton_summary",
  sort_cols = c("canton")
)

# 4. Change-detection table (4-weeks-ago vs current)
stable_save(
  df_changes, "df_changes",
  sort_cols = c("Kanton", "Datensatz")
)

# 5. Time-series aggregate (from time-series.R)
stable_save(
  df_timeseries, "df_timeseries",
  sort_cols = c("updated", "canton")
)

# ---- Snapshot the data underlying each plot ---------------------------------

snapshot_plot_data(plt_data_prop_all,        "plt_data_prop_all")
snapshot_plot_data(plt_data_prop_wo_nd,      "plt_data_prop_wo_nd")
snapshot_plot_data(plt_data_prop_wo_nduc,    "plt_data_prop_wo_nduc")
snapshot_plot_data(plt_data_missingdata,     "plt_data_missingdata")
snapshot_plot_data(plt_data_comparison,      "plt_data_comparison")
snapshot_plot_data(plt_data_comparison_subset,"plt_data_comparison_subset")
snapshot_plot_data(lineplot_number,          "lineplot_number")
snapshot_plot_data(lineplot_open_score,      "lineplot_open_score")

# ---- Record metadata about the capture --------------------------------------

meta <- list(
  captured_at    = Sys.time(),
  git_branch     = tryCatch(system("git rev-parse --abbrev-ref HEAD",
                                   intern = TRUE), error = function(e) NA),
  git_commit     = tryCatch(system("git rev-parse HEAD",
                                   intern = TRUE), error = function(e) NA),
  r_version      = R.version.string,
  updated_string = if (exists("updated")) updated else NA,
  input_csv      = here("data", "geodienste-ch.csv"),
  input_csv_md5  = tryCatch(tools::md5sum(here("data", "geodienste-ch.csv")),
                            error = function(e) NA),
  archive_csvs_n = length(list.files(here("data"),
                                     pattern = "^\\d{4}-\\d{2}-\\d{2}-geodienste-ch\\.csv$"))
)
saveRDS(meta, file.path(out_dir, "_metadata.rds"), version = 2)
message("Wrote ", file.path(out_dir, "_metadata.rds"))

message("\nGolden capture complete. ", length(list.files(out_dir)),
        " files written to ", out_dir)
