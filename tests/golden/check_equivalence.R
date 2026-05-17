# tests/golden/check_equivalence.R
#
# Compare artefacts produced by a NEW pipeline against the golden artefacts
# captured from the current pipeline.
#
# Intended workflow (once a {targets} pipeline exists):
#   1. tar_make()
#   2. Rscript tests/golden/check_equivalence.R
#
# This script is intentionally tolerant of:
#   - row order differences (frames are re-sorted on a stable key)
#   - tiny floating-point noise (default tolerance 1e-10)
# but strict about column sets, types and values.
#
# To plug in a new pipeline, edit `load_candidate()` below to return a named
# list of objects with the same names as the golden artefacts.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
})

golden_dir <- here("tests", "golden", "artefacts")
if (!dir.exists(golden_dir)) {
  stop("No golden artefacts found at ", golden_dir,
       "\nRun `Rscript tests/golden/capture_golden.R` on the current pipeline first.")
}

# ---- Load candidates from the NEW pipeline ----------------------------------
# Replace the body of this function once {targets} is in place. Example:
#
#   load_candidate <- function() {
#     targets::tar_load_everything()
#     list(
#       df_current_cleaned    = current_clean,
#       df2_canton_aggregates = canton_aggregates,
#       df_canton_summary     = canton_summary,
#       df_changes            = df_changes,
#       df_timeseries         = timeseries,
#       plt_data_prop_all       = plt_data_prop_all,
#       plt_data_prop_wo_nd     = plt_data_prop_wo_nd,
#       plt_data_prop_wo_nduc   = plt_data_prop_wo_nduc,
#       plt_data_missingdata    = plt_data_missingdata,
#       plt_data_comparison     = plt_data_comparison,
#       plt_data_comparison_subset = plt_data_comparison_subset,
#       lineplot_number         = lineplot_number,
#       lineplot_open_score     = lineplot_open_score
#     )
#   }
#
# Two candidate loaders are provided:
#
#   load_candidate_scripts() — sources the existing analyse.R / time-series.R.
#     This is a self-check: a freshly-captured golden set must equal itself.
#
#   load_candidate_targets() — reads artefacts from the {targets} pipeline
#     (after `targets::tar_make()`). This is the real refactor check.
#
# Select which one runs by setting the environment variable
# GEODIENSTE_CHECK=scripts|targets (default: targets).

load_candidate_scripts <- function() {
  set.seed(20240101L)
  source(here("analyse.R"),     encoding = "UTF-8")
  source(here("time-series.R"), encoding = "UTF-8")
  list(
    df_current_cleaned         = df_current_cleaned,
    df2_canton_aggregates      = df2,
    df_canton_summary          = df_canton,
    df_changes                 = df_changes,
    df_timeseries              = df_timeseries,
    plt_data_prop_all          = plt_data_prop_all,
    plt_data_prop_wo_nd        = plt_data_prop_wo_nd,
    plt_data_prop_wo_nduc      = plt_data_prop_wo_nduc,
    plt_data_missingdata       = plt_data_missingdata,
    plt_data_comparison        = plt_data_comparison,
    plt_data_comparison_subset = plt_data_comparison_subset,
    lineplot_number            = lineplot_number,
    lineplot_open_score        = lineplot_open_score
  )
}

load_candidate_targets <- function() {
  stopifnot(requireNamespace("targets", quietly = TRUE))
  tar_read <- targets::tar_read
  list(
    df_current_cleaned         = tar_read(current_clean),
    df2_canton_aggregates      = tar_read(canton_aggregates),
    df_canton_summary          = tar_read(canton_summary),
    df_changes                 = tar_read(df_changes),
    df_timeseries              = tar_read(timeseries),
    plt_data_prop_all          = tar_read(plt_data_prop_all),
    plt_data_prop_wo_nd        = tar_read(plt_data_prop_wo_nd),
    plt_data_prop_wo_nduc      = tar_read(plt_data_prop_wo_nduc),
    plt_data_missingdata       = tar_read(plt_data_missingdata),
    plt_data_comparison        = tar_read(plt_data_comparison),
    plt_data_comparison_subset = tar_read(plt_data_comparison_subset),
    lineplot_number            = tar_read(lineplot_number),
    lineplot_open_score        = tar_read(lineplot_open_score)
  )
}

load_candidate <- function() {
  mode <- Sys.getenv("GEODIENSTE_CHECK", "targets")
  switch(mode,
         scripts = load_candidate_scripts(),
         targets = load_candidate_targets(),
         stop("Unknown GEODIENSTE_CHECK mode: ", mode))
}

# ---- Comparison helpers ------------------------------------------------------

# Stable re-sort to neutralise row-order changes.
stable_sort <- function(df, sort_cols) {
  df <- dplyr::ungroup(df)
  df <- tibble::as_tibble(df)
  sort_cols <- intersect(sort_cols, names(df))
  if (length(sort_cols) > 0) {
    df <- df[do.call(order, lapply(sort_cols, function(c) df[[c]])), ,
             drop = FALSE]
    rownames(df) <- NULL
  }
  df
}

compare_df <- function(name, candidate, sort_cols, tolerance = 1e-10) {
  golden_path <- file.path(golden_dir, paste0(name, ".rds"))
  if (!file.exists(golden_path)) {
    return(list(name = name, status = "missing-golden", details = golden_path))
  }
  golden    <- readRDS(golden_path)
  candidate <- stable_sort(candidate, sort_cols)
  golden    <- stable_sort(golden, sort_cols)

  if (requireNamespace("waldo", quietly = TRUE)) {
    diff <- waldo::compare(golden, candidate, tolerance = tolerance)
    ok <- length(diff) == 0
    return(list(name = name,
                status = if (ok) "ok" else "DIFFER",
                details = if (ok) "" else paste(format(diff), collapse = "\n")))
  }
  # Fallback if waldo is not installed.
  ae <- all.equal(golden, candidate, tolerance = tolerance)
  ok <- isTRUE(ae)
  list(name = name,
       status = if (ok) "ok" else "DIFFER",
       details = if (ok) "" else paste(as.character(ae), collapse = "\n"))
}

snapshot_plot_data_for_compare <- function(plt, seed = 1L) {
  # Match capture_golden.R: seed RNG so that position_jitter() etc. produce
  # deterministic per-layer coordinates.
  set.seed(seed)
  if (inherits(plt, "plotly")) {
    list(plotly_data = plt$x$data)
  } else {
    layer_dfs <- tryCatch(
      lapply(seq_along(plt$layers), function(i) ggplot2::layer_data(plt, i)),
      error = function(e) list(error = conditionMessage(e))
    )
    list(plot_data  = tibble::as_tibble(plt$data),
         layer_data = layer_dfs)
  }
}

compare_plot <- function(name, candidate_plt, tolerance = 1e-10) {
  golden_path <- file.path(golden_dir, paste0(name, ".rds"))
  if (!file.exists(golden_path)) {
    return(list(name = name, status = "missing-golden", details = golden_path))
  }
  golden    <- readRDS(golden_path)
  candidate <- snapshot_plot_data_for_compare(candidate_plt)

  if (requireNamespace("waldo", quietly = TRUE)) {
    diff <- waldo::compare(golden, candidate, tolerance = tolerance)
    ok <- length(diff) == 0
    return(list(name = name,
                status = if (ok) "ok" else "DIFFER",
                details = if (ok) "" else paste(format(diff), collapse = "\n")))
  }
  ae <- all.equal(golden, candidate, tolerance = tolerance)
  ok <- isTRUE(ae)
  list(name = name,
       status = if (ok) "ok" else "DIFFER",
       details = if (ok) "" else paste(as.character(ae), collapse = "\n"))
}

# ---- Run comparisons ---------------------------------------------------------

cand <- load_candidate()

results <- list(
  compare_df  ("df_current_cleaned",         cand$df_current_cleaned,
               sort_cols = c("canton", "topic_title")),
  compare_df  ("df2_canton_aggregates",      cand$df2_canton_aggregates,
               sort_cols = c("canton", "offering", "publication_type")),
  compare_df  ("df_canton_summary",          cand$df_canton_summary,
               sort_cols = c("canton")),
  compare_df  ("df_changes",                 cand$df_changes,
               sort_cols = c("Kanton", "Datensatz")),
  compare_df  ("df_timeseries",              cand$df_timeseries,
               sort_cols = c("updated", "canton")),
  compare_plot("plt_data_prop_all",          cand$plt_data_prop_all),
  compare_plot("plt_data_prop_wo_nd",        cand$plt_data_prop_wo_nd),
  compare_plot("plt_data_prop_wo_nduc",      cand$plt_data_prop_wo_nduc),
  compare_plot("plt_data_missingdata",       cand$plt_data_missingdata),
  compare_plot("plt_data_comparison",        cand$plt_data_comparison),
  compare_plot("plt_data_comparison_subset", cand$plt_data_comparison_subset),
  compare_plot("lineplot_number",            cand$lineplot_number),
  compare_plot("lineplot_open_score",        cand$lineplot_open_score)
)

# ---- Report -----------------------------------------------------------------

cat("\nEquivalence check results:\n")
cat(strrep("-", 60), "\n", sep = "")
for (r in results) {
  cat(sprintf("  %-30s  %s\n", r$name, r$status))
}
cat(strrep("-", 60), "\n", sep = "")

failing <- Filter(function(r) r$status != "ok", results)
if (length(failing) > 0) {
  cat("\nDetails for non-OK artefacts:\n")
  for (r in failing) {
    cat("\n### ", r$name, " (", r$status, ")\n", sep = "")
    cat(r$details, "\n", sep = "")
  }
  quit(status = 1)
} else {
  cat("\nAll artefacts match the golden reference.\n")
}
