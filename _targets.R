# _targets.R
#
# {targets} pipeline for the geodienste-ch analysis.
#
# Step 2 of the refactor wraps the existing analysis without changing any
# behaviour. Step 3 replaces the inline case_when()s in functions.R with
# CSV-driven joins (clean_data_v2 / compute_openness_per_topic_v2 in
# R/pipeline.R), keeping outputs bit-for-bit identical. Verified via
# tests/golden/check_equivalence.R.
#
# Run with:
#   LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 Rscript -e 'targets::tar_make()'
#
# Inspect with:
#   Rscript -e 'targets::tar_visnetwork()'
#   Rscript -e 'targets::tar_read(canton_aggregates)'

library(targets)
library(tarchetypes)
library(here)

# Source the existing analysis functions (unit functions in functions.R) and
# the constants in config.R. These files are intentionally not modified at
# this step.
source(here("config.R"),    encoding = "UTF-8")
source(here("functions.R"), encoding = "UTF-8")

# Orchestration helpers (new, in R/).
source(here("R", "pipeline.R"), encoding = "UTF-8")

tar_option_set(
  packages = c("here", "dplyr", "tidyr", "stringr", "readr", "ggplot2",
               "plotly", "purrr", "scales"),
  format   = "rds"
)

list(
  # ---- Inputs (file-tracked) -------------------------------------------------
  tar_target(current_csv_file,
             here("data", "geodienste-ch.csv"),
             format = "file"),
  tar_target(archive_csv_files_asc,
             list_archive_csvs(here("data"), decreasing = FALSE),
             format = "file"),
  tar_target(archive_csv_files_desc,
             list_archive_csvs(here("data"), decreasing = TRUE),
             format = "file"),

  # ---- Reference data (file-tracked CSVs replacing inline case_when()s) ----
  tar_target(topic_shortnames_file,
             here("data", "reference", "topic_shortnames.csv"),
             format = "file"),
  tar_target(openness_scores_file,
             here("data", "reference", "openness_scores.csv"),
             format = "file"),
  tar_target(cantons_file,
             here("data", "reference", "cantons.csv"),
             format = "file"),

  tar_target(topic_shortnames, read_topic_shortnames(topic_shortnames_file)),
  tar_target(openness_scores,  read_openness_scores(openness_scores_file)),
  tar_target(cantons,          read_cantons(cantons_file)),

  # ---- Current dataset ------------------------------------------------------
  # clean_current_v2() / long_with_openness_v2() use the reference CSVs
  # above instead of the hard-coded case_when()s in functions.R.
  tar_target(current_clean,
             clean_current_v2(current_csv_file, topic_shortnames, cantons)),
  tar_target(long_data,
             long_with_openness_v2(current_clean, openness_scores,
                                   factor_levels_publication)),
  tar_target(canton_aggregates,    aggregate_per_canton(long_data)),
  tar_target(canton_summary,       summarise_per_canton(canton_aggregates)),

  # ---- Change detection (4-weeks-ago) ---------------------------------------
  tar_target(recent_csv_file,
             pick_recent_csv(archive_csv_files_desc),
             format = "file"),
  tar_target(recent_clean,
             clean_recent_v2(recent_csv_file, topic_shortnames, cantons)),
  tar_target(df_changes,           compute_changes(current_clean, recent_clean)),

  # ---- Time series ----------------------------------------------------------
  # Uses the ASCENDING file order, matching time-series.R's list.files()
  # default; this affects the canton factor levels via clean_data().
  tar_target(historic_long,
             build_historic_long_v2(archive_csv_files_asc, topic_shortnames,
                                    openness_scores, factor_levels_publication,
                                    cantons)),
  tar_target(timeseries,           compute_timeseries(historic_long)),

  # ---- Plots ---------------------------------------------------------------
  # The colour vector for the proportion plots is the one defined in config.R
  # (`cols`); the missing-data plot uses its own greyscale palette internally,
  # so we no longer overwrite the global `cols` mid-pipeline as analyse.R does.
  # Each ggplot is wrapped in plotlyfy() to match the script pipeline's
  # final outputs (which are plotly widgets consumed by index.qmd).
  tar_target(plt_data_prop_all,
             plotlyfy(plot_prop_all(canton_aggregates, cols, theme_options))),
  tar_target(plt_data_prop_wo_nd,
             plotlyfy(plot_prop_wo_nd(canton_aggregates, cols, theme_options))),
  tar_target(plt_data_prop_wo_nduc,
             plotlyfy(plot_prop_wo_nduc(canton_aggregates, cols, theme_options))),
  tar_target(plt_data_missingdata,
             plotlyfy(plot_missingdata(canton_aggregates, theme_options,
                                       factor_levels_publication_missing))),

  # Comparison plots use position_jitter(). The two plots share a single
  # RNG stream (seeded once) so that the second plot consumes random numbers
  # after the first — matching the sequential build order in the legacy
  # script pipeline. We compute them in one target and split into two.
  tar_target(comparison_plots, {
    set.seed(20240101L)
    full   <- build_comparison(canton_summary, theme_options, zoom = FALSE)
    subset <- build_comparison(canton_summary, theme_options, zoom = TRUE)
    list(full = full, subset = subset)
  }),
  tar_target(plt_data_comparison,        comparison_plots$full),
  tar_target(plt_data_comparison_subset, comparison_plots$subset),

  tar_target(lineplot_number,
             plot_timeseries_number(timeseries, theme_options)),
  tar_target(lineplot_open_score,
             plot_timeseries_openness(timeseries, theme_options)),

  # ---- Convenience scalars (used by the Quarto report) ----------------------
  tar_target(updated_string,
             format(min(current_clean$updated), "%d.%m.%Y"))
)
