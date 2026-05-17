# _targets.R
#
# {targets} pipeline for the geodienste-ch analysis.
#
# Orchestration helpers live in R/ and are auto-sourced by tar_source().
# The data flow per dataset is:
#
#   csv file --> raw --> cleaned --> harmonised --> with-openness
#
# downstream of which the aggregations, change detection, time-series and
# plots branch off. clean_data_v2 + compute_openness_per_topic_v2 are
# CSV-driven equivalents of the inline case_when()s in functions.R.
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
# the constants in config.R. These files are intentionally not modified.
source(here("config.R"),    encoding = "UTF-8")
source(here("functions.R"), encoding = "UTF-8")

# Auto-source every R/*.R helper file. Splitting R/pipeline.R into
# concern-oriented files (data_io.R, data_clean.R, plot_proportions.R, ...)
# is purely organisational; tar_source() handles it transparently.
tar_source(files = here("R"))

tar_option_set(
  packages = c("here", "dplyr", "tidyr", "stringr", "readr", "ggplot2",
               "plotly", "purrr", "scales"),
  format   = "rds"
)

list(
  # ---- File inputs ----------------------------------------------------------
  tar_target(current_csv_file,
             here("data", "geodienste-ch.csv"),
             format = "file"),
  tar_target(archive_csv_files_asc,
             list_archive_csvs(here("data"), decreasing = FALSE),
             format = "file"),
  tar_target(archive_csv_files_desc,
             list_archive_csvs(here("data"), decreasing = TRUE),
             format = "file"),

  # ---- Reference data (CSV-driven replacements for inline case_when()s) ----
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

  # ---- Current dataset chain ------------------------------------------------
  # csv -> raw -> clean -> harmonised -> long (with openness)
  tar_target(current_raw,
             read_geodienste_csv(current_csv_file)),
  tar_target(current_clean,
             clean_data_v2(current_raw, topic_shortnames, cantons)),
  tar_target(current_harmonised,
             harmonise_data_and_wms_atts(current_clean)),
  tar_target(current_long,
             compute_openness_per_topic_v2(current_harmonised, openness_scores,
                                           factor_levels_publication)),

  tar_target(canton_aggregates,    aggregate_per_canton(current_long)),
  tar_target(canton_summary,       summarise_per_canton(canton_aggregates)),

  # ---- Recent (4-weeks-ago) dataset chain -----------------------------------
  # Only needs raw + clean; change detection compares cleaned frames.
  tar_target(recent_csv_file,
             pick_recent_csv(archive_csv_files_desc),
             format = "file"),
  tar_target(recent_raw,
             read_geodienste_csv(recent_csv_file)),
  tar_target(recent_clean,
             clean_data_v2(recent_raw, topic_shortnames, cantons)),

  tar_target(df_changes,           compute_changes(current_clean, recent_clean)),

  # ---- Historic dataset chain (time-series) ---------------------------------
  # Uses the ASCENDING file order, matching time-series.R's list.files()
  # default; this affects the canton factor levels via clean_data_v2().
  tar_target(historic_raw,
             purrr::map_df(archive_csv_files_asc, read_geodienste_csv)),
  tar_target(historic_clean,
             clean_data_v2(historic_raw, topic_shortnames, cantons)),
  tar_target(historic_harmonised,
             harmonise_data_and_wms_atts(historic_clean)),
  tar_target(historic_long_full,
             compute_openness_per_topic_v2(historic_harmonised, openness_scores,
                                           factor_levels_publication)),
  tar_target(historic_long,
             filter_historic_for_timeseries(historic_long_full)),

  tar_target(timeseries,           compute_timeseries(historic_long)),

  # ---- Plots ---------------------------------------------------------------
  # Each proportion ggplot is wrapped in plotlyfy() to match the script
  # pipeline's final outputs (which are plotly widgets consumed by index.qmd).
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
             format(min(current_clean$updated), "%d.%m.%Y")),
  tar_target(date_recent_string,
             format(min(recent_clean$updated), "%d.%m.%Y"))
)
