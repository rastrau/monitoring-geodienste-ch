# R/data_timeseries.R
#
# Per-(week, canton) historic aggregation feeding the time-series plots.
# Extracted from time-series.R.

suppressPackageStartupMessages({
  library(dplyr)
})

compute_timeseries <- function(df_hist_long) {
  df_hist_long %>%
    dplyr::group_by(updated, canton) %>%
    dplyr::summarise(
      number_available_datasets = dplyr::n(),
      mean_open_score = mean(open_score),
      .groups = "drop") %>%
    dplyr::arrange(updated)
}

# Post-openness filter that prepares the historic frame for time-series
# aggregation: drops missing/under-construction rows, WMS-only offerings,
# Liechtenstein, and the pre-stabilisation period before 2023-06-06.
filter_historic_for_timeseries <- function(df_hist_long_full) {
  df_hist_long_full %>%
    dplyr::filter(publication_type != "Keine Daten") %>%
    dplyr::filter(publication_type != "Im Aufbau") %>%
    dplyr::filter(offering != "WMS") %>%
    dplyr::filter(canton != "FL") %>%
    dplyr::filter(updated >= "2023-06-06")
}
