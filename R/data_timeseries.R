# R/data_timeseries.R
#
# Per-(week, canton) historic aggregation feeding the time-series plots.
# Extracted from time-series.R.

suppressPackageStartupMessages({
  library(dplyr)
})

compute_timeseries <- function(df_hist_long) {
  df_hist_long %>%
    group_by(updated, canton) %>%
    summarise(
      number_available_datasets = n(),
      mean_open_score = mean(open_score),
      .groups = "drop") %>%
    arrange(updated)
}

# Post-openness filter that prepares the historic frame for time-series
# aggregation: drops missing/under-construction rows, WMS-only offerings,
# Liechtenstein, and the pre-stabilisation period before 2023-06-06.
filter_historic_for_timeseries <- function(df_hist_long_full) {
  df_hist_long_full %>%
    filter(!publication_type %in% publication_types_unavailable) %>%
    filter(offering != "WMS") %>%
    filter(canton != "FL") %>%
    filter(updated >= "2023-06-06")
}
