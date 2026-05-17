# R/plot_timeseries.R
#
# Faceted line plots of (a) the per-canton count of available datasets and
# (b) the per-canton mean openness score, over time. facet_theme() is the
# shared theme helper for both plots; kept local since these are its only
# callers.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

facet_theme <- function(theme_options) {
  theme_options +
    theme(
      panel.grid.major.y = element_line(color = "#DDDDDD", linewidth = 0.2),
      panel.grid.minor.y = element_line(color = "#DDDDDD", linewidth = 0.1),
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.2),
      axis.text.x = element_text(angle = 55, hjust = 1),
      strip.text = element_text(face = "bold")
    )
}

plot_timeseries_number <- function(df_ts, theme_options) {
  ggplot(df_ts, aes(x = updated, y = number_available_datasets)) +
    geom_line(color = "#4575B4", linewidth = 1) +
    labs(x = "Datum", y = "Anzahl verf\u00fcgbarer Datens\u00e4tze\n") +
    scale_y_continuous(breaks = breaks_width(5)) +
    facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}

plot_timeseries_openness <- function(df_ts, theme_options) {
  ggplot(df_ts, aes(x = updated, y = mean_open_score)) +
    geom_line(color = "#54B987", linewidth = 1) +
    labs(x = "Datum", y = "Offenheit der verf\u00fcgbaren Datens\u00e4tze\n") +
    facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}
