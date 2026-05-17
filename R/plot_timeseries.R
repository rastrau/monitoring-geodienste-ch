# R/plot_timeseries.R
#
# Faceted line plots of (a) the per-canton count of available datasets and
# (b) the per-canton mean openness score, over time. facet_theme() is the
# shared theme helper for both plots; kept local since these are its only
# callers.

suppressPackageStartupMessages({
  library(ggplot2)
})

facet_theme <- function(theme_options) {
  theme_options +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.2),
      panel.grid.minor.y = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.1),
      panel.grid.major.x = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.2),
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

plot_timeseries_number <- function(df_ts, theme_options) {
  ggplot2::ggplot(df_ts, ggplot2::aes(x = updated, y = number_available_datasets)) +
    ggplot2::geom_line(color = "#4575B4", linewidth = 1) +
    ggplot2::labs(x = "Datum", y = "Anzahl verf\u00fcgbarer Datens\u00e4tze\n") +
    ggplot2::facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}

plot_timeseries_openness <- function(df_ts, theme_options) {
  ggplot2::ggplot(df_ts, ggplot2::aes(x = updated, y = mean_open_score)) +
    ggplot2::geom_line(color = "#54B987", linewidth = 1) +
    ggplot2::labs(x = "Datum", y = "Offenheit der verf\u00fcgbaren Datens\u00e4tze\n") +
    ggplot2::facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}
