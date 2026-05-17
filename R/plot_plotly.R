# R/plot_plotly.R
#
# Conversion helpers that turn ggplot objects into the plotly widgets
# embedded in the Quarto report.
#
# Copied verbatim from functions.R. The project-root copy is preserved
# untouched so the legacy scripts (analyse.R, time-series.R), used by
# the equivalence harness's scripts mode, keep working.

suppressPackageStartupMessages({
  library(plotly)
})

plotlyfy <- function(plt){
  ggplotly(plt, tooltip = c("text")) %>%
    config(locale = "de-ch",
           modeBarButtonsToRemove = plotly_buttons_to_remove,
           displaylogo = FALSE) %>%
    layout(dragmode = FALSE)
}

plotlyfy_w_zoom <- function(plt){
  ggplotly(plt, tooltip = c("text")) %>%
    config(locale = "de-ch",
           modeBarButtonsToRemove = plotly_w_zoom_buttons_to_remove,
           displaylogo = FALSE,
           displayModeBar = TRUE) %>%
    layout(dragmode = FALSE)
}
