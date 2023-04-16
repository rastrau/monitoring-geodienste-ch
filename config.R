library(here)
library(ggplot2)

csv_path <- here("data/geodienste-ch.csv")

factor_levels_publication <- c("Frei erhältlich", "Frei erhältlich, mit Vertrag",
                               "Registrierung erforderlich",
                               "Registrierung erforderlich, mit Vertrag", "Freigabe erforderlich",
                               "Freigabe erforderlich, mit Vertrag", "Im Aufbau", "Keine Daten")

factor_levels_publication_missing <- c("Keine Daten", "Im Aufbau")

cols <- c("#54B987", "#85CCA9", "#4575B4", "#91BFDB", "#FF9F73", "#E66244", "#B9B9B9", "#9F9F9F")

theme_options <-
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white", colour = "white"))

plotly_buttons_to_remove <- c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                              "autoScale2d", "resetScale2d", "hoverClosestCartesian",
                              "hoverCompareCartesian", "handleCartesian", "toggleSpikelines")

plotly_w_zoom_buttons_to_remove <- c("select2d", "lasso2d", "hoverClosestCartesian", "zoomIn2d",
                                     "zoomOut2d", "hoverCompareCartesian", "handleCartesian",
                                     "toggleSpikelines")
