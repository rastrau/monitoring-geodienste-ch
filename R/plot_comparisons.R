# R/plot_comparisons.R
#
# Scatter plot comparing per-canton openness against per-canton dataset
# count, with quadrant shading and labelled points. Jitter is RNG-driven
# — the caller (in _targets.R) seeds before invocation so the two
# variants (full + zoomed) share a single RNG stream and match the
# legacy sequential build order.
#
# build_comparison() (below) is the thin wrapper that computes axis
# limits and chooses jitter amplitudes. The inner ggplot builder
# plot_comparison() is copied verbatim from functions.R; the
# project-root functions.R copy is preserved untouched for the
# equivalence harness's scripts mode.

suppressPackageStartupMessages({
  library(ggplot2)
  library(stringr)
})

plot_comparison <- function(df_canton, theme_options, min_openness, med_openness, max_openness,
                            min_count, med_count, max_count,
                            jitter_horizontal, jitter_vertical) {

  ggplot(data = df_canton) +
    # Define rectangles for different categories
    geom_rect(aes(xmin = min_openness, xmax = med_openness,
                  ymin = min_count, ymax = med_count,
                  text = "unterdurchschnittliche Anzahl verf\u00fcgbarer Datens\u00e4tze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datens\u00e4tze"),
              fill = "#AABFDB") +
    geom_rect(aes(xmin = min_openness, xmax = med_openness,
                  ymin = med_count, ymax = max_count,
                  text = "\u00fcberdurchschnittliche Anzahl verf\u00fcgbarer Datens\u00e4tze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datens\u00e4tze"),
              fill = "#85CCA9") +
    geom_rect(aes(xmin = med_openness, xmax = max_openness,
                  ymin = min_count, ymax = med_count,
                  text = "unterdurchschnittliche Anzahl verf\u00fcgbarer Datens\u00e4tze,\n\u00fcberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datens\u00e4tze"),
              fill = "#85CCD2") +
    geom_rect(aes(xmin = med_openness, xmax = max_openness,
                  ymin = med_count, ymax = max_count,
                  text = "\u00fcberdurchschnittliche Anzahl verf\u00fcgbarer Datens\u00e4tze,\n\u00fcberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datens\u00e4tze"),
              fill = "#54B987") +
    geom_text(aes(x = open_score_wo_nduc_canton,
                  y = count_available_canton,
                  label = paste0(sprintf("<b>%s</b>", canton)),
                  text = str_c("Kanton ", canton, "\n\n",
                               "Offenheit: ", round(open_score_wo_nduc_canton, 2), "\n",
                               "Verf\u00fcgbare Datens\u00e4tze: ", count_available_canton)),
              size = 5,
              position = position_jitter(width = jitter_horizontal,
                                         height = jitter_vertical)) +
    scale_x_continuous(limits = c(min_openness, max_openness),
                       breaks = pretty(c(min_openness, max_openness), n = 5)) +
    scale_y_continuous(limits = c(min_count, max_count),
                       breaks = pretty(c(min_count + 0.5, max_count - 0.5), n = 5)) +

    labs(y = "Anzahl verf\u00fcgbarer Datens\u00e4tze",
         x = "Offenheit der verf\u00fcgbaren Datens\u00e4tze") +
    theme_options
}

build_comparison <- function(df_canton, theme_options, zoom = FALSE) {
  min_openness <- min(df_canton$open_score_wo_nduc_canton) - 0.15
  med_openness <- median(df_canton$open_score_wo_nduc_canton)
  max_openness <- 3 + 0.15
  min_count    <- min(df_canton$count_available_canton) - 0.5
  med_count    <- median(df_canton$count_available_canton)
  max_count    <- max(df_canton$count_available_canton) + 0.5

  if (zoom) {
    # Mirrors analyse.R lines 230-232.
    min_openness <- med_openness - (3.01 - med_openness)
    max_openness <- 3.01
    min_count    <- med_count - (max_count - med_count)
    jh <- 0.01
  } else {
    jh <- 0.05
  }

  plt <- plot_comparison(df_canton, theme_options,
                         min_openness, med_openness, max_openness,
                         min_count, med_count, max_count, jh, 0.2)
  plotlyfy_w_zoom(plt)
}
