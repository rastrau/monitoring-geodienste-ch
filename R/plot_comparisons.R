# R/plot_comparisons.R
#
# Scatter plot comparing per-canton openness against per-canton dataset
# count, with quadrant shading and labelled points. Jitter is RNG-driven
# — the caller (in _targets.R) seeds before invocation so the two
# variants (full + zoomed) share a single RNG stream and match the
# legacy sequential build order.

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
