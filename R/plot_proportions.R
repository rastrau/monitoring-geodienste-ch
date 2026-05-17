# R/plot_proportions.R
#
# Three stacked horizontal bar plots of per-canton publication-type
# proportions, with progressively narrower exclusions:
#   plot_prop_all      — all publication types
#   plot_prop_wo_nd    — excludes "Keine Daten"
#   plot_prop_wo_nduc  — excludes "Keine Daten" and "Im Aufbau"
#
# All three are wrapped in plotlyfy() at the target level (see _targets.R).
# The colour vector `cols` is passed explicitly to avoid the side-effecting
# reassignment that happens in analyse.R line 173.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(scales)
})

plot_prop_all <- function(df2, cols, theme_options) {
  df2 %>%
    dplyr::filter(offering == "data download") %>%
    ggplot2::ggplot(ggplot2::aes(
      proportion, reorder(canton, +open_score_canton),
      fill = publication_type,
      text = stringr::str_c("Kanton ", canton, "\n\n",
                            publication_type, ": ", round(100 * proportion, 1),
                            "% aller Datens\u00e4tze\n\n",
                            "Datens\u00e4tze in dieser Kategorie:\n", topics))) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(fill = "Zugriffskategorie", x = "Anteil der Datens\u00e4tze") +
    theme_options +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

plot_prop_wo_nd <- function(df2, cols, theme_options) {
  df2 %>%
    dplyr::filter(offering == "data download") %>%
    dplyr::filter(!publication_type == "Keine Daten") %>%
    ggplot2::ggplot(ggplot2::aes(
      proportion_wo_nd, reorder(canton, +open_score_wo_nd_canton),
      fill = publication_type,
      text = stringr::str_c("Kanton ", canton, "\n\n",
                            publication_type, ": ", round(100 * proportion_wo_nd, 1),
                            "% der untersuchten Datens\u00e4tze\n(ohne \"Keine Daten\")\n\n",
                            "Datens\u00e4tze in dieser Kategorie:\n", topics))) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(fill = "Zugriffskategorie", x = "Anteil der untersuchten Datens\u00e4tze") +
    theme_options +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}

plot_prop_wo_nduc <- function(df2, cols, theme_options) {
  df2 %>%
    dplyr::filter(offering == "data download") %>%
    dplyr::filter(!publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
    ggplot2::ggplot(ggplot2::aes(
      proportion_wo_nduc,
      reorder(canton, +(open_score_wo_nduc_canton * 1000 + count_available_canton)),
      fill = publication_type,
      text = stringr::str_c("Kanton ", canton, "\n\n",
                            publication_type, ": ", round(100 * proportion_wo_nduc, 1),
                            "% der untersuchten Datens\u00e4tze\n(ohne \"Keine Daten\" und ohne \"Im Aufbau\")\n\n",
                            "Datens\u00e4tze in dieser Kategorie:\n", topics))) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(fill = "Zugriffskategorie", x = "Anteil der untersuchten Datens\u00e4tze") +
    theme_options +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}
