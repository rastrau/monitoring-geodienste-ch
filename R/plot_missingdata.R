# R/plot_missingdata.R
#
# Stacked bar plot of "absence of data": for each canton, how many
# topics are flagged "Keine Daten" or "Im Aufbau". Uses its own
# greyscale palette so the global `cols` from config.R is not mutated
# (analyse.R line 173 used to do this).

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

plot_missingdata <- function(df2, theme_options,
                             factor_levels_publication_missing) {
  cols_missing <- c("#9F9F9F", "#B9B9B9")
  df2_missing <- df2 %>%
    dplyr::mutate(
      temp_order_nd = count_missing_canton * 1000 + count_nd_canton,
      publication_type = factor(publication_type, factor_levels_publication_missing)
    )
  df2_missing %>%
    dplyr::filter(!canton == "FL") %>%
    dplyr::filter(offering == "data download") %>%
    dplyr::filter(publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
    ggplot2::ggplot(ggplot2::aes(
      count, reorder(canton, -temp_order_nd),
      fill = publication_type,
      text = stringr::str_c("Kanton ", canton, "\n\n",
                            publication_type, ": ", count,
                            " Themen\n\n",
                            "Themen in dieser Kategorie:\n", topics))) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = cols_missing) +
    ggplot2::labs(fill = "Datenverf\u00fcgbarkeit",
                  x = "Anzahl nicht vorhandener Datens\u00e4tze") +
    theme_options +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}
