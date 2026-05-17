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
    mutate(
      temp_order_nd = count_missing_canton * 1000 + count_nd_canton,
      publication_type = factor(publication_type, factor_levels_publication_missing)
    )
  df2_missing %>%
    filter(!canton == "FL") %>%
    filter(offering == "data download") %>%
    filter(publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
    ggplot(aes(
      count, reorder(canton, -temp_order_nd),
      fill = publication_type,
      text = str_c("Kanton ", canton, "\n\n",
                   publication_type, ": ", count,
                   " Themen\n\n",
                   "Themen in dieser Kategorie:\n", topics))) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = cols_missing) +
    labs(fill = "Datenverf\u00fcgbarkeit",
         x = "Anzahl nicht vorhandener Datens\u00e4tze") +
    theme_options +
    theme(axis.title.y = element_blank())
}
