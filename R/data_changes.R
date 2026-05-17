# R/data_changes.R
#
# Week-over-week (4-weeks-ago) change detection. Diffs the current cleaned
# frame against the recent cleaned frame and renders the result with the
# German column names the Quarto report expects.
#
# Extracted from analyse.R lines 256-278.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

compute_changes <- function(df_current_cleaned, df_recent) {
  date_recent <- format(min(df_recent$updated), "%d.%m.%Y")
  df_changes <- df_current_cleaned %>%
    inner_join(df_recent,
               by = c("canton", "topic_title"),
               suffix = c("_current", "_recent")) %>%
    mutate(
      publication_data_current = ifelse(
        contract_required_data_current == TRUE,
        str_c(publication_data_current, ", mit Vertrag"),
        publication_data_current),
      publication_data_recent = ifelse(
        contract_required_data_recent == TRUE,
        str_c(publication_data_recent, ", mit Vertrag"),
        publication_data_recent)) %>%
    filter(publication_data_current != publication_data_recent) %>%
    select(canton, topic_title, publication_data_recent, publication_data_current)

  names(df_changes) <- c("Kanton", "Datensatz",
                         str_c("Zugriffsregelung am ", date_recent),
                         "Zugriffsregelung neu")
  arrange(df_changes, Kanton, Datensatz)
}
