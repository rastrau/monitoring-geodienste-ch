# R/data_clean.R
#
# CSV-driven equivalent of functions.R:clean_data().
#
# Observable behaviour is identical to the legacy clean_data(), including:
#   - factor(canton, levels = unique(canton)) — the level order is taken
#     from the input row order so the time-series and current-data
#     pipelines retain the same factor structure they had before.
#   - the contract_required_wms <- contract_required_data assignment in
#     functions.R:86 (apparent copy/paste bug) is preserved verbatim
#     to keep equivalence; cleanup is tracked as a follow-up task.
#   - topics not found in the shortnames CSV are labelled "unbekannt".

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

clean_data_v2 <- function(df, topic_shortnames, valid_cantons = NULL) {
  out <- df %>%
    dplyr::filter(!stringr::str_detect(topic_title, "verwaltungsintern")) %>%
    dplyr::filter(!stringr::str_detect(canton, "Broker")) %>%
    dplyr::select(-version, -comment) %>%
    dplyr::mutate(canton = factor(canton, levels = unique(canton))) %>%
    dplyr::left_join(topic_shortnames, by = "topic_title") %>%
    dplyr::mutate(topic_title_short = ifelse(is.na(topic_title_short),
                                             "unbekannt",
                                             topic_title_short)) %>%
    dplyr::mutate(
      publication_data = ifelse(
        publication_data %in% c("Keine Daten", "keine Daten", ""),
        "Keine Daten",
        publication_data),
      publication_wms = ifelse(
        publication_wms %in% c("Keine Daten", "keine Daten", ""),
        "Keine Daten",
        publication_wms)
    ) %>%
    dplyr::mutate(
      contract_required_data = tidyr::replace_na(contract_required_data, FALSE),
      # bug-compat with functions.R:86 — should plausibly reference
      # contract_required_wms; preserved so equivalence holds.
      contract_required_wms  = tidyr::replace_na(contract_required_data, FALSE)
    )

  # Optional validation: warn if unexpected cantons appear. Does not change
  # the data — kept side-effecting like quality_assurance_after_import().
  if (!is.null(valid_cantons)) {
    seen <- as.character(unique(out$canton))
    unexpected <- setdiff(seen, valid_cantons)
    if (length(unexpected) > 0) {
      warning("Unexpected canton(s) in input: ",
              paste(unexpected, collapse = ", "))
    }
  }
  out
}
