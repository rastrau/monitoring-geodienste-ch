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
  # Identify "federal-only" topics: ones for which Broker is the sole
  # publisher (data or WMS) while no canton publishes anything. These get
  # dropped from the cantonal analysis so cantons aren't penalised for
  # topics they aren't responsible for. Self-maintaining: if a canton
  # later starts publishing such a topic, it re-enters automatically.
  empty_values <- c("Keine Daten", "keine Daten", "")
  federal_only_topics <- df %>%
    filter(!str_detect(topic_title, "verwaltungsintern")) %>%
    group_by(topic_title) %>%
    summarise(
      broker_has_any = any(
        str_detect(canton, "Broker") &
          (!publication_data %in% empty_values |
           !publication_wms  %in% empty_values)
      ),
      cantons_have_any = any(
        !str_detect(canton, "Broker") &
          (!publication_data %in% empty_values |
           !publication_wms  %in% empty_values)
      ),
      .groups = "drop"
    ) %>%
    filter(broker_has_any & !cantons_have_any) %>%
    pull(topic_title)

  out <- df %>%
    filter(!str_detect(topic_title, "verwaltungsintern")) %>%
    filter(!topic_title %in% federal_only_topics) %>%
    filter(!str_detect(canton, "Broker")) %>%
    select(-version, -comment) %>%
    mutate(canton = factor(canton, levels = unique(canton))) %>%
    left_join(topic_shortnames, by = "topic_title") %>%
    mutate(topic_title_short = ifelse(is.na(topic_title_short),
                                      "unbekannt",
                                      topic_title_short)) %>%
    mutate(
      publication_data = ifelse(
        publication_data %in% c("Keine Daten", "keine Daten", ""),
        "Keine Daten",
        publication_data),
      publication_wms = ifelse(
        publication_wms %in% c("Keine Daten", "keine Daten", ""),
        "Keine Daten",
        publication_wms)
    ) %>%
    mutate(
      contract_required_data = replace_na(contract_required_data, FALSE),
      # bug-compat with functions.R:86 — should plausibly reference
      # contract_required_wms; preserved so equivalence holds.
      contract_required_wms  = replace_na(contract_required_data, FALSE)
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
