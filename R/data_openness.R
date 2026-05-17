# R/data_openness.R
#
# CSV-driven equivalent of functions.R:compute_openness_per_topic().
#
# - Adds ", mit Vertrag" suffix to publication_type when a contract is
#   required and the type is not "Im Aufbau" (matches functions.R:142-146).
# - Joins openness scores from the reference CSV instead of an inline
#   case_when().
# - Factorises publication_type using the levels in config.R
#   (factor_levels_publication), preserved as the function argument.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

compute_openness_per_topic_v2 <- function(df, openness_scores,
                                          factor_levels_publication) {
  df %>%
    dplyr::mutate(
      publication_type = ifelse(
        contract_required == TRUE & publication_type != "Im Aufbau",
        stringr::str_c(publication_type, ", mit Vertrag"),
        publication_type)
    ) %>%
    dplyr::left_join(openness_scores, by = "publication_type") %>%
    dplyr::mutate(
      publication_type = factor(publication_type, factor_levels_publication)
    ) %>%
    dplyr::select(canton, topic_title, topic_title_short, updated, offering,
                  publication_type, open_score)
}
