# R/data_aggregate.R
#
# Per-canton aggregations: the wide canton/topic frame (df2 in analyse.R
# lines 38-102) and the canton-level summary used by the scatter plots.
# `sort_topics()` (from functions.R) is called to factor the publication
# types in the configured order.

suppressPackageStartupMessages({
  library(dplyr)
})

# Verbatim extract of analyse.R lines 38-102, made a pure function.
aggregate_per_canton <- function(df_long) {
  df2 <- df_long %>%
    dplyr::group_by(canton, offering, publication_type, open_score) %>%
    dplyr::summarise(
      count = dplyr::n(),
      topics = paste0(topic_title_short, collapse = ", "),
      .groups = "drop") %>%
    dplyr::mutate(
      open_score_wo_nd = ifelse(publication_type == "Keine Daten", NA, open_score),
      count_wo_nd = ifelse(publication_type == "Keine Daten", NA, count),
      open_score_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, open_score),
      count_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, count)
    ) %>%
    dplyr::group_by(canton, offering) %>%
    dplyr::mutate(
      proportion = count / sum(count, na.rm = TRUE),
      proportion_wo_nd = count_wo_nd / sum(count_wo_nd, na.rm = TRUE),
      proportion_wo_nduc = count_wo_nduc / sum(count_wo_nduc, na.rm = TRUE),
      open_score_canton = sum(open_score * proportion, na.rm = TRUE),
      open_score_wo_nd_canton = sum(open_score_wo_nd * proportion_wo_nd, na.rm = TRUE),
      open_score_wo_nduc_canton = sum(open_score_wo_nduc * proportion_wo_nduc, na.rm = TRUE)) %>%
    dplyr::select(canton, offering, publication_type, topics, open_score,
                  count, count_wo_nd, count_wo_nduc,
                  proportion, proportion_wo_nd, proportion_wo_nduc,
                  open_score_canton, open_score_wo_nd_canton, open_score_wo_nduc_canton)

  df2 <- sort_topics(df2, width = 30)

  missing_data_summary <- df2 %>%
    dplyr::filter(offering == "data download") %>%
    dplyr::group_by(canton) %>%
    dplyr::summarise(
      count_missing_canton =
        sum(count[publication_type %in% c("Keine Daten", "Im Aufbau")],
            na.rm = TRUE),
      count_nd_canton =
        sum(count[publication_type == "Keine Daten"], na.rm = TRUE),
      count_available_canton =
        sum(count[!publication_type %in% c("Keine Daten", "Im Aufbau")],
            na.rm = TRUE),
      .groups = "drop"
    )

  df2 <- df2 %>%
    dplyr::left_join(missing_data_summary, by = "canton") %>%
    dplyr::mutate(
      count_missing_canton = ifelse(is.na(count_missing_canton), 0, count_missing_canton),
      count_nd_canton = ifelse(is.na(count_nd_canton), 0, count_nd_canton),
      count_available_canton = ifelse(is.na(count_available_canton), 0, count_available_canton)
    )

  df2
}

# Canton-level roll-up (analyse.R lines 197-209). Drops FL because the
# downstream scatter plot excludes Liechtenstein for sample-size reasons.
summarise_per_canton <- function(df2) {
  df2 %>%
    dplyr::filter(offering == "data download") %>%
    dplyr::filter(!canton == "FL") %>%
    dplyr::group_by(canton) %>%
    dplyr::reframe(
      open_score_canton = open_score_canton,
      open_score_wo_nd_canton = open_score_wo_nd_canton,
      open_score_wo_nduc_canton = open_score_wo_nduc_canton,
      count_missing_canton = count_missing_canton,
      count_nd_canton = count_nd_canton,
      count_available_canton = count_available_canton
    ) %>%
    dplyr::distinct()
}
