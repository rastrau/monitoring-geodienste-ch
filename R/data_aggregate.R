# R/data_aggregate.R
#
# Per-canton aggregations: the wide canton/topic frame (df2 in analyse.R
# lines 38-102) and the canton-level summary used by the scatter plots.
# Also includes `sort_topics()`, the small helper that alphabetises and
# wraps the concatenated topic labels for plot tooltips.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# Copied verbatim from functions.R::sort_topics. The project-root copy
# is kept untouched for the legacy scripts.
sort_topics <- function(df, width = 30) {
  df <- df %>%
    mutate(
      topics = str_split(topics, ", ") %>%     # Split the topics into a list
        lapply(sort) %>%                       # Sort each list element
        sapply(paste, collapse = ", ")         # Collapse back into a string
    ) %>%
    mutate(topics = str_wrap(topics, width = width)) # Wrap text for better readability

  return(df)
}

# Verbatim extract of analyse.R lines 38-102, made a pure function.
aggregate_per_canton <- function(df_long) {
  df2 <- df_long %>%
    group_by(canton, offering, publication_type, open_score) %>%
    summarise(
      count = n(),
      topics = paste0(topic_title_short, collapse = ", "),
      .groups = "drop") %>%
    mutate(
      open_score_wo_nd = ifelse(publication_type == "Keine Daten", NA, open_score),
      count_wo_nd = ifelse(publication_type == "Keine Daten", NA, count),
      open_score_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, open_score),
      count_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, count)
    ) %>%
    group_by(canton, offering) %>%
    mutate(
      proportion = count / sum(count, na.rm = TRUE),
      proportion_wo_nd = count_wo_nd / sum(count_wo_nd, na.rm = TRUE),
      proportion_wo_nduc = count_wo_nduc / sum(count_wo_nduc, na.rm = TRUE),
      open_score_canton = sum(open_score * proportion, na.rm = TRUE),
      open_score_wo_nd_canton = sum(open_score_wo_nd * proportion_wo_nd, na.rm = TRUE),
      open_score_wo_nduc_canton = sum(open_score_wo_nduc * proportion_wo_nduc, na.rm = TRUE)) %>%
    select(canton, offering, publication_type, topics, open_score,
           count, count_wo_nd, count_wo_nduc,
           proportion, proportion_wo_nd, proportion_wo_nduc,
           open_score_canton, open_score_wo_nd_canton, open_score_wo_nduc_canton)

  df2 <- sort_topics(df2, width = 30)

  missing_data_summary <- df2 %>%
    filter(offering == "data download") %>%
    group_by(canton) %>%
    summarise(
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
    left_join(missing_data_summary, by = "canton") %>%
    mutate(
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
    filter(offering == "data download") %>%
    filter(!canton == "FL") %>%
    group_by(canton) %>%
    reframe(
      open_score_canton = open_score_canton,
      open_score_wo_nd_canton = open_score_wo_nd_canton,
      open_score_wo_nduc_canton = open_score_wo_nduc_canton,
      count_missing_canton = count_missing_canton,
      count_nd_canton = count_nd_canton,
      count_available_canton = count_available_canton
    ) %>%
    distinct()
}
