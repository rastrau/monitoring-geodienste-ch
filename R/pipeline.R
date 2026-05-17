# R/pipeline.R
#
# Thin orchestration helpers used by _targets.R.
#
# Step 2 of the refactor: this file does NOT reimplement any analysis. It just
# wraps the existing scripts (config.R, functions.R) so that their unit
# functions can be called from individual targets. The function bodies below
# are direct extracts of script-level code from analyse.R and time-series.R,
# rewritten as pure functions of their inputs. No analytical behaviour
# changes; this is verified by tests/golden/check_equivalence.R.
#
# All non-ASCII text is preserved verbatim (German umlauts in labels,
# strings that appear in the original scripts).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(plotly)
})

# ---- Inputs ------------------------------------------------------------------

# Read a geodienste-ch CSV with the same parsing options as the existing
# scripts (analyse.R:16, time-series.R:12).
read_geodienste_csv <- function(path) {
  readr::read_delim(path, delim = ";", na = c("{}", "''", '""', ""),
                    show_col_types = FALSE)
}

# List archival CSVs in data/.
#
# Two distinct orderings exist in the legacy code:
#   - time-series.R uses list.files() default (ascending), and the canton
#     factor levels in the time-series outputs depend on this order via
#     clean_data()'s `factor(canton, levels = unique(canton))`.
#   - analyse.R sorts descending and picks the 5th element for the
#     4-weeks-ago comparison.
# Both orderings are preserved exactly to keep behaviour identical.
list_archive_csvs <- function(data_dir = here::here("data"),
                              decreasing = FALSE) {
  files <- list.files(path = data_dir,
                      pattern = "^\\d{4}-\\d{2}-\\d{2}-geodienste-ch\\.csv$",
                      full.names = TRUE)
  stringr::str_sort(files, decreasing = decreasing)
}

# Pick the 4-weeks-ago CSV by position, exactly as analyse.R:249 does. Expects
# a descending-sorted file list. The selection rule is intentionally
# preserved at this step; revisiting it (date-based selection) is a
# separate, behaviour-changing step.
pick_recent_csv <- function(csv_files_desc) {
  csv_files_desc[5]
}

# ---- Per-canton aggregation (df2 in analyse.R) -------------------------------
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

# ---- Canton-level summary (df_canton in analyse.R lines 197-209) -------------

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

# ---- Change detection (analyse.R lines 256-278) ------------------------------

compute_changes <- function(df_current_cleaned, df_recent) {
  date_recent <- format(min(df_recent$updated), "%d.%m.%Y")
  df_changes <- df_current_cleaned %>%
    dplyr::inner_join(df_recent,
                      by = c("canton", "topic_title"),
                      suffix = c("_current", "_recent")) %>%
    dplyr::mutate(
      publication_data_current = ifelse(
        contract_required_data_current == TRUE,
        stringr::str_c(publication_data_current, ", mit Vertrag"),
        publication_data_current),
      publication_data_recent = ifelse(
        contract_required_data_recent == TRUE,
        stringr::str_c(publication_data_recent, ", mit Vertrag"),
        publication_data_recent)) %>%
    dplyr::filter(publication_data_current != publication_data_recent) %>%
    dplyr::select(canton, topic_title, publication_data_recent, publication_data_current)

  names(df_changes) <- c("Kanton", "Datensatz",
                         stringr::str_c("Zugriffsregelung am ", date_recent),
                         "Zugriffsregelung neu")
  dplyr::arrange(df_changes, Kanton, Datensatz)
}

# ---- Time-series (time-series.R) ---------------------------------------------

compute_timeseries <- function(df_hist_long) {
  df_hist_long %>%
    dplyr::group_by(updated, canton) %>%
    dplyr::summarise(
      number_available_datasets = dplyr::n(),
      mean_open_score = mean(open_score),
      .groups = "drop") %>%
    dplyr::arrange(updated)
}

# ---- Plot builders -----------------------------------------------------------
# Each plot builder takes only data + already-loaded constants from config.R.
# The colour vector `cols` is passed explicitly to avoid the side-effecting
# reassignment that happens in analyse.R line 173.

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

# Missing-data plot uses its own (greyscale) colour vector; we keep this
# local instead of mutating the global `cols`.
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

# Comparison plots: jitter is RNG-driven; the seed is set in _targets.R before
# the target evaluates, to match the harness.
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

# ---- Time-series plot builders ----------------------------------------------

facet_theme <- function(theme_options) {
  theme_options +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.2),
      panel.grid.minor.y = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.1),
      panel.grid.major.x = ggplot2::element_line(color = "#DDDDDD", linewidth = 0.2),
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

plot_timeseries_number <- function(df_ts, theme_options) {
  ggplot2::ggplot(df_ts, ggplot2::aes(x = updated, y = number_available_datasets)) +
    ggplot2::geom_line(color = "#4575B4", linewidth = 1) +
    ggplot2::labs(x = "Datum", y = "Anzahl verf\u00fcgbarer Datens\u00e4tze\n") +
    ggplot2::facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}

plot_timeseries_openness <- function(df_ts, theme_options) {
  ggplot2::ggplot(df_ts, ggplot2::aes(x = updated, y = mean_open_score)) +
    ggplot2::geom_line(color = "#54B987", linewidth = 1) +
    ggplot2::labs(x = "Datum", y = "Offenheit der verf\u00fcgbaren Datens\u00e4tze\n") +
    ggplot2::facet_wrap(~canton, ncol = 5, scales = "fixed") +
    facet_theme(theme_options)
}

# ============================================================================
# Step 3: CSV-driven reference data
# ----------------------------------------------------------------------------
# The functions below replace the inline case_when() lookups in
# functions.R with joins against CSVs in data/reference/. They are observably
# equivalent to the originals on the current inputs — verified bit-for-bit
# via tests/golden/check_equivalence.R against GEODIENSTE_CHECK=targets.
#
# Adding a new topic or publication type is now a one-line edit in
# data/reference/topic_shortnames.csv or openness_scores.csv, no code change.

read_topic_shortnames <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(.default = readr::col_character()))
}

read_openness_scores <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(
                    publication_type = readr::col_character(),
                    open_score = readr::col_double()
                  ))
}

read_cantons <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(canton = readr::col_character()))$canton
}

# CSV-driven equivalent of functions.R:clean_data().
#
# Observable behaviour is identical, including:
#   - factor(canton, levels = unique(canton)) — the level order is taken
#     from the input row order so the time-series and current-data
#     pipelines retain the same factor structure they had before.
#   - the contract_required_wms <- contract_required_data assignment in
#     functions.R:86 (apparent copy/paste bug) is preserved verbatim
#     to keep equivalence; cleanup is tracked as a follow-up task.
#   - topics not found in the shortnames CSV are labelled "unbekannt".
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

# CSV-driven equivalent of functions.R:compute_openness_per_topic().
#
# - Adds ", mit Vertrag" suffix to publication_type when a contract is
#   required and the type is not "Im Aufbau" (matches functions.R:142-146).
# - Joins openness scores from the reference CSV instead of an inline
#   case_when().
# - Factorises publication_type using the levels in config.R
#   (factor_levels_publication), preserved as the function argument.
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

# Targets-facing wrappers that bundle CSV-driven cleaning + harmonisation +
# openness computation in the same shape as the v1 functions in pipeline.R.

clean_current_v2 <- function(csv_path, topic_shortnames, valid_cantons) {
  df <- read_geodienste_csv(csv_path)
  df <- clean_data_v2(df, topic_shortnames, valid_cantons)
  df
}

clean_recent_v2 <- function(csv_path, topic_shortnames, valid_cantons) {
  df <- read_geodienste_csv(csv_path)
  df <- clean_data_v2(df, topic_shortnames, valid_cantons)
  df
}

long_with_openness_v2 <- function(df_clean, openness_scores,
                                  factor_levels_publication) {
  df <- harmonise_data_and_wms_atts(df_clean)
  df <- compute_openness_per_topic_v2(df, openness_scores,
                                      factor_levels_publication)
  df
}

build_historic_long_v2 <- function(archive_csvs, topic_shortnames,
                                   openness_scores,
                                   factor_levels_publication,
                                   valid_cantons) {
  df <- purrr::map_df(archive_csvs, read_geodienste_csv)
  df <- clean_data_v2(df, topic_shortnames, valid_cantons)
  df <- harmonise_data_and_wms_atts(df)
  df <- compute_openness_per_topic_v2(df, openness_scores,
                                      factor_levels_publication)
  df %>%
    dplyr::filter(publication_type != "Keine Daten") %>%
    dplyr::filter(publication_type != "Im Aufbau") %>%
    dplyr::filter(offering != "WMS") %>%
    dplyr::filter(canton != "FL") %>%
    dplyr::filter(updated >= "2023-06-06")
}
