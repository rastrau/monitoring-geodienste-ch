library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(tidylog)
library(plotly)

source(here("config.R"), encoding = "UTF-8")

clean_data <- function(df) {
  # Function to clean the raw data
  # This step takes care of the following:
  # - remove administration-internal datasets (these datasets contain the term "verwaltungsintern"
  #   in their German `topic_title`)
  # - abbreviate topic names for later use as labels in the visualizations
  # - harmonise values for missing data or service for both data downloads and WMS
  # - fill NAs in contract requirement attributes
  df <- df %>%
    filter(!str_detect(topic_title, "verwaltungsintern")) %>%
    filter(!str_detect(canton, "Broker")) %>%
    select(-version, -comment) %>%
    mutate(
      canton = factor(canton, levels = unique(canton)),
      topic_title_short =
             case_when(
               topic_title == "Amtliche Vermessung" ~ "AV",
               topic_title == "Bewirtschaftungseinheiten" ~ "BewE",
               topic_title == "Biodiversitätsförderflächen, Qualitätsstufe II und Vernetzung" ~ "BdF",
               topic_title == "Elektrische Anlagen mit einer Nennspannung von über 36 kV" ~ "EAl36",
               topic_title == "Fixpunkte (Kategorie 2)" ~ "FP",
               topic_title == "Fruchtfolgeflächen" ~ "FFF",
               topic_title == "Gefahrenkarten" ~ "Gk",
               topic_title == "Gewässerraum" ~ "Gwr",
               topic_title == "Grundwasservorkommen" ~ "Gwv",
               topic_title == "Holznutzungsbewilligung" ~ "Hnb",
               topic_title == "Inventar der bestehenden Wasserentnahmen" ~ "IbWe",
               topic_title == "Kantonale Ausnahmetransportrouten" ~ "KAtr",
               topic_title == "Kataster der belasteten Standorte" ~ "KbS",
               topic_title == "Landw. Bewirtschaftung: Elemente mit Landschaftsqualität" ~ "ELq",
               topic_title == "Leitungskataster" ~ "LK",
               topic_title == "Luftbild" ~ "LB",
               topic_title == "Lärmempfindlichkeitsstufen (in Nutzungszonen)" ~ "LeS",
               topic_title == "Naturereigniskataster" ~ "NeK",
               topic_title == "Naturereigniskataster erweitert" ~ "NeKe",
               topic_title == "Nutzungsflächen" ~ "NF",
               topic_title == "Nutzungsplanung (kantonal / kommunal)" ~ "NuP",
               topic_title == "Perimeter Landwirtschaftliche Nutzfläche und Sömmerung" ~ "PLSF",
               topic_title == "Perimeter Terrassenreben" ~ "PTr",
               topic_title == "Planerischer Gewässerschutz" ~ "PGs",
               topic_title == "Planung der Revitalisierungen von Fliessgewässern" ~ "RFg",
               topic_title == "Planung der Revitalisierungen von Seeufern" ~ "RSu",
               topic_title == "Planung und Berichterstattung der Sanierung Wasserkraft" ~ "SWk",
               topic_title == "Planungszonen" ~ "Pz",
               topic_title == "Rebbaukataster" ~ "RbK",
               topic_title == "Richtplanung erneuerbare Energien" ~ "ReE",
               topic_title == "Rodungen und Rodungsersatz" ~ "RuR",
               topic_title == "Statische Waldgrenzen" ~ "SWG",
               topic_title == "Stromversorgungssicherheit: Netzgebiete" ~ "SNG",
               topic_title == "Waldabstandslinien" ~ "WaL",
               topic_title == "Waldreservate" ~ "Wr",
               topic_title == "Wildruhezonen" ~ "WrZ")) %>%
    # If this occurs, topic_title_short is not yet defined:
    mutate(topic_title_short = ifelse(is.na(topic_title_short), "unbekannt", topic_title_short)) %>%
    mutate(
      publication_data = ifelse(
        publication_data %in% c("Keine Daten", "keine Daten", ""),
        "Keine Daten",
        publication_data),
      publication_wms = ifelse(
        publication_wms %in% c("Keine Daten","keine Daten", ""),
        "Keine Daten",
        publication_wms)) %>%
    mutate(
      contract_required_data = replace_na(contract_required_data, FALSE),
      contract_required_wms = replace_na(contract_required_data, FALSE))

  return(df)
}


quality_assurance_after_import <- function(df) {

  # Quality assurance: Is there data for all topics over all cantons and FL
  # (27 entities)? If yes, count should be 27 for all records.
  df %>%
    group_by(topic_title) %>%
    mutate(cantons = paste0(canton, collapse = ", "),
           count = n()) %>%
    select(topic_title, count, cantons) %>%
    unique()

  # Quality assurance: Are <topic_title_short> values defined for all values of
  # <topic_title>? If yes, result set should be empty.
  df %>%
    filter(topic_title_short == "unbekannt") %>%
    group_by(topic_title) %>%
    select(topic_title, topic_title_short) %>%
    unique()
}


harmonise_data_and_wms_atts <- function(df) {
  # Split data into data about data downloads and about WMS, and reassemble the
  # data into a long table with harmonised attribute names
  df_data <- df %>%
    mutate(offering = "data download") %>%
    select(canton, topic_title, topic_title_short, offering,
           publication_data, contract_required_data, updated) %>%
    rename(publication_type = publication_data,
           contract_required = contract_required_data)

  df_wms <- df %>%
    mutate(offering = "WMS") %>%
    select(canton, topic_title, topic_title_short, offering,
           publication_wms, contract_required_wms, updated) %>%
    rename(publication_type = publication_wms,
           contract_required = contract_required_wms)

  df <- rbind(df_data, df_wms)
  rm(df_data)
  rm(df_wms)

  return(df)
}


compute_openness_per_topic <- function(df) {
  df <- df %>%
    # Mutate the publication type ("mit Vertrag" is added to datasets that
    # require a contract)
    mutate(
      publication_type =
        ifelse(contract_required == TRUE & publication_type != "Im Aufbau",
               str_c(publication_type, ", mit Vertrag"),
               publication_type)) %>%
    # Assign an openness score to each dataset based on publication type. The
    # openness score is halfed, if using the dataset requires a contract.
    mutate(
      open_score =
        case_when(
          publication_type == "Frei erhältlich" ~ 3,
          publication_type == "Frei erhältlich, mit Vertrag" ~ 1.5,
          publication_type == "Registrierung erforderlich" ~ 2,
          publication_type == "Registrierung erforderlich, mit Vertrag" ~ 1,
          publication_type == "Freigabe erforderlich" ~ 1,
          publication_type == "Freigabe erforderlich, mit Vertrag" ~ 0.5,
          publication_type == "Im Aufbau" ~ 0,
          publication_type == "Keine Daten" ~ 0),
      publication_type = factor(publication_type, factor_levels_publication)
      ) %>%
    select(canton, topic_title, topic_title_short, updated, offering,
           publication_type, open_score)

  return(df)
}


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


analyse_openness <- function(df) {
  df <- df %>%
    # Aggregate topics per canton, per offering (data or WMS), per publication
    # type (and open score - which is the same aggregation as publication type)
    # (and per point in time)
    group_by(canton, offering, publication_type, open_score, updated) %>%
    summarise(count = n(),
              topics = paste0(topic_title_short, collapse = ", ")) %>%
    ungroup() %>%

    # Compute several auxiliary metrics:
    # - ..._wo_nd:     without, i.e. ignoring, "Keine Daten" (no data)
    # - ..._wo_nduc:   without, i.e. ignoring, "Keine Daten" (no data)
    #                  and "Im Aufbau" (under construction)
    mutate(
      open_score_wo_nd = ifelse(publication_type == "Keine Daten", NA, open_score),
      count_wo_nd = ifelse(publication_type == "Keine Daten", NA, count),
      open_score_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, open_score),
      count_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, count)) %>%
    # Compute proportion of each publication type per canton (and point in time)
    group_by(canton, offering, updated) %>%
    mutate(
      proportion = count / sum(count, na.rm = TRUE),
      proportion_wo_nd = count_wo_nd / sum(count_wo_nd, na.rm = TRUE),
      proportion_wo_nduc = count_wo_nduc / sum(count_wo_nduc, na.rm = TRUE),
      open_score_canton = sum(open_score * proportion, na.rm = TRUE),
      open_score_wo_nd_canton = sum(open_score_wo_nd * proportion_wo_nd, na.rm = TRUE),
      open_score_wo_nduc_canton = sum(open_score_wo_nduc * proportion_wo_nduc, na.rm = TRUE)) %>%
    # Reorder attributes
    select(canton, updated, offering, publication_type, topics, open_score,
           count, count_wo_nd, count_wo_nduc, proportion, proportion_wo_nd,
           proportion_wo_nduc, open_score_canton, open_score_wo_nd_canton,
           open_score_wo_nduc_canton)
  df
}


plot_comparison <- function(df_canton, theme_options, min_openness, med_openness, max_openness,
                            min_count, med_count, max_count,
                            jitter_horizontal, jitter_vertical) {

  ggplot(data = df_canton) +
    # Define rectangles for different categories
    geom_rect(aes(xmin = min_openness, xmax = med_openness,
                  ymin = min_count, ymax = med_count,
                  text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"),
              fill = "#AABFDB") +
    geom_rect(aes(xmin = min_openness, xmax = med_openness,
                  ymin = med_count, ymax = max_count,
                  text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"),
              fill = "#85CCA9") +
    geom_rect(aes(xmin = med_openness, xmax = max_openness,
                  ymin = min_count, ymax = med_count,
                  text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"),
              fill = "#85CCD2") +
    geom_rect(aes(xmin = med_openness, xmax = max_openness,
                  ymin = med_count, ymax = max_count,
                  text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"),
              fill = "#54B987") +
    geom_text(aes(x = open_score_wo_nduc_canton,
                  y = count_available_canton,
                  label = paste0(sprintf("<b>%s</b>", canton)),
                  text = str_c("Kanton ", canton, "\n\n",
                               "Offenheit: ", round(open_score_wo_nduc_canton, 2), "\n",
                               "Verfügbare Datensätze: ", count_available_canton)),
              size = 5,
              position = position_jitter(width = jitter_horizontal,
                                         height = jitter_vertical)) +
    #scale_x_continuous(limits = c(min_openness, max_openness),
                       #breaks = seq(0, 3, 0.5),
                       #                  expand = expansion(add = c(0.03, 0))) +
    #scale_y_continuous(breaks = seq(0, max_count, 1),
                       #expand = expansion(add = c(0.3, 0.2))) +
    scale_x_continuous(limits = c(min_openness, max_openness),
                       breaks = pretty(c(min_openness, max_openness), n = 5)) +
    scale_y_continuous(limits = c(min_count, max_count),
                       breaks = pretty(c(min_count + 0.5, max_count - 0.5), n = 5)) +

    labs(y = "Anzahl verfügbarer Datensätze",
         x = "Offenheit der verfügbaren Datensätze") +
    theme_options
}


plotlyfy <- function(plt){
  ggplotly(plt, tooltip = c("text")) %>%
    config(locale = "de-ch",
           modeBarButtonsToRemove = plotly_buttons_to_remove,
           displaylogo = FALSE) %>%
    layout(dragmode = FALSE)
}

plotlyfy_w_zoom <- function(plt){
  ggplotly(plt, tooltip = c("text")) %>%
    config(locale = "de-ch",
           modeBarButtonsToRemove = plotly_w_zoom_buttons_to_remove,
           displaylogo = FALSE,
           displayModeBar = TRUE) %>%
    layout(dragmode = FALSE)
}