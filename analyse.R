library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(tidylog)
library(plotly)
library(ggrepel)
# library(sf)

source(here("config.R"), encoding = "UTF-8")

# Ingest CSV as provided by the Python script (the geodienste.ch API also offers
# a CSV directly, but it is malformed in several ways as of 05.2022)
df <- read_delim(csv_path, delim = ";", na = c("{}", "''", '""', ""))

updated <- format(min(df$updated), "%d.%m.%Y")



# Clean the raw data --------------------------------------------------------------------------

# This step takes care of the following:
# - remove administration-internal datasets (these datasets contain the term "verwaltungsintern"
#   in their German `topic_title`)
# - abbreviate topic names for later use as labels in the visualizations
# - harmonise values for missing data or service for both data downloads and WMS
# - fill NAs in contract requirement attributes

df <- df %>%
  filter(!str_detect(topic_title, "verwaltungsintern")) %>%
  filter(!str_detect(canton, "Broker")) %>%
  mutate(topic_title_short =
           case_when(
             topic_title == "Amtliche Vermessung" ~ "AV",
             topic_title == "Bewirtschaftungseinheiten" ~ "BewE",
             topic_title == "Biodiversitätsförderflächen, Qualitätsstufe II und Vernetzung" ~ "BdF",
             topic_title == "Elektrische Anlagen mit einer Nennspannung von über 36 kV" ~ "EAl36",
             topic_title == "Fixpunkte (Kategorie 2)" ~ "FP",
             topic_title == "Fruchtfolgeflächen" ~ "FFF",
             topic_title == "Gefahrenkarten" ~ "Gk",
             topic_title == "Gewässerraum" ~ "Gwr",
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

df_current_cleaned <- df

df %>%
  group_by(topic_title) %>%
  mutate(cantons = paste0(canton, collapse = ", "),
         count = n()) %>%
  select(topic_title, count, cantons) %>%
  unique()

df %>%
  filter(topic_title_short == "unbekannt") %>%
  group_by(topic_title) %>%
  select(topic_title, topic_title_short) %>%
  unique()

# Compute openness scores (for data and for WMS) per dataset ----------------------------------

df <- df %>%
  # Assign an openness score (for data and for WMS) to each dataset based on publication type
  mutate(open_score_data =
           case_when(
             publication_data == "Frei erhältlich" ~ 3,
             publication_data == "Registrierung erforderlich" ~ 2,
             publication_data == "Freigabe erforderlich" ~ 1,
             publication_data == "Im Aufbau" ~ 0,
             publication_data == "Keine Daten" ~ 0),
         open_score_wms =
           case_when(
             publication_wms == "Frei erhältlich" ~ 3,
             publication_wms == "Registrierung erforderlich" ~ 2,
             publication_wms == "Freigabe erforderlich" ~ 1,
             publication_wms == "Im Aufbau" ~ 0,
             publication_wms == "Keine Daten" ~ 0)) %>%
  # Modify the raw openness scores based on contract requirements (the openness score is halfed, if
  # using the dataset requires a contract) and adapt the publication type ("mit Vertrag" is added
  # to datasets that require a contract)
  mutate(
    open_score_data = ifelse(
      contract_required_data == TRUE,
      open_score_data / 2,
      open_score_data),
    open_score_wms = ifelse(
      contract_required_wms == TRUE,
      open_score_wms / 2,
      open_score_wms),
    publication_data = ifelse(
      contract_required_data == TRUE & publication_data != "Im Aufbau",
      str_c(publication_data, ", mit Vertrag"),
      publication_data),
    publication_wms = ifelse(
      contract_required_wms == TRUE & publication_wms != "Im Aufbau",
      str_c(publication_wms, ", mit Vertrag"),
      publication_wms))



# Convert some attributes to factors ----------------------------------------------------------

df <- df %>%
mutate(
  canton = factor(canton, levels = unique(canton)),
  publication_data = factor(publication_data, factor_levels_publication),
  publication_wms = factor(publication_wms, factor_levels_publication))



# Reshape data for easier canton-level analysis -----------------------------------------------

# Split data into data about data downloads and about WMS

df_data <- df %>%
  mutate(
    offering = "data download") %>%
  select(
    canton,
    topic_title,
    topic_title_short,
    version,
    offering,
    publication_data,
    contract_required_data,
    open_score_data,
    updated) %>%
  rename(
    publication_type = publication_data,
    contract_required = contract_required_data,
    open_score = open_score_data)

df_wms <- df %>%
  mutate(
    offering = "WMS") %>%
  select(
    canton,
    topic_title,
    topic_title_short,
    version,
    offering,
    publication_wms,
    contract_required_wms,
    open_score_wms,
    updated) %>%
  rename(
    publication_type = publication_wms,
    contract_required = contract_required_wms,
    open_score = open_score_wms)

# Reassemble data into a long table
df <- rbind(df_data, df_wms)
rm(df_data)
rm(df_wms)



# Compute openness scores per canton and per offering -----------------------------------------

df2 <- df %>%
  # Aggregate topics per canton, per offering (data or WMS) and per publication type
  # (and open score - which is the same aggregation as publication type)
  group_by(canton,
           offering,
           publication_type,
           open_score) %>%
  summarise(count = n(),
            topics = paste0(topic_title_short, collapse = ", ")) %>%
  ungroup() %>%
  # Compute several auxiliary metrics:
  # - ..._wo_nd: without, i.e. ignoring, "Keine Daten" (no data)
  # - ..._wo_nduc: without, i.e. ignoring, "Keine Daten" (no data) and "Im Aufbau" (under
  #   construction)
  mutate(
    open_score_wo_nd = ifelse(publication_type == "Keine Daten", NA, open_score),
    count_wo_nd = ifelse(publication_type == "Keine Daten", NA, count),
    open_score_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, open_score),
    count_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, count)) %>%
  # Compute proportion of each publication type per canton
  group_by(canton,
           offering) %>%
  mutate(
    proportion = count / sum(count, na.rm = TRUE),
    proportion_wo_nd = count_wo_nd / sum(count_wo_nd, na.rm = TRUE),
    proportion_wo_nduc = count_wo_nduc / sum(count_wo_nduc, na.rm = TRUE),
    open_score_canton = sum(open_score * proportion, na.rm = TRUE),
    open_score_wo_nd_canton = sum(open_score_wo_nd * proportion_wo_nd, na.rm = TRUE),
    open_score_wo_nduc_canton = sum(open_score_wo_nduc * proportion_wo_nduc, na.rm = TRUE)) %>%
  # Reorder attributes
  select(canton, offering, publication_type, topics, open_score, count, count_wo_nd, count_wo_nduc,
         proportion, proportion_wo_nd, proportion_wo_nduc,
         open_score_canton, open_score_wo_nd_canton, open_score_wo_nduc_canton)

# Compute some canton-level metrics ------------------------

df2 <- df2 %>%
  left_join(
    df2 %>%
      filter(offering == "data download") %>%
      filter(publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
      group_by(canton) %>%
      summarise(count_missing_canton = sum(count)),
    by = "canton") %>%
  left_join(
    df2 %>%
      filter(offering == "data download") %>%
      filter(publication_type %in% c("Keine Daten")) %>%
      group_by(canton) %>%
      summarise(count_nd_canton = sum(count)),
    by = "canton") %>%
  left_join(
    df2 %>%
      filter(offering == "data download") %>%
      filter(!publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
      group_by(canton) %>%
      summarise(count_available_canton = sum(count)),
    by = "canton") %>%
  mutate(
    count_missing_canton = ifelse(is.na(count_missing_canton), 0, count_missing_canton),
    count_nd_canton = ifelse(is.na(count_nd_canton), 0, count_nd_canton),
    count_available_canton = ifelse(is.na(count_available_canton), 0, count_available_canton))


# Sort the abbreviated topic titles in the "topics" attribute alphabetically ------------------

# Hmm, this seems somewhat more complicated than necessary (?)
sort_topics <- function(dataframe, output){
  str_c(unlist(dataframe[4]), collapse = ", ")
}

df2$topics = lapply(str_split(df2$topics, ", "), sort)
df2 <- cbind(df2, sorted_topics = apply(df2, 1, sort_topics))
df2$sorted_topics = str_wrap(df2$sorted_topics, width = 30)
df2$topics <- df2$sorted_topics
df2$sorted_topics <- NULL



# Produce plots of publication type proportions ---------------------------

plt_data_prop_all <- df2 %>%
  filter(offering == "data download") %>%
  ggplot(aes(proportion, reorder(canton, +open_score_canton),
             fill = publication_type,
             text = str_c("Kanton ", canton, "\n\n",
                          publication_type, ": ", round(100 * proportion, 1),
                          "% aller Datensätze\n\n",
                          "Datensätze in dieser Kategorie:\n", topics))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  labs(fill = "Zugriffskategorie", x = "Anteil der Datensätze") +
  theme_options +
  theme(axis.title.y = element_blank())

plt_data_prop_wo_nd <- df2 %>%
  filter(offering == "data download") %>%
  filter(!publication_type == "Keine Daten") %>%
  ggplot(aes(proportion_wo_nd, reorder(canton, +open_score_wo_nd_canton),
             fill = publication_type,
             text = str_c("Kanton ", canton, "\n\n",
                          publication_type, ": ", round(100 * proportion_wo_nd, 1),
                          "% der untersuchten Datensätze\n(ohne \"Keine Daten\")\n\n",
                          "Datensätze in dieser Kategorie:\n", topics))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  labs(fill = "Zugriffskategorie", x = "Anteil der untersuchten Datensätze") +
  theme_options +
  theme(axis.title.y = element_blank())

plt_data_prop_wo_nduc <- df2 %>%
  filter(offering == "data download") %>%
  filter(!publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
  ggplot(aes(proportion_wo_nduc, reorder(canton, +(open_score_wo_nduc_canton * 1000 + count_available_canton)),
             fill = publication_type,
             text = str_c("Kanton ", canton, "\n\n",
                          publication_type, ": ", round(100 * proportion_wo_nduc, 1),
                          "% der untersuchten Datensätze\n(ohne \"Keine Daten\" und ohne \"Im Aufbau\")\n\n",
                          "Datensätze in dieser Kategorie:\n", topics))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(labels = scales::percent) +
  labs(fill = "Zugriffskategorie", x = "Anteil der untersuchten Datensätze") +
  theme_options +
  theme(axis.title.y = element_blank())

plt_data_prop_wo_nduc

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

plt_data_prop_all <- plotlyfy(plt_data_prop_all)
plt_data_prop_wo_nd <- plotlyfy(plt_data_prop_wo_nd)
plt_data_prop_wo_nduc <- plotlyfy(plt_data_prop_wo_nduc)





# -------------------------------------------------------------------------



df2_missing <- df2 %>%
  mutate(
    temp_order_nd = count_missing_canton * 1000 + count_nd_canton,
    publication_type = factor(publication_type, factor_levels_publication_missing)
  )

cols <- c("#9F9F9F", "#B9B9B9")

plt_data_missingdata <- df2_missing %>%
  filter(!canton == "FL") %>%
  filter(offering == "data download") %>%
  filter(publication_type %in% c("Keine Daten", "Im Aufbau")) %>%
  ggplot(aes(count, reorder(canton, -temp_order_nd),
             fill = publication_type,
             text = str_c("Kanton ", canton, "\n\n",
                          publication_type, ": ", count,
                          " Themen\n\n",
                          "Themen in dieser Kategorie:\n", topics))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = cols) +
  labs(fill = "Datenverfügbarkeit", x = "Anzahl nicht vorhandener Datensätze") +
  theme_options +
  theme(axis.title.y = element_blank())

plt_data_missingdata <- plotlyfy(plt_data_missingdata)



# Analyse openness and availability together ------------------------------

df_canton <- df2 %>%
  filter(offering == "data download") %>%
  filter(!canton == "FL") %>%
  group_by(canton) %>%
  summarise(
    open_score_canton = open_score_canton,
    open_score_wo_nd_canton = open_score_wo_nd_canton,
    open_score_wo_nduc_canton = open_score_wo_nduc_canton,
    count_missing_canton = count_missing_canton,
    count_nd_canton = count_nd_canton,
    count_available_canton = count_available_canton
  ) %>%
  distinct()


# Produce plot
min_openness <- min(df_canton$open_score_wo_nduc_canton) - 0.15
med_openness <- median(df_canton$open_score_wo_nduc_canton)
max_openness <- 3 + 0.15
min_count <- min(df_canton$count_available_canton) - 0.5
med_count <- median(df_canton$count_available_canton)
max_count <- max(df_canton$count_available_canton) + 0.5

plt_data_comparison <- df_canton %>%
  ggplot() +
  # In sequence: bottom left, bottom right, top left, and top right
  geom_rect(aes(xmin = min_openness, xmax = med_openness,
                ymin = min_count, ymax = med_count,
                text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#AABFDB") +
  geom_rect(aes(xmin = min_openness, xmax = med_openness,
                ymin = med_count, ymax = max_count,
                text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#85CCA9") +
  geom_rect(aes(xmin = med_openness, xmax = max_openness,
                ymin = min_count, ymax = med_count,
                text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#85CCD2") +
  geom_rect(aes(xmin = med_openness, xmax = max_openness,
                ymin = med_count, ymax = max_count,
                text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#54B987") +
  geom_text(aes(x = open_score_wo_nduc_canton,
                y = count_available_canton,
                label = paste0(sprintf("<b>%s</b>", canton)),
                text = str_c("Kanton ", canton, "\n\n",
                             "Offenheit: ", round(open_score_wo_nduc_canton, 2), "\n",
                             "Verfügbare Datensätze: ", count_available_canton)),
            size = 5,
            position = position_jitter(width = 0.05, height = 0.2)) +
  scale_x_continuous(limits = c(min_openness, max_openness),
                     breaks = seq(0, 3, 0.5),
                     expand = expansion(add = c(0.03, 0))) +
  scale_y_continuous(breaks = seq(0, max_count, 1),
                     expand = expansion(add = c(0.3, 0.2))) +
  labs(y = "Anzahl verfügbarer Datensätze",
       x = "Offenheit der verfügbaren Datensätze") +
  theme_options

plt_data_comparison <- plotlyfy_w_zoom(plt_data_comparison)
plt_data_comparison

min_openness <- med_openness - 0.23
max_openness <- 3.02
min_count <- med_count - 4

plt_data_comparison_subset <- df_canton %>%
  ggplot() +
  # In sequence: bottom left, top left, bottom right, and top right
  geom_rect(aes(xmin = min_openness, xmax = med_openness,
                ymin = min_count, ymax = med_count,
                text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#AABFDB") +
  geom_rect(aes(xmin = min_openness, xmax = med_openness,
                ymin = med_count, ymax = max_count,
                text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nunterdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#85CCA9") +
  geom_rect(aes(xmin = med_openness, xmax = max_openness,
                ymin = min_count, ymax = med_count,
                text = "unterdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#85CCD2") +
  geom_rect(aes(xmin = med_openness, xmax = max_openness,
                ymin = med_count, ymax = max_count,
                text = "überdurchschnittliche Anzahl verfügbarer Datensätze,\nüberdurchschnittlich offene Nutzbarkeit\nder vorhandenen Datensätze"), fill = "#54B987") +
  geom_text(aes(x = open_score_wo_nduc_canton,
                y = count_available_canton,
                label = canton,
                text = str_c("Kanton ", canton, "\n\n",
                             "Offenheit: ", round(open_score_wo_nduc_canton, 2), "\n",
                             "Verfügbare Datensätze: ", count_available_canton)),
            fontface = "bold",
            position = position_jitter(width = 0.005, height = 0.2)) +
  scale_x_continuous(limits = c(min_openness, max_openness), breaks = seq(0, 3, 0.05)) +
  scale_y_continuous(limits = c(min_count, max_count), breaks = seq(min_count, max_count, 2)) +
  labs(y = "Anzahl verfügbarer Datensätze",
       x = "Offenheit der verfügbaren Datensätze") +
  theme_options

plt_data_comparison_subset <- plotlyfy_w_zoom(plt_data_comparison_subset)
plt_data_comparison_subset

# Produce map of categorized cantons
df_canton %>%
  mutate(
    category = case_when(
      open_score_wo_nduc_canton >= med_openness & count_available_canton >= med_count ~ "überdurchschnittlich viele Daten,\nüberdurchschnittlich offen",
      open_score_wo_nduc_canton >= med_openness & count_available_canton < med_count ~ "unterdurchschnittliche Anzahl Daten,\nüberdurchschnittlich offen",
      open_score_wo_nduc_canton < med_openness & count_available_canton < med_count ~ "unterdurchschnittliche Anzahl Daten,\nunterdurchschnittlich offen",
      open_score_wo_nduc_canton < med_openness & count_available_canton >= med_count ~ "überdurchschnittlich viele Daten,\nunterdurchschnittlich offen")) -> df_canton


# swiss_map <- st_read(here("data", "switzerland-canton-map.geojson"), quiet = TRUE) %>%
#   left_join(df_canton, by = c("iso_3166_2" = "canton"))
#
# cantons_map <- ggplot(data = swiss_map,
#                       aes(fill = category)) +
#   geom_sf(color = "white") +
#   scale_fill_manual(values = c("#54B987", "#85CCA9", "#85CCD2", "#AABFDB")) +
#   theme_options +
#   theme(axis.text = element_blank(),
#         legend.position = "bottom",
#         legend.spacing.x = unit(0.3, 'cm'),
#         legend.spacing.y = unit(0.3, 'cm'),
#         legend.text = element_text(size = 9),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.margin = unit(c(0, 0, 0, 0), "cm"),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         plot.background = element_rect(fill = "white", colour = NA),
#         plot.margin = unit(c(-1.4, -1, -1, -1.1), "cm")) +
#   labs(x = NULL, y = NULL) +
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE))
#
# ggsave(here("map-data-openness-completeness.png"),
#        cantons_map, width = 22, height = 17.3, units = "cm")


# Change detection

# Find the CSV file from four weeks ago
csv_files <- list.files(path="data",
                        pattern="^\\d{4}-\\d{2}-\\d{2}-geodienste-ch\\.csv$",
                        full.names=TRUE)
csv_files <- str_sort(csv_files, decreasing=TRUE)
recent_csv_file <- csv_files[5]

# Read and clean the previous CSV file
df_recent <- read_delim(recent_csv_file, delim = ";", na = c("{}", "''", '""', ""))
df_recent <- df_recent %>%
  filter(!str_detect(topic_title, "verwaltungsintern")) %>%
  mutate(topic_title_short =
           case_when(
             topic_title == "Amtliche Vermessung" ~ "AV",
             topic_title == "Bewirtschaftungseinheiten" ~ "BewE",
             topic_title == "Biodiversitätsförderflächen, Qualitätsstufe II und Vernetzung" ~ "BdF",
             topic_title == "Elektrische Anlagen mit einer Nennspannung von über 36 kV" ~ "EAl36",
             topic_title == "Fixpunkte (Kategorie 2)" ~ "FP",
             topic_title == "Fruchtfolgeflächen" ~ "FFF",
             topic_title == "Gefahrenkarten" ~ "Gk",
             topic_title == "Gewässerraum" ~ "Gwr",
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

# Join the recent data to the current data and keep records with changes
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

# Rename dataframe columns and sort by canton and by dataset title
date_recent <- format(min(df_recent$updated), "%d.%m.%Y")
names(df_changes) <- c("Kanton", "Datensatz",
                      str_c("Zugriffsregelung am ", date_recent),
                      "Zugriffsregelung neu")
df_changes <- arrange(df_changes, Kanton, Datensatz)
