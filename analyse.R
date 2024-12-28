library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(tidylog)
library(plotly)

source(here("config.R"), encoding = "UTF-8")
source(here("functions.R"), encoding = "UTF-8")

# Ingest CSV as provided by the Python script (the geodienste.ch API also offers
# a CSV directly, but it is malformed in several ways as of 05.2022). Store
# the update date of the input data in a variable for use in the Quarto report.
df <- read_delim(csv_path, delim = ";", na = c("{}", "''", '""', ""))
updated <- format(min(df$updated), "%d.%m.%Y")


# Clean and transform the raw data ---------------------------------------------

df <- clean_data(df)

quality_assurance_after_import(df)

# Copy this dataframe for comparison later on
df_current_cleaned <- df

# Transform data into long format
df <- harmonise_data_and_wms_atts(df)

# Compute openness scores (for data and for WMS) per topic ---------------------
df <- compute_openness_per_topic(df)


# Compute openness scores per offering (data and wms) and per canton -----------

df2 <- df %>%
  # Aggregate topics per canton, per offering (data or WMS) and per publication
  # type (and open score - which is the same aggregation as publication type)
  group_by(canton, offering, publication_type, open_score) %>%
  summarise(
    count = n(),
    topics = paste0(topic_title_short, collapse = ", "),
    .groups = 'drop') %>%
  # Compute several auxiliary metrics:
  # - ..._wo_nd:    without, i.e. ignoring (=not counting as available),
  #                 "Keine Daten" (no data)
  # - ..._wo_nduc:  without, i.e. ignoring (=not counting as available)
  #                 "Keine Daten" (no data) and "Im Aufbau" (under construction)
  mutate(
    open_score_wo_nd = ifelse(publication_type == "Keine Daten", NA, open_score),
    count_wo_nd = ifelse(publication_type == "Keine Daten", NA, count),
    open_score_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, open_score),
    count_wo_nduc = ifelse(publication_type %in% c("Keine Daten", "Im Aufbau"), NA, count)
  ) %>%
  # Compute proportion of each publication type per canton
  group_by(canton, offering) %>%
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

# Sort abbreviated topic titles in the "topics" attribute alphabetically
df2 <- sort_topics(df2, width = 30)


# Compute some canton-level metrics and join them to df2 -----------------------

# Create a summary for missing data counts by canton
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
    .groups = 'drop'
  )

# Join the summaries back to df2
df2 <- df2 %>%
  left_join(missing_data_summary, by = "canton") %>%

  # Replace NA values with zero for counts
  mutate(
    count_missing_canton = ifelse(is.na(count_missing_canton), 0, count_missing_canton),
    count_nd_canton = ifelse(is.na(count_nd_canton), 0, count_nd_canton),
    count_available_canton = ifelse(is.na(count_available_canton), 0, count_available_canton)
  )


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


# Produce 2D plot of available datasets vs. openness of available datasets -----
min_openness <- min(df_canton$open_score_wo_nduc_canton) - 0.15
med_openness <- median(df_canton$open_score_wo_nduc_canton)
max_openness <- 3 + 0.15
min_count <- min(df_canton$count_available_canton) - 0.5
med_count <- median(df_canton$count_available_canton)
max_count <- max(df_canton$count_available_canton) + 0.5

plt_data_comparison <- plot_comparison(df_canton, theme_options,
                                       min_openness, med_openness, max_openness,
                                       min_count, med_count, max_count, 0.05,
                                       0.2)
plt_data_comparison <- plotlyfy_w_zoom(plt_data_comparison)
plt_data_comparison


# Produce 2D plot of available datasets vs. openness of available datasets,
# showing only a subset of the data (zoomed in)
min_openness <- med_openness - (3.01 - med_openness)
max_openness <- 3.01
min_count <- med_count - (max_count - med_count)

plt_data_comparison_subset <- plot_comparison(df_canton, theme_options,
                                       min_openness, med_openness, max_openness,
                                       min_count, med_count, max_count, 0.01,
                                       0.2)
plt_data_comparison_subset <- plotlyfy_w_zoom(plt_data_comparison_subset)
plt_data_comparison_subset


# Change detection -------------------------------------------------------------

# Find the CSV file from four weeks ago
csv_files <- list.files(path="data",
                        pattern="^\\d{4}-\\d{2}-\\d{2}-geodienste-ch\\.csv$",
                        full.names=TRUE)
csv_files <- str_sort(csv_files, decreasing=TRUE)
recent_csv_file <- csv_files[5]

# Read and clean the previous CSV file
df_recent <- read_delim(recent_csv_file, delim = ";", na = c("{}", "''", '""', ""))
df_recent <- clean_data(df_recent)
quality_assurance_after_import(df_recent)

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
