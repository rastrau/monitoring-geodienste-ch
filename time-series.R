library(here)
library(ggplot2)
library(purrr)

source(here("config.R"), encoding = "UTF-8")
source(here("functions.R"), encoding = "UTF-8")


# Ingest historic CSVs
df <-
  list.files(path = here("data"), pattern = "2.*\\.csv$", full.names = TRUE) %>%
  map_df(~ read_delim(., delim = ";", na = c("{}", "''", '""', "")))
df


# Clean and transform the raw data ---------------------------------------------

df <- clean_data(df)
quality_assurance_after_import(df)

# Transform data into long format
df <- harmonise_data_and_wms_atts(df)

# Compute openness scores (for data and for WMS) per topic ---------------------
df <- compute_openness_per_topic(df)

# Remove records of "Keine Daten" and "Im Aufbau" and remove WMS offerings
df <- df %>%
  filter(publication_type != "Keine Daten") %>%
  filter(publication_type != "Im Aufbau") %>%
  filter(offering != "WMS") %>%
  filter(canton != "FL") %>%
  filter(updated >= "2023-06-06")


# Analyse openness scores and number of datasets per canton over time ----------

df_timeseries <- df %>%
  group_by(updated, canton) %>%
  summarise(
    number_available_datasets = n(),
    mean_open_score = mean(open_score)) %>%
  ungroup()


# Visualize the data -----------------------------------------------------------

facet_plot_theme_options <- theme_options +
  theme(
    panel.grid.major.y = element_line(color = "#DDDDDD", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "#DDDDDD", linewidth = 0.1),
    panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.2),
    axis.text.x = element_text(angle = 55, hjust = 1),
    strip.text = element_text(face = "bold")
  )

# Ensure the data is sorted by date
df_timeseries <- df_timeseries %>% arrange(updated)

# Create a faceted plot of the number of available datasets
lineplot_number <- ggplot(df_timeseries, aes(x = updated, y = number_available_datasets)) +
  geom_line(color = "#4575B4", linewidth = 1) +
  labs(x = "Datum",
       y = "Anzahl verfügbarer Datensätze\n") +
  facet_wrap(~canton, ncol=5, scales="fixed") +
  facet_plot_theme_options

# Create a faceted plot of the openness score
lineplot_open_score <- ggplot(df_timeseries, aes(x = updated, y = mean_open_score)) +
  geom_line(color = "#54B987", linewidth = 1) +
  labs(x = "Datum",
       y = "Offenheit der verfügbaren Datensätze\n") +
  facet_wrap(~canton, ncol=5, scales="fixed") +
  facet_plot_theme_options