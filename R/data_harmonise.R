# R/data_harmonise.R
#
# Reshape one row per (canton, topic) into two rows: one for the data-
# download offering and one for the WMS offering, with harmonised
# attribute names (publication_type, contract_required).
#
# Copied verbatim from functions.R::harmonise_data_and_wms_atts. The
# original is preserved at the project root so the legacy scripts
# (analyse.R, time-series.R) keep working under the equivalence
# harness's scripts mode.

suppressPackageStartupMessages({
  library(dplyr)
})

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
