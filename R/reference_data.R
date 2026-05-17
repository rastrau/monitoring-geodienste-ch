# R/reference_data.R
#
# Loaders for CSV-driven reference tables in data/reference/.
# These replace the inline case_when() lookups that used to live in
# functions.R; adding a new topic / publication type is now a CSV edit.

suppressPackageStartupMessages({
  library(readr)
})

read_topic_shortnames <- function(path) {
  read_csv(path, show_col_types = FALSE,
           col_types = cols(.default = col_character()))
}

read_openness_scores <- function(path) {
  read_csv(path, show_col_types = FALSE,
           col_types = cols(
             publication_type = col_character(),
             open_score = col_double()
           ))
}

read_cantons <- function(path) {
  read_csv(path, show_col_types = FALSE,
           col_types = cols(canton = col_character()))$canton
}
