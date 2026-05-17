# R/reference_data.R
#
# Loaders for CSV-driven reference tables in data/reference/.
# These replace the inline case_when() lookups that used to live in
# functions.R; adding a new topic / publication type is now a CSV edit.

suppressPackageStartupMessages({
  library(readr)
})

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
