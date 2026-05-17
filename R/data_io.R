# R/data_io.R
#
# CSV file I/O: reading the geodienste-ch snapshot files and enumerating
# the archive directory.

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(stringr)
})

# Read a geodienste-ch CSV with the same parsing options as the existing
# scripts (analyse.R:16, time-series.R:12).
read_geodienste_csv <- function(path) {
  read_delim(path, delim = ";", na = c("{}", "''", '""', ""),
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
list_archive_csvs <- function(data_dir = here("data"),
                              decreasing = FALSE) {
  files <- list.files(path = data_dir,
                      pattern = "^\\d{4}-\\d{2}-\\d{2}-geodienste-ch\\.csv$",
                      full.names = TRUE)
  str_sort(files, decreasing = decreasing)
}

# Pick the 4-weeks-ago CSV by position, exactly as analyse.R:249 does. Expects
# a descending-sorted file list. The selection rule is intentionally
# preserved at this step; revisiting it (date-based selection) is a
# separate, behaviour-changing step.
pick_recent_csv <- function(csv_files_desc) {
  csv_files_desc[5]
}
