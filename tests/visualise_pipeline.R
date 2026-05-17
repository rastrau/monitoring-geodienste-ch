#!/usr/bin/env Rscript
# tests/visualise_pipeline.R
#
# Render the {targets} DAG to an interactive HTML widget for review.
# Not part of the pipeline itself — run manually when you want to inspect
# the dependency graph, e.g. before opening a PR.
#
# Usage:
#   LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 Rscript tests/visualise_pipeline.R
#
# Opens the widget in your default browser (interactive sessions) and/or
# writes it to /tmp/geodienste-pipeline.html.

suppressPackageStartupMessages({
  library(targets)
  library(here)
  library(htmlwidgets)
})

setwd(here())

widget <- tar_visnetwork(targets_only = TRUE)

out <- file.path(tempdir(), "geodienste-pipeline.html")
# selfcontained = FALSE writes a sibling _files/ directory but doesn't
# require pandoc; pandoc isn't on PATH for plain Rscript invocations.
saveWidget(widget, out, selfcontained = FALSE)
message("Wrote DAG snapshot to: ", out)

if (interactive()) print(widget)
