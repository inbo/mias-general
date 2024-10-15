rm(list = ls())
#
# -------------------------------------------------------------------------
#
files_source <- list.files(
    path = "source/report/formal/species_overview/sections",
    pattern = "skeleton",
    full.names = TRUE
)
files_destination <- files_source |>
  gsub(pattern = "_skeleton", replacement = "", x = _)
file.copy(from = files_source, to = files_destination, overwrite = TRUE)

