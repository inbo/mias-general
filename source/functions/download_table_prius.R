download_table_prius <- function(
    path = "data/raw"
) {
  inborutils::download_zenodo(
    doi = "10.5281/zenodo.7678524",
    path = path
  )
  # remove xlsx file
  list.files(path, full.names = TRUE) |>
    grep(pattern = "prius.*xlsx", value = TRUE, ignore.case = TRUE) |>
    file.remove()
  # rename csv file to snake case
  filename <- list.files(path, full.names = TRUE) |>
    grep(pattern = "prius.*csv", value = TRUE, ignore.case = TRUE)
  file.rename(
      from = filename,
      to = tolower(filename)
    )
}
