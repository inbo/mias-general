download_table_prius <- function(
    path = "data/raw"
) {
  #' Download the PrIUS table from Zenodo
  #'
  #' @description \code{download_table_prius} applies the function \code{\link[inborutils]{download_zenodo}} from the \href{https://inbo.github.io/inborutils/}{inborutils} package to download the table from the PrIUS report from Zenodo (\href{https://zenodo.org/records/7678524}{website}). Of the files available on Zenodo only the .csv file is written to the provided path.
  #'
  #' @param path A character string holding a path name
  #'
  #' @examples
  #'
  #' \dontrun{
  #' download_table_prius(
  #'   path = "data/raw"
  #' )
  #' }
  #'
  #' @export
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
