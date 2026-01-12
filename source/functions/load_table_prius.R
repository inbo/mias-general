load_table_prius <- function(
    path = "data/raw",
    colname_prefix = "prius_"
) {
  #' Load the PrIUS table
  #'
  #' @description \code{load_table_prius} load the table from the PrIUS report by reading the .csv file at the provided path. To first download the table use the function \code{download_table_prius} from this collection.
  #'
  #' @param path A character string holding a path name
  #' @param colname_prefix A character string holding a prefix to be added to all column names. Provide an empty string or set to \code{NULL} if no prefix should be added.
  #'
  #' @return A data.frame containing the PrIUS table.
  #'
  #' @examples
  #'
  #' \dontrun{
  #' table_prius <- load_table_prius(
  #'   path = "data/raw",
  #'   colname_prefix = "prius_"
  #' )
  #' }
  #'
  #' @export
  table_raw <- read.csv2(
    paste(path, "dhondt_etal_2022_prius_table.csv", sep = "/"),
    sep = ","
    )
  table_upd <- table_raw |>
    dplyr::mutate(
      nubKey =  get("GBIF_codes") |>
        substr(start = 1, stop = 7) |>
        as.numeric(),
      .before = 1
    ) |>
    dplyr::rename(
      scientificName = "species"
    ) |>
    dplyr::arrange(
      get("nubKey")
    )
  setNames(table_upd, paste0(colname_prefix, names(table_upd)))
}
