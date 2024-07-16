get_table_prius <- function(
    path = "data/raw",
    colname_prefix = "prius_"
) {
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
