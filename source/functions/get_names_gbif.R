get_names_gbif <- function(
    keys
) {
  purrr::map(
    .x = keys,
    .f = rgbif::name_usage
  ) |>
    purrr::map(
      .x = _,
      .f = function(x) {
        purrr::pluck(.x = x, "data")
      }
    ) |>
    dplyr::bind_rows()
}
