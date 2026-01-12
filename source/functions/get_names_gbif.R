get_names_gbif <- function(
    keys
) {
  #' Get taxon-level names from GBIF
  #'
  #' @description \code{get_names_gbif} applies the function \code{\link[rgbif]{name_usage}} from the \href{https://cran.r-project.org/web/packages/rgbif/}{rgbif} package to a vector of taxon keys to get taxon-level names from GBIF.
  #'
  #' @param keys A numeric or character vector of GBIF taxon keys
  #'
  #' @return A tibble containing taxon-level names and additional information.
  #'
  #' @examples
  #'
  #' \dontrun{
  #' names_gbif <- get_names_gbif(
  #'   keys = 3170247
  #' )
  #'
  #' names_gbif <- get_names_gbif(
  #'   keys = c(3170247, 2706080)
  #' )
  #' }
  #'
  #' @export
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
