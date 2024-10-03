get_checklist_ias_gbif <- function(
    taxonomic_status = "ACCEPTED"
) {
  #' Get the checklist of invasive alien species of Union concern from GBIF
  #'
  #' @description \code{get_checklist_ias_gbif} applies the function \code{\link[rgbif]{name_usage}} from the \href{https://cran.r-project.org/web/packages/rgbif/}{rgbif} package to get the
  #' "List of Invasive Alien Species of Union concern" checklist data set from GBIF (\href{https://www.gbif.org/dataset/79d65658-526c-4c78-9d24-1870d67f8439}{website}). The code is based on \href{https://inbo.github.io/tutorials/tutorials/r_gbif_checklist/}{this INBO tutorial}.
  #'
  #' @param taxonomic_status A character string or vector specifying which taxonomic status should apply. Possible values are "ACCEPTED" and or "SYNONYM"
  #'
  #' @return A tibble containing the checklist data set.
  #'
  #' @examples
  #'
  #' \dontrun{
  #' checklist_ias <- get_checklist_ias_gbif(
  #'   taxonomic_status = "ACCEPTED"
  #' )
  #'
  #' checklist_ias <- get_checklist_ias_gbif(
  #'   taxonomic_status = c("ACCEPTED", "SYNONYM")
  #' )
  #' }
  #'
  #' @export
  rgbif::name_usage(
    datasetKey = "79d65658-526c-4c78-9d24-1870d67f8439",
    limit = 1000
  ) |>
    (`[[`)("data") |>
    dplyr::filter(
      get("origin") == "SOURCE",
      get("taxonomicStatus") %in% taxonomic_status
    ) |>
    dplyr::arrange(
      get("nubKey")
    )
}
