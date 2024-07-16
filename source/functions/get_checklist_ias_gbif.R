get_checklist_ias_gbif <- function(
    taxonomic_status = "ACCEPTED"
) {
  rgbif::name_usage(
    datasetKey = "79d65658-526c-4c78-9d24-1870d67f8439",
    limit = 1000
  ) |>
    (`[[`)("data") |>
    dplyr::filter(
      get("origin") == "SOURCE",
      get("taxonomicStatus") == taxonomic_status
    ) |>
    dplyr::arrange(
      get("nubKey")
    )
}
