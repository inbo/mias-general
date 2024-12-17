rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
gis_data_path <- "data"
#
# -----------------------------------------------------------------------------
#
# define polygon flanders + buffer
buffer <- 1000*30 # in meters
fla <- sf::st_read("data/gis/prius/flanders_wgs84.geojson") # flanders
fla_buffer <- sf::st_buffer(x = fla, dist = buffer)
if (FALSE) {
  plot_check <- ggplot2::ggplot() + # plot to check
    ggplot2::geom_sf(data = fla_buffer, fill = "blue", size = 0.2) +
    ggplot2::geom_sf(data = fla, fill = "coral", size = 0.2)
}
fla_buffer_txt <- fla_buffer |>
  sf::st_geometry() |>
  sf::st_as_text() |>
  wk::wkt() |>
  wk::wk_orient()
sf::st_write(
  obj = fla_buffer,
  dsn = paste0("data/gis/fla_buffer_", buffer, "m.shp")
  )
#
# species list
species_sheetid_args <- list(
  sheet_id = "1dClhdsk1QMHniYv6xcFVTKd6pLtWBDz-8avvK-xZHQ0",
  tab_variablename = "soortengroep",
  colnames_old = c("soort", "wetenschappelijke naam"),
  colnames_new = c("species", "sci_name"),
  gbif_namevariable = "sci_name"
)
species <- do.call(
  process_expertsheet,
  species_sheetid_args
)
#
# vespa velutina
# use key of https://www.gbif.org/species/1311477
# instead of https://www.gbif.org/species/6247411 (subspecies)
vespa_velutina_name <- "Vespa velutina Lepeletier, 1836"
species_upd <- species |>
  dplyr::mutate(
    sci_name_gbif_acc = dplyr::case_when(
      grepl("Vespa velutina", sci_name_gbif_acc) ~ vespa_velutina_name,
      TRUE ~ sci_name_gbif_acc
        ),
    key_acc_gbif = dplyr::case_when(
      grepl("Vespa velutina", sci_name_gbif_acc) ~ rgbif::name_backbone(vespa_velutina_name)$usageKey,
      TRUE ~ key_acc_gbif
    )
  )
#
species_upd <- species_upd |>
  dplyr::select(dplyr::intersect(dplyr::contains("gbif"), dplyr::contains("acc")))
#
# define remaining sql parameters
year_end <- lubridate::year(Sys.Date())
year_begin <- year_end - 10
#
# write sql query
sql_query <- write_sql_query_occcubes(
  species_keys = species_upd |> dplyr::pull(dplyr::contains("key")),
  year_begin = year_begin,
  year_end = year_end,
  polygon_wtk = fla_buffer_txt
)
write.table(sql_query, "sql_test.txt")
#
# download
gbif_download_key <- do.call(
  eval(parse(text = "rgbif::occ_download_sql")),
  list(q = sql_query)
)
#
# check status of download
rgbif::occ_download_meta(key = gbif_download_key) |> purrr::pluck("status")
#
# save download key & metadata
gbif_download_meta <- list(
  key = gbif_download_key,
  sql_query = sql_query,
  date = Sys.Date()
)
save(gbif_download_meta,
     file = paste0(
       "data/raw/gbif_download_",
       gbif_download_meta$key |> as.character(),
       ".rda"
     )
)
#
# paths
occcubes_data_path <- paste(gis_data_path, "gbif_occcubes", NULL, sep = "/")
#
# download zip file
zip_file <- paste0(occcubes_data_path, gbif_download_key, ".zip")
if (!file.exists(zip_file)) {
  occ <- rgbif::occ_download_get(
    key = gbif_download_key,
    path = occcubes_data_path
  )
}
#
# unzip csv
occ_file <- paste0(gbif_download_key, ".csv")
if (!file.exists(occ_file)) {
  unzip(zipfile = zip_file,
        files = occ_file,
        exdir = occcubes_data_path)
  file.remove(zip_file)
}

