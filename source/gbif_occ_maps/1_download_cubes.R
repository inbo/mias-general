rm(list = ls())
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
# CHECK: https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html


sf::st_write(
  obj = fla_buffer,
  dsn = paste0("data/gis/fla_buffer_", buffer, "m.shp")
  )
#
# define remaining sql parameters
species_names <- get(
  load("data/processed/names_prius.Rda")
) |>
  purrr::pluck("data")  |>
  dplyr::pull(dplyr::contains("scientific"))
species_keys <- rgbif::name_backbone_checklist(species_names) |>
  dplyr::pull("usageKey")
# species_keys %in% taxon_keys$data$key_accepted |> sum()
year_end <- lubridate::year(Sys.Date())
year_begin <- year_end - 10

species_keys <- species_keys[1:2] ## REMOVE

sql_query <- write_sql_query_occcubes(
  species_keys = species_keys,
  year_begin = year_begin,
  year_end = year_end,
  polygon_wtk = fla_borders_buffer_txt
)
# write.table(sql_query, "sql_test.txt")

# download
gbif_download_key <- do.call(
  eval(parse(text = "rgbif::occ_download_sql")),
  list(q = sql_query)
)

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




# paths
gis_data_path <- "data" #"G:/Mijn Drive/gis_data" ## change back
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
# remove:
if (!file.exists(occ_file)) {
  unzip(zipfile = zip_file,
        files = occ_file,
        exdir = occcubes_data_path)
}
#



