rm(list = ls())
#
# https://tutorials.inbo.be/tutorials/spatial_point_in_polygon/

# -------------------------------------------------------------
#
#
# download maps
countries <- c("Belgium", "France", "Germany", "Luxembourg", "Netherlands")
gis_data_path <- "data/gis/eea_reference_grid"
if (FALSE) {
  eea_rgrid_url <- file.path(
    "https://www.eea.europa.eu",
    "data-and-maps/data/eea-reference-grids-2/gis-files/"
  )
  lapply(countries, function(x) {
    download.file(
      paste(
        eea_rgrid_url,
        paste0(tolower(x), "-spatialite"),
        "at_download",
        "file", sep = "/"
      ),
      destfile = file.path(gis_data_path, paste0(x, "_spatialite.zip")),
      mode = "wb")
    unzip(
      zipfile = file.path(gis_data_path, paste0(x, "_spatialite.zip")),
      exdir = gis_data_path
      )
    file.remove(file.path(gis_data_path, paste0(x, "_spatialite.zip")))
  })
}
#
# read maps
map_list <- lapply(countries, function(x) {
  sf::st_read(
    dsn = paste0(gis_data_path, "/", x, ".sqlite"),
    layer = sf::st_layers(dsn = paste0(gis_data_path, "/", x, ".sqlite")) |>
      purrr::pluck("name") |>
      grep(pattern = "1km$", x = _, value = TRUE)
  )
})
#
# check coordinate reference system
crs_check <- lapply(map_list, function(x) {
  sf::st_crs(x)$Name
})
crs_check
#
# get polygon flanders + buffer 30 km
fla_buffer <- sf::st_read("data/gis/fla_buffer_30000m.shp")
sf::st_crs(fla_buffer)$input # wgs84
#
# transform crs from wsg84 to laea (the eea reference grid crs)
fla_buffer_laea <- fla_buffer |> sf::st_transform(
  x = _,
  crs = paste0("EPSG:", sf::st_crs(map_list[[1]])$epsg)
)
#
# create intersection of each country map with the fla_buffer_laea_obj
map_list_reduced <- lapply(map_list, function(x) {
  sf::st_intersection(x, fla_buffer_laea)
})
#
# join reduced country maps
map_tmp <- dplyr::bind_rows(map_list_reduced)
map_union <- map_tmp |> dplyr::distinct(cellcode, .keep_all = TRUE)
#
# check plot
ggplot2::ggplot() +
  ggplot2::geom_sf(data = map_union, color = "red", size = 0.2) +
  ggplot2::geom_sf(data = fla_buffer_laea, fill = "yellow", alpha = .3)
#
# save map_union
sf::st_write(
  obj = map_union,
  dsn = paste0("data/gis/eea_fla_", buffer, "m.shp")
)
