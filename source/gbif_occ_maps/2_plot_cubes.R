rm(list = ls())
#
# -------------------------------------------------------------
#
# get shapes
fla_1km <- sf::st_read("data/gis/prius/vla_1km.geojson")
fla_borders <- sf::st_read("data/gis/prius/flanders_wgs84.geojson")
prov_borders <- sf::st_read("data/gis/prius/Provincies.geojson")
ps_vglrl <- sf::st_read("data/gis/prius/ps_vglrl.geojson")
ps_hbtrl_deel <- sf::st_read("data/gis/prius/ps_hbtrl_deel.geojson")
water <- sf::st_read("data/gis/prius/waterlopen.geojson")
#
#
# get & filter cube data
cube <- readr::read_tsv(file = "data/gbif_occcubes/0026520-241126133413365.csv")|>
  dplyr::filter(withinpolygon == TRUE)

#
species_info <- get_names_gbif(cube$specieskey |> unique())

cube_sub <- species_info |>
  dplyr::left_join(cube, by = c("key" = "specieskey", "species", "class"))
# maps
#
plot_map  <- function(
    data_cube = ias_cube_1km_spec_i,
    data_fla_borders = fla_borders,
    data_prov_borders = prov_borders,
    data_ps_vglrl = ps_vglrl,
    data_ps_hbtrl_deel = ps_hbtrl_deel,
    data_water = water,
    plot_title = NULL,
    plot_subtitle = NULL,
    col_prior = "#FFD700",
    col_water ="#0000FF",
    col_occ = "#EA5F94"
){
  plot_map <- ggplot2::ggplot() +
    #ggplot2::geom_sf(data = data_fla_borders, fill = NA, size = 0.2) +
    #ggplot2::geom_sf(data = data_prov_borders, fill = NA, size = 0.2) +
    ggplot2::geom_sf(data = data_ps_vglrl, colour = col_prior, fill = col_prior, alpha = .3) +
    ggplot2::geom_sf(data = data_ps_vglrl, colour = col_prior, fill = col_prior, alpha = .3) +
    ggplot2::geom_sf(data = data_water, colour = col_water, fill = col_water, alpha = .5, size = 0.5) +
    ggplot2::geom_sf(data = data_cube, colour = col_occ, fill = col_occ, alpha = .5, size = 0.5) +
    ggplot2::theme_void() + # Zonder "assen" (coördinaten)
    #ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank()) +
    #ggplot2::theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    ggplot2::coord_sf() +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle)
}

name_i = "Aedes albopictus"

names_in_cube <- cube_sub$canonicalName |> unique() |> sort()
plot_list <- list()
for (name_i in names_in_cube){
  keys_i <- species_info |>
    dplyr::filter(canonicalName == name_i) |>
    dplyr::pull(key)
  cube_sub_i <- cube_sub |>
    dplyr::filter(key %in% keys_i)
  # HERE: make sure buffer is kept
  # EUROPE & cut out ploygon?
  ias_cube_1km_spec_i <- merge(fla_1km,
                               cube_sub_i,
                               by.x = 'CELLCODE',
                               by.y ='eeacellcode',
                               all.y = TRUE)
  plot_i <- plot_map(
    plot_title = paste0("Distribution of ", name_i, ""),
    plot_subtitle = "X occurrences from [date] to [date]"
  )
  plot_list <- append(plot_list, list(plot_i))
}
#
#
save(plot_list, file = "media/gbif_occcubes/plot_list.rda")


