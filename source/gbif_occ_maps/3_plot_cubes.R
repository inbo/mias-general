rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
gis_data_path <- "data/gis"
#
# -------------------------------------------------------------
#
# get map data
map_fla_1km <- sf::st_read(
  file.path(gis_data_path, "prius/vla_1km.geojson")
  )
map_fla_borders <- sf::st_read(
  file.path(gis_data_path, "prius/flanders_wgs84.geojson")
  )
map_bg <- sf::st_read(
  file.path(gis_data_path, "eea_fla_30000m.shp")
  )
map_bg_borders <- sf::st_read(
  file.path(gis_data_path, "fla_buffer_30000m.shp")
  )
#
#
# check crs
sf::st_crs(map_fla_1km)$input
sf::st_crs(map_fla_borders)$input
sf::st_crs(map_water)$input
sf::st_crs(map_bg)$input
sf::st_crs(map_bg_borders)$input
#
# transform to wgs84
map_bg_wgs84 <- map_bg |> sf::st_transform(
  x = _,
  crs = paste0("EPSG:", sf::st_crs(map_fla_1km)$epsg)
)
#
# get occurrence cube data
# GBIF.org (17 December 2024)
# GBIF Occurrence Download https://doi.org/10.15468/dl.63mdsh
cube <- readr::read_tsv(file = "data/gbif_occcubes/0037665-241126133413365.csv")
cube_poly <- cube |> dplyr::filter(withinpolygon == TRUE)
#
# get species information
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
      grepl("Vespa velutina", sci_name_gbif_acc) ~
        rgbif::name_backbone(vespa_velutina_name)$usageKey,
      TRUE ~ key_acc_gbif
    )
  )
#
# check species
keys_common <- intersect(
  cube_poly$specieskey |> unique(),
  species_upd$key_acc_gbif
  )
assertthat::are_equal(keys_common, cube_poly$specieskey |> unique()) # same
assertthat::are_equal(keys_common, species_upd$key_acc_gbif) # not the same
#
# merge occurrence cube and additional species data
cube_upd <- dplyr::left_join(
  cube_poly,
  species_upd,
  by = c("specieskey" = "key_acc_gbif")
)
#
# keys invasieve duizendknopen
knotweed_keys <- species_upd |>
  dplyr::filter(grepl("duizendknopen", opmerking)) |>
  dplyr::pull(key_acc_gbif)
knotweed_names <- species_upd |>
  dplyr::filter(key_acc_gbif %in% knotweed_keys) |>
  dplyr::pull(sci_name_gbif_acc)
#
# summarize knotweed occurrences
cube_knotweed <- cube_upd |> dplyr::filter(specieskey %in% knotweed_keys)
assertthat::are_equal(
  cube_knotweed$sci_name_gbif_acc |> unique(),
  knotweed_names
)
cube_knotweed_sum <- cube_knotweed |> dplyr::summarise(
  occurrences = sum(occurrences),
  .by = eeacellcode
) |>
  dplyr::mutate(
  sci_name_gbif_acc = paste(knotweed_names, collapse = ", ")
)
species_upd <- species_upd |>
  dplyr::mutate(
    sci_name_gbif_acc = dplyr::case_when(
      key_acc_gbif %in% knotweed_keys ~ paste(knotweed_names, collapse = ", "),
      TRUE ~ sci_name_gbif_acc
    )
  ) |>
  dplyr::distinct(sci_name_gbif_acc)
#
# final cube: sum and add knotweed
cube_sum <- dplyr::bind_rows(
  cube_upd |>
    dplyr::filter(!specieskey %in% knotweed_keys) |>
    dplyr::summarise(
      occurrences = sum(occurrences),
      .by = c(eeacellcode, sci_name_gbif_acc)
    ),
  cube_knotweed_sum
)
all(cube_sum$sci_name_gbif_acc %in% species_upd$sci_name_gbif_acc)
all(species_upd$sci_name_gbif_acc %in% cube_sum$sci_name_gbif_acc)
#
# plot function
plot_map  <- function(
    data_cube,
    data_bg_borders = map_bg_borders,
    data_fla_borders = map_fla_borders,
    plot_title,
    plot_subtitle,
    col_occ = "#EA5F94",
    transform = FALSE
) {
  if (transform) {
  # transform to epsg:3857 used by open street maps
  data_cube <- sf::st_transform(x = data_cube, crs = "EPSG:3857")
  data_bg_borders <- sf::st_transform(x = data_bg_borders, crs = "EPSG:3857")
  data_fla_borders <- sf::st_transform(x = data_fla_borders, crs = "EPSG:3857")
  }
  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(
      type = "cartolight", zoom = 9, cachedir = tempdir(), alpha = .8
      ) +
    ggplot2::geom_sf(
      data = data_bg_borders,
      fill = NA, size = 0.2, color = "black", linetype = "dashed"
      ) +
    ggplot2::geom_sf(
      data = data_fla_borders,
      fill = NA, size = 0.2, color = "black", linetype = "solid"
      ) +
    ggplot2::geom_sf(
      data = data_cube,
      colour = col_occ, fill = col_occ, alpha = 1, size = 1
      ) +
    ggplot2::theme_void() + # remove axes
    ggplot2::coord_sf() +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle)
}
#
# loop over species
plot_filepaths <- data.frame(
  species = character(),
  path_to_map = character()
)
for (species_i in species_upd$sci_name_gbif_acc){
  cube_sum_i <- cube_sum |>
    dplyr::filter(sci_name_gbif_acc == species_i)
  map_cube_i <- merge(
    map_bg_wgs84,
    cube_sum_i,
    by.x = "cellcode",
    by.y = "eeacellcode"
    )
  #
  # ggplot2
  plot_i <- plot_map(
    data_cube = map_cube_i,
    plot_title = species_i,
    plot_subtitle = paste(
      cube_sum_i |> dplyr::distinct(eeacellcode) |> nrow(),
      "grid cells with occurrences between",
      cube_poly$year |> min(),
      "and",
      cube_poly$year |> max()
      )
    )
  filepath_i <- paste0(
    "media/gbif_occcubes/plots_ggspatial/",
    species_i |>
      gsub(pattern = "\\s+|-", replacement = "_", x = _) |>
      tolower(),
    ".png")
  ggplot2::ggsave(
    filename =  filepath_i,
    plot = (plot_i + ggplot2::theme(text = ggplot2::element_text(size = 10))),
    width = 1500,
    height = 800,
    units = "px",
    dpi = 200,
    bg = "white"
    )
  plot_filepaths <- rbind(
    plot_filepaths,
    data.frame(species = species_i, path_to_map =  filepath_i)
    )
}
save(plot_filepaths, file = "source/gbif_occ_maps/plot_filepaths.Rda")
