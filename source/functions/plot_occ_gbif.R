plot_occ_gbif <- function(
    occ_data,
    mapdata = ggplot2::map_data("world", region = "Belgium")
){
  occ_map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = mapdata,
      ggplot2::aes(x = long, y = lat),
      fill = NA,
      color = "black",
      alpha = .5
    ) +
    ggplot2::geom_point(
      data = occ_data$data,
      ggplot2::aes(x = decimalLongitude, y = decimalLatitude),
      color = "coral",
      size = 2) +
    ggplot2::labs(
      title = paste("Occurrences of", occ_data$hierarchy[[1]] |> dplyr::filter(rank == "species") |> dplyr::pull(name), "in Flanders"),
      subtitle = paste("Total count =", occ_data$meta$count),
      x = "Longitude",
      y = "Latitude") +
    ggplot2::theme_minimal()
  return(occ_map)
}
