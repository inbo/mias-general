plot_occ_gbif <- function(
    occ_data_sf,
    map_data_sf,
    plot_title,
    plot_subtitle
) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_data_sf) +
    ggplot2::geom_sf(data = occ_data_sf, color = "coral") +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle) +
    ggplot2::theme_minimal()
}
