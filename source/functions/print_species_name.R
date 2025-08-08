# print species names from species table
# format species names for printing
print_species_name <- function(
    species_table,
    name_expr = expression(c("*", species, " [EN: ", vern_name_eng, ", NL: ", vern_name_nld, "]", "*"))
) {
  species_table |>
    dplyr::rowwise() |>
    dplyr::mutate(
      tmp = paste0(eval(expr = name_expr), collapse = "")
    ) |>
    dplyr::arrange(tmp) |>
    dplyr::pull(tmp) |>
    gsub("\\\\&", "&", x = _)
}
