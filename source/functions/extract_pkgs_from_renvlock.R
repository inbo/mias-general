extract_pkgs_from_renvlock <- function(lockfile_path = "renv.lock") {
  # Read lockfile
  lock <- jsonlite::fromJSON(lockfile_path, simplifyVector = FALSE)

  # Extract R information
  data_R <- data.frame(
    name = "R",
    version = lock$R$Version
  )

  # Extract packages and package information
  pkg <- lock$Packages
  data_pkg <- data.frame(
    name = purrr::map(pkg, "Package") |> unlist() |> unname(),
    version = purrr::map(pkg, "Version") |> unlist() |> unname()
  ) |>
    dplyr::arrange(name, .locale="en")

  data_list <- setNames(list(data_R, data_pkg), c("R", "pkg"))

  return(data_list)
}
