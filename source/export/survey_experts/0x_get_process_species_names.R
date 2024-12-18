# --- get and process species information -------------
#
# get species names
# (there will be one form per species)
species <- do.call(
  process_speciessheet,
  species_sheet_args
)
#
# test: all species unique
assertthat::are_equal(
  species$key_acc_gbif |> unique() |> length(),
  nrow(species)
)
#
# create gsheet for review (esp. scientific names)
sheetname <- googledrive::drive_ls(
  pattern = "^experts",
  type = "spreadsheet",
  shared_drive = "PRJ_MIUS"
) |> dplyr::pull(name)
googlesheets4::gs4_create(name = paste0("CONTROL_", sheetname, " (don't edit)"), sheets = species)
# move sheet to target folder
tmp_id <- googledrive::drive_find(
  pattern = paste0("CONTROL_", sheetname),
  type = "spreadsheet"
) |> googledrive::as_id()
googledrive::drive_mv(
  file = tmp_id,
  path = distribution_folder_url |> paste0(x = _, "/")
)
#
# rename 'invasieve duizenknopen' and 'vespa velutina'
knotweed_names <- species |>
  dplyr::filter(grepl("duizendknopen", species)) |>
  dplyr::pull(sci_name_gbif_acc) |>
  paste(x = _, collapse = ", ")
vespa_velutina_name <- "Vespa velutina Lepeletier, 1836"
species_upd <- species |>
  dplyr::mutate(
    sci_name_gbif_upd = dplyr::case_when(
      grepl("duizendknopen", species) ~ knotweed_names,
      grepl("Vespa velutina", sci_name_gbif_acc) ~ vespa_velutina_name,
      TRUE ~ sci_name_gbif_acc
    )
  ) |>
  dplyr::distinct(sci_name_gbif_upd, .keep_all = TRUE)
