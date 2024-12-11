process_expertsheet <- function(
    sheet_id,
    tab_variablename = "soortengroep",
    colnames_old,
    colnames_new,
    gbif_namevariable = "sci_name"
){
  #
  # sheet tab names
  sheet_tab_names <- googlesheets4::sheet_names(ss = sheet_id)
  #
  # read in sheet tabs
  sheet_list <- lapply(
    sheet_tab_names,
    function(i){
      googlesheets4::read_sheet(
        ss = sheet_id,
        sheet = i,
        .name_repair = function(x) {
          # rename variables
          gsub(pattern = "\\s+|-", replacement = " ", x)
        }
      ) |>
        # add tab names as variable
        dplyr::mutate(tab_variablename := i, .before = 1)
    }
  )
  #
  # create data frame
  sheet_data <- sheet_list |> dplyr::bind_rows()
  #
  # rename columns
  colnames_comb <- setNames(colnames_old, colnames_new)
  sheet_data <- sheet_data |> dplyr::rename(!!!colnames_comb)
  #
  # get GBIF data
  gbif_data <- rgbif::name_backbone_checklist(name_data = sheet_data[, gbif_namevariable]) |>
    dplyr::mutate(
      key_acc = ifelse(status == "SYNONYM", acceptedUsageKey, usageKey)
    )
  #
  # get GBIF accepted names
  gbif_names <- get_names_gbif(gbif_data$key_acc)
  #
  # combine data
  sheet_data_gbif <- sheet_data |>
    dplyr::mutate(
      !!paste0(gbif_namevariable, "_gbif") := gbif_data$scientificName,
      !!paste0("status_", gbif_namevariable, "_gbif") := gbif_data$status,
      !!paste0(gbif_namevariable, "_gbif_acc") := gbif_names$scientificName,
      !!paste0("status_", gbif_namevariable, "_gbif_acc") := gbif_names$taxonomicStatus,
      .after = gbif_namevariable
    ) |>
    dplyr::mutate(
      key_acc_gbif = gbif_data$key_acc
    )
  #
  #
  return(sheet_data_gbif)
}


