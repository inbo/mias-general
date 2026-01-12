rm(list = ls())

# path to locally saved processed response data
response_data_path <- "data/survey_experts/"
#
#
#
# --- load response data ---------------
#
res_comb_upd <- get(load(paste0(response_data_path, "results_combined_upd.rda")))
res_meth_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda")))
res_moni_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda")))
#
#
# --- rename columns ---------------
#
rename_cols_NL_EN <- function(
    dataframe
){
  # EN
  colnames_EN <- grep("EN", colnames(dataframe), value = TRUE)
  colnames_EN_index <- which(
    colnames(dataframe) %in% colnames_EN
  )
  colnames_EN_upd <- colnames(dataframe)[colnames_EN_index] |> gsub("_EN", "", x = _)
  message(
    paste("column names\n", paste(colnames_EN, collapse = ", "),
          "\nare renamed to\n", paste(colnames_EN_upd, collapse = ", "))
  )
  # NL
  colnames_NL_index <- which(
    colnames(dataframe) %in% (colnames_EN_upd)
  )
  colnames_NL <- colnames(dataframe)[colnames_NL_index]
  colnames_NL_upd <- colnames(dataframe)[colnames_NL_index] |> paste0(x = _, "_NL")
  message(
    paste("column names\n", paste(colnames_NL, collapse = ", "),
          "\nare renamed to\n", paste(colnames_NL_upd, collapse = ", "))
  )
  #
  colnames(dataframe)[colnames_NL_index] <- colnames_NL_upd
  colnames(dataframe)[colnames_EN_index] <- colnames_EN_upd
  return(dataframe)
}
#
#
res_comb_upd <- res_comb_upd |> rename_cols_NL_EN()
res_meth_recoded <- res_meth_recoded |> rename_cols_NL_EN()
res_moni_recoded <- res_moni_recoded |> rename_cols_NL_EN()
#
#
# --- save updated data ---------------
#
save(res_comb_upd, file = paste0(response_data_path, "results_combined_upd.rda"))
save(res_meth_recoded, file = paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda"))
save(res_moni_recoded, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda"))

