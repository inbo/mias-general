rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
source('source/export/survey_experts/00_definitions.R')
#
# define folder for recoding sourced out to g-sheets
responses_recoded_folder_url <- "https://drive.google.com/drive/folders/1MdeXChxuP2uMxKTUB3f25wJQafkvT2-8"
#
#
#
# --- load response data ---------------
#
load(paste0(response_data_path, "results_combined.rda"))
#
#
#
# --- add variable question_text_short (EN) ---------------
#
res_comb_upd <- res_comb |>
  dplyr::mutate(
    question_text_short = dplyr::case_match(
      question_id,
      "A1" ~ "Configuratie introductieplaatsen?",
      "A2" ~ "Introductieplaatsen toegankelijk?",
      "A4" ~ "Bijzondere introductieplaatsen?",
      "A5" ~ "Kans op introductie?",
      "A6" ~ "Kans op vestiging?",
      "B1" ~ "Verspreiding gekend?",
      "B2" ~ "Verspreidingspatroon?",
      "B3" ~ "Welke populatiedichtheid?",
      "B4" ~ "Verandering verspreidingsgebieden?",
      "B5" ~ "Verspreidingsgebieden toegankelijk?",
      "B7" ~ "Bijzondere verspreidingsgebieden?",
      "C1" ~ "Impact biodiversiteit?",
      "C2" ~ "Impact biodiversiteit natuurgebieden?",
      "C3" ~ "Impact andere sectoren?",
      "D1" ~ "Welke bemonsteringsmethoden?",
      "D2" ~ "Meest relevante methode?",
      "D3" ~ "Sensitiviteit methode?",
      "D4" ~ "Specificiteit methode?",
      "D5" ~ "Kosten methode?",
      "D6" ~ "Scope methode?",
      "D7" ~ "Veldprotocol beschikbaar?",
      "D8" ~ "Relevante meetnetten?",
      "D9" ~ "Door meetnetten opgepikt?",
      "D10" ~ "Losse waarnemingen representatief?",
      "E1" ~ "Soort beheerd?",
      "E2" ~ "Informatie beheersevaluatie?",
    ),
    .after = question_text
  )
#
#
#
# --- manually recode methods ---------------
#
#
# export to g-sheet for manual recoding
# (only once for given response data)
if (FALSE){
  #
  # search for existing gsheet with already recoded data
  tmp_id <- googledrive::drive_ls(
    path = responses_recoded_folder_url,
    shared_drive = "PRJ_MIUS"
  ) |>
    dplyr::filter(grepl("methods", name)) |>
    dplyr::filter(grepl("JA", name)) |>
    googledrive::as_id()
  #
  # if existing, read gsheet and extract species
  if (length(tmp_id) > 0) {
    tmp_sheet <- googlesheets4::read_sheet(ss = tmp_id)
    species_recoded <- tmp_sheet$species |> unique()
  } else {
    species_recoded <- NULL
  }
  #
  # prepare data frame for manual recoding
  res_to_recode <- res_comb_upd |>
    dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
    dplyr::filter(grepl("D1$|D2", question_id)) |>
    dplyr::mutate(response_text_recoded = NA_character_, .after = response_text) |>
    dplyr::arrange(species, question_id) |>
    dplyr::select(tidyselect::contains(c("species", "question", "response_text"))) |>
    dplyr::filter(!species %in% species_recoded)
  #
  # upload data frame or append to sheet if existing
  if (is.null(species_recoded)) {
    #
    # upload new sheet
    googlesheets4::gs4_create(
      name =  paste0(Sys.Date(), "_methods_recoded"),
      sheets = res_to_recode
    )
    # move sheet to target folder
    tmp_id <- googledrive::drive_find(
      pattern = paste0(Sys.Date(), "_methods_recoded"),
      type = "spreadsheet"
    ) |> googledrive::as_id()
    googledrive::drive_mv(
      file = tmp_id,
      path = responses_recoded_folder_url |> paste0(x = _, "/")
    )
  } else {
    #
    # append new data to sheet
    googlesheets4::sheet_append(
      ss = tmp_id,
      data = res_to_recode
    )
  }
  #
  # in g-sheet recode response_text manually
  # D1: = response_text
  # D1 followup: other methods (should not be one out of the ones given in D1)
  # D2: most relevant method (should be one out of the ones given in D1 / D1fu)
}
#
#
# import recoded methods from g-sheet
tmp <- googledrive::drive_find(
  pattern = "methods_recoded_JA",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  dplyr::arrange(dplyr::desc(name))
tmp_id <- tmp[1,] |>
  googledrive::as_id()
meth_recoded <- googlesheets4::read_sheet(ss = tmp_id)
#
#
#
#
# --- manually recode monitoring schemes ---------------
#
#
# export to g-sheet for manual recoding
# (only once for given response data)
if (FALSE){
  #
  # search for existing gsheet with already recoded data
  tmp_id <- googledrive::drive_ls(
    path = responses_recoded_folder_url,
    shared_drive = "PRJ_MIUS"
  ) |>
    dplyr::filter(grepl("monitoring", name)) |>
    dplyr::filter(grepl("JA", name)) |>
    googledrive::as_id()
  #
  # if existing, read gsheet and extract species
  if (length(tmp_id) > 0) {
    tmp_sheet <- googlesheets4::read_sheet(ss = tmp_id)
    species_recoded <- tmp_sheet$species |> unique()
  } else {
    species_recoded <- NULL
  }
  #
  # prepare data frame for manual recoding
  res_to_recode <- res_comb_upd |>
    dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
    dplyr::filter(grepl("D8", question_id)) |>
    dplyr::mutate(response_text_recoded = NA_character_, .after = response_text) |>
    dplyr::arrange(species, question_id) |>
    dplyr::select(tidyselect::contains(c("species", "question", "response_text"))) |>
    dplyr::filter(!species %in% species_recoded)
  #
  # upload data frame or append to sheet if existing
  if (is.null(species_recoded)) {
    #
    # upload new sheet
    googlesheets4::gs4_create(
      name =  paste0(Sys.Date(), "_monitoring_recoded"),
      sheets = res_to_recode
    )
    # move updated sheet to target folder
    tmp_id <- googledrive::drive_find(
      pattern = paste0(Sys.Date(), "_monitoring_recoded"),
      type = "spreadsheet"
    ) |> googledrive::as_id()
    googledrive::drive_mv(
      file = tmp_id,
      path = responses_recoded_folder_url |> paste0(x = _, "/")
    )
  } else {
    #
    # append new data to sheet
    googlesheets4::sheet_append(
      ss = tmp_id,
      data = res_to_recode
    )
  }
  #
  # in g-sheet recode response_text manually where needed:
  # D8 followup: other monitoring schemes
}
#
#
# import recoded monitoring from g-sheet
tmp <- googledrive::drive_find(
  pattern = "monitoring_recoded_JA",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  dplyr::arrange(dplyr::desc(name))
tmp_id <- tmp[1,] |>
  googledrive::as_id()
moni_recoded <- googlesheets4::read_sheet(ss = tmp_id)
#
#
#
# --- join data ---------------
#
# join both recoded data frames
res_recoded <- dplyr::full_join(
  meth_recoded,
  moni_recoded
)
#
# re-join with original data
res_comb_upd <- dplyr::full_join(
  res_comb_upd,
  res_recoded
)
#
#
#
# --- save updated response data ---------------
#
save(res_comb_upd, file = paste0(response_data_path, "results_combined_upd.rda"))
#
#
#
# --- process recoded data for methods ---------------
#
# CHECK: can D1 fu be removed here? no filtering needed later
res_meth_recoded <- res_comb_upd |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
  dplyr::filter(on_unionlist, !grepl("IRR", prius_stadium)) |>
  dplyr::filter(grepl("D1$|D2", question_id)) |>
  dplyr::arrange(question_id, species) |>
  # add other methods to question D1 not fu
  dplyr::mutate(
    helper_response_other = dplyr::case_when(
      grepl("D1", question_id) & grepl("followup", question_text) ~ response_text_recoded,
      TRUE ~ NA
    )) |>
  dplyr::group_by(species, question_id) |>
  tidyr::fill(helper_response_other, .direction = "up") |>
  dplyr::ungroup() |>
  # remove string "andere" if other methods are listed
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      !is.na(helper_response_other) ~ gsub(", andere", "", response_text_recoded),
      TRUE ~ response_text_recoded
    )
  ) |>
  # add other methods listed
  dplyr::rowwise() |>
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      !is.na(helper_response_other) ~ paste(
        response_text_final, helper_response_other, sep = ", "
      ),
      TRUE ~ response_text_final
    )
  )   |>
  # move response_text_final
  dplyr::relocate(
    tidyselect::any_of(c("response_text_recoded", "response_text_final")),
    .after = response_text
  )
#
#
# get all possible response options for methods
q_file <- list.files(questions_path, pattern = "long.rda", full.names = TRUE)
q_long <- get(load(q_file))
response_options <- q_long |>
  dplyr::filter(grepl("D1$", question_id), !grepl("followup", question_text)) |>
  dplyr::select(response_option) |>
  # recode case which is otherwise missed
  dplyr::mutate(
    response_option = dplyr::case_when(
      grepl("passieve akoestische monitoring", response_option) ~ "passieve akoestische monitoring",
      TRUE ~ response_option
    )
  ) |>
  dplyr::pull(response_option)
#
# add other methods listed
response_options_other <- res_meth_recoded |>
  dplyr::filter(grepl("D1$", question_id) & grepl("followup", question_text)) |>
  dplyr::pull(response_text_recoded) |>
  na.omit() |>
  paste(x = _, collapse = ", ") |>
  stringr::str_split_1(string = _, pattern = ",") |>
  unique() |>
  trimws()
response_options_upd <- append(response_options, response_options_other)
#
# check for duplicates
duplicated(response_options_upd) |> any()
#
# add methods categories
categories <- c(
  "surveys", #1
  "eDNA", #2
  "trapping and netting", #3
  "electrofishing", #4
  "remote sensing", #5
  "research", #6
  "citizen science", #7
  "trained dogs", #8
  "other" #9
)
response_options_data <- data.frame(
  response_options = response_options_upd,
  response_options_cat = NA
) |> dplyr::mutate(
  response_options_cat = dplyr::case_when(
    grepl("surveys|sporen", response_options) ~ categories[1],
    grepl("environmental DNA", response_options) ~ categories[2],
    grepl("camera|passieve", response_options) ~ categories[5],
    grepl("fuik|vallen|trap|netten", response_options) ~ categories[3],
    grepl("elektrovisserij", response_options) ~ categories[4],
    grepl("experten|genetische", response_options) ~ categories[6],
    grepl("burger", response_options) ~ categories[7],
    grepl("honden", response_options) ~ categories[8],
    grepl("andere", response_options) ~ categories[9]
  )
) |>
  dplyr::arrange(match(response_options_cat, categories))
#
# save data sets
save(res_meth_recoded, file = paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda"))
save(response_options_data, file = paste0(response_data_path, "recoded_processed/", "results_methods_options.rda"))
#
#
#
# --- process recoded data for monitoring schemes ---------------
#
res_moni_recoded <- res_comb_upd |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
  dplyr::filter(on_unionlist, !grepl("IRR", prius_stadium)) |>
  dplyr::filter(grepl("D8", question_id)) |>
  dplyr::arrange(question_id, species) |>
  dplyr::mutate(
    response_text_final = response_text_recoded
  ) |>
  # add other methods to question D8 not fu
  dplyr::mutate(
    helper_response_other = dplyr::case_when(
      grepl("D8", question_id) & grepl("followup", question_text) ~ response_text_final,
      TRUE ~ NA
    )) |>
  dplyr::group_by(species, question_id) |>
  tidyr::fill(helper_response_other, .direction = "up") |>
  dplyr::ungroup() |>
  # add other methods listed
  dplyr::rowwise() |>
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      !is.na(helper_response_other) ~ paste(
        response_text_final, helper_response_other, sep = ", "
      ),
      TRUE ~ response_text_final
    )
  )   |>
  # move response_text_final
  dplyr::relocate(
    tidyselect::any_of(c("response_text_recoded", "response_text_final")),
    .after = response_text
  )
# here: filter fu question
#
#
# get all possible listed response options for monitoring
res_moni_options <- q_long |>
  dplyr::filter(grepl("D8", question_id), !grepl("followup", question_text)) |>
  dplyr::pull(response_option)
#
# get "other" methods listed
res_moni_options_other <- res_moni_recoded |>
  dplyr::pull(helper_response_other) |>
  na.omit() |>
  paste(x = _, collapse = ", ") |>
  stringr::str_split_1(string = _, pattern = ",") |>
  trimws() |>
  unique()
#
# combine response options monitoring
res_moni_options_upd <- append(res_moni_options, res_moni_options_other)
#
# check for duplicates
duplicated(res_moni_options_upd) |> any()
#
# save data sets
save(res_moni_recoded, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda"))
save(res_moni_options_upd, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_options.rda"))
