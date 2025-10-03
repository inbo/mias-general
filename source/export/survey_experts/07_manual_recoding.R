rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
lang <- "EN"
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
# --- shorten certain vernacular names ---------------
#
res_comb_upd <- res_comb |>
  dplyr::mutate(
    vern_name_eng = dplyr::case_when(
      grepl("Colombian ramshorn apple snail", vern_name_eng) ~ "Colombian ramshorn apple snail",
      TRUE ~ vern_name_eng
      ),
    vern_name_nld = dplyr::case_when(
      grepl("moerasaronskelk", vern_name_nld) ~ "moerasaronskelk",
      TRUE ~ vern_name_nld
    )
  )
#
#
#
# --- add variable question_text_short (EN) ---------------
#
res_comb_upd <- res_comb_upd |>
  dplyr::mutate(
    question_text_short_EN = dplyr::case_match(
      question_id,
      "A1" ~ "Configuration introduction sites?",
      "A2" ~ "Introduction sites accessible?",
      "A4" ~ "Special introduction sites?",
      "A5" ~ "Probability of introduction?",
      "A6" ~ "Probability of establishment?",
      "B1" ~ "Distribution known?",
      "B2" ~ "Distribution pattern?",
      "B3" ~ "Which population density?",
      "B4" ~ "Change in distribution sites?",
      "B5" ~ "Distribution sites accessible?",
      "B7" ~ "Special distribution sites?",
      "C1" ~ "Impact biodiversity?",
      "C2" ~ "Impact biodiversity conservation areas?",
      "C3" ~ "Impact other sectors?",
      "D1" ~ "Which surveillance techniques?",
      "D2" ~ "Most relevant surveillance technique?",
      "D3" ~ "Sensitivity surveillance technique?",
      "D4" ~ "Specificity surveillance technique?",
      "D5" ~ "Cost surveillance technique?",
      "D6" ~ "Scope surveillance technique?",
      "D7" ~ "Field protocol available?",
      "D8" ~ "Relevant surveillance schemes?",
      "D9" ~ "Picked up by surveillance schemes?",
      "D10" ~ "Opportunistic observations representative?",
      "E1" ~ "Species managed?",
      "E2" ~ "Information management evaluation?",
    ),
    .after = question_text_EN
  )
#
#
#
# --- manually recode methods (NL) ---------------
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
# --- manually recode monitoring schemes (NL) ---------------
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
  # D8: remove "andere" if D8 fu is not NA
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
#
#
# --- save updated response data ---------------
#
save(res_comb_upd, file = paste0(response_data_path, "results_combined_upd.rda"))
#
#
#
# --- process recoded data for methods & translate ---------------
#
# add methods listed as "other" (NL)
res_meth_recoded <- res_comb_upd |>
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
  # remove string "andere" (always at the end) if other methods are listed # done manually?
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
# (write and) get method dictionary
if (FALSE) {
  # get all possible listed response options for methods (EN & NL)
  q_file <- list.files(questions_path, pattern = "long.rda", full.names = TRUE)
  q_long <- get(load(q_file))
  res_meth_options <- q_long |>
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
  # get "other" methods listed
  res_meth_options_other <- res_meth_recoded |>
    dplyr::pull(helper_response_other) |>
    na.omit() |>
    paste(x = _, collapse = ", ") |>
    stringr::str_split_1(string = _, pattern = ",") |>
    trimws() |>
    unique()
  #
  # manually update g-sheet 'methods_EN_NL' for method_type_EN == 'other'
}
# import method dictionary from g-sheet
tmp <- googledrive::drive_find(
  pattern = "methods_NL_EN",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  dplyr::arrange(dplyr::desc(name))
tmp_id <- tmp[1,] |>
  googledrive::as_id()
meth_dict <- googlesheets4::read_sheet(ss = tmp_id)
# replace environmental DNA by eDNA
meth_dict <- meth_dict |>
  dplyr::mutate(method_EN = method_EN |> gsub("environmental DNA", "eDNA", x = _))
#
# translate response_text_final
meth_dict_upd <- meth_dict$method_EN
names(meth_dict_upd) <- meth_dict$method_NL
res_meth_recoded <- res_meth_recoded |>
  dplyr::rowwise() |>
  dplyr::mutate(
    response_text_final_EN = stringr::str_replace_all(
      response_text_final,
      pattern = meth_dict_upd
    ),
    .after = response_text_final
  ) |>
  dplyr::mutate(
    response_text_final_EN = gsub(" \\(dmv. automatische opnames\\)", "", response_text_final_EN)
  )
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
res_meth_options_data <- data.frame(
  response_options = meth_dict$method_EN |> unique(),
  response_options_cat = NA
) |> dplyr::mutate(
  response_options_cat = dplyr::case_when(
    grepl("surveys|trace", response_options) ~ categories[1],
    grepl("eDNA", response_options) ~ categories[2],
    grepl("camera|passive", response_options) ~ categories[5],
    grepl("trap|nets", response_options) ~ categories[3],
    grepl("electrofishing", response_options) ~ categories[4],
    grepl("expert|genetic|samples", response_options) ~ categories[6],
    grepl("citizen|opportunistic|traffic", response_options) ~ categories[7],
    grepl("dogs", response_options) ~ categories[8],
    grepl("other", response_options) ~ categories[9]
  )
) |>
  dplyr::arrange(match(response_options_cat, categories))
#
# save data sets
save(res_meth_recoded, file = paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda"))
save(res_meth_options_data, file = paste0(response_data_path, "recoded_processed/", "results_methods_options.rda"))
#
#
#
# --- process recoded data for monitoring schemes ---------------
#
res_moni_recoded <- res_comb_upd |>
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
# (write and) get monitoring dictionary
if (FALSE) {
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
  # manually update g-sheet 'monitoring_EN_NL' for monitoring_type_EN == 'other'
}
# import method dictionary from g-sheet
tmp <- googledrive::drive_find(
  pattern = "monitoring_NL_EN",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  dplyr::arrange(dplyr::desc(name))
tmp_id <- tmp[1,] |>
  googledrive::as_id()
moni_dict <- googlesheets4::read_sheet(ss = tmp_id)
#
# translate response_text_final
moni_dict_upd <- moni_dict$monitoring_EN
names(moni_dict_upd) <- moni_dict$monitoring_NL
res_moni_recoded <- res_moni_recoded |>
  dplyr::rowwise() |>
  dplyr::mutate(
    response_text_final_EN = stringr::str_replace_all(
      response_text_final,
      pattern = moni_dict_upd
    ),
    .after = response_text_final
  )  |>
  dplyr::mutate(
    response_text_final_EN = gsub("\\?\\?", "\\?", response_text_final_EN)
  )
#
# get all response options monitoring
res_moni_options_upd <- moni_dict$monitoring_EN |> unique()
#
#
#
#
# save data sets
save(res_moni_recoded, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda"))
save(res_moni_options_upd, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_options.rda"))

