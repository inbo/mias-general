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
# split up "other" if other methods are named in follow-up (manually)
#
#
# export to g-sheet for manual recoding
# (only once for given response data)
if (FALSE){
  res_to_recode <- res_comb_upd |>
    dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
    dplyr::filter(grepl("D1$|D2", question_id)) |>
    dplyr::mutate(response_text_recoded = NA_character_, .after = response_text) |>
    dplyr::arrange(species, question_id) |>
    dplyr::select(tidyselect::contains(c("species", "question", "response_text")))
  #
  # upload updated sheet
  googlesheets4::gs4_create(
    name =  paste0(Sys.Date(), "_methods_recoded"),
    sheets = res_to_recode
  )
  # move updated sheet to target folder
  tmp_id <- googledrive::drive_find(
    pattern = paste0(Sys.Date(), "_methods_recoded"),
    type = "spreadsheet"
  ) |> googledrive::as_id()
  googledrive::drive_mv(
    file = tmp_id,
    path = responses_recoded_folder_url |> paste0(x = _, "/")
  )
  #
  # in g-sheet recode response_text manually where needed (D1 followup & D2)
  # D1 followup: other methods
  # D2: most relevant method (should be 1 out of the ones given in D1 / D1fu)
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
# split up "other" if other schemes are named in follow-up (manually)
#
#
# export to g-sheet for manual recoding
# (only once for given response data)
if (FALSE){
  res_to_recode <- res_comb_upd |>
    dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
    dplyr::filter(grepl("D8", question_id)) |>
    dplyr::mutate(response_text_recoded = NA_character_, .after = response_text) |>
    dplyr::arrange(species, question_id) |>
    dplyr::select(tidyselect::contains(c("species", "question_text", "question_id", "response_text")))
  #
  # upload updated sheet
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
  #
  # in g-sheet recode response_text manually where needed (D1 followup)
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
res_meth_recoded <- res_comb_upd |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped) |>
  dplyr::filter(on_unionlist, !grepl("IRR", prius_stadium)) |>
  dplyr::filter(grepl("D1$|D2", question_id)) |>
  dplyr::arrange(question_id, species) |>
  # add other methods to question D1 not fu
  dplyr::group_by(species, question_id) |>
  tidyr::fill(response_text_recoded, .direction = "up") |>
  dplyr::ungroup() |>
  # remove string "andere" if other methods are listed
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      !is.na(response_text_recoded) ~ gsub(", andere", "", response_text),
      TRUE ~ response_text
    )
  ) |>
  # add other methods listed
  dplyr::rowwise() |>
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      grepl("D1$", question_id) & !grepl("followup", question_text) & !is.na(response_text_recoded) ~
        paste(response_text_final, response_text_recoded, sep = ", "),
      grepl("D1$", question_id) & !grepl("followup", question_text) & is.na(response_text_recoded) ~
        response_text_final,
      TRUE ~ NA_character_
    )
  ) |>
  # add best method
  dplyr::mutate(
    response_text_final = dplyr::case_when(
      grepl("D2$", question_id) ~ response_text_recoded,
      TRUE ~ response_text_final
    )
  ) |>
  # move response_text_final
  dplyr::relocate(response_text_final, .after = response_text_recoded)
#
# save data set
save(res_meth_recoded, file = paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda"))
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
  # add other monitoring schemes to question D1 not fu
  #dplyr::group_by(species, question_id) |>
  #tidyr::fill(response_text_recoded, .direction = "up") |>
  #dplyr::ungroup() |>
  # remove string "andere" if other methods are listed
  #dplyr::mutate(
  #  response_text_final = dplyr::case_when(
  #    !is.na(response_text_recoded) ~ gsub(", andere", "", response_text),
  #    grepl("-andere", response_text_recoded) ~ gsub("andere", "", response_text),
  #    TRUE ~ response_text
  #  )
  # add other methods listed
  #dplyr::rowwise() |>
  #dplyr::mutate(
  #  response_text_final = dplyr::case_when(
  #    grepl("D8", question_id) & !grepl("followup", question_text) & !is.na(response_text_recoded) ~
  #      paste(response_text_final, response_text_recoded, sep = ", "),
  #    grepl("D8", question_id) & !grepl("followup", question_text) & is.na(response_text_recoded) ~
  #      response_text_final,
  #    TRUE ~ NA_character_
  #  )
  #) |>
  dplyr::mutate(
    response_text_final = response_text_recoded
    ) |>
  # move response_text_final
  dplyr::relocate(response_text_final, .after = response_text_recoded)
#
# save data set
save(res_moni_recoded, file = paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda"))
#
