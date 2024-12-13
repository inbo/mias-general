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
#
#
# --- import data with questions and answers ---------------
#
questions_file <- list.files(
  questions_path,
  pattern = "wide.rda",
  full.names = TRUE
  )
questions_wide <- get(load(questions_file))
#
#
# retrieve public url to overview pdf
pdf_url <- googledrive::drive_find(
  pattern = "overview_questions",
  type = "pdf",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_link()
#
#
# --- get species information -------------
#
# get species names
# (there will be one form per species)
species <- do.call(
  process_expertsheet,
  expert_sheetid_args
)
#
# test: all species unique
assertthat::are_equal(
  species$key_acc_gbif |> unique() |> length(),
  nrow(species)
)
#
# write gsheet for review (esp. scientific names)
if (FALSE) {
  # create gsheet
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
}
#
# summarize 'invasieve duizenknopen' in species data frame
species_sum <- species |>
  dplyr::filter(grepl("duizendknopen", species)) |>
  dplyr::pull(sci_name_gbif_acc) |>
  paste(x = _, collapse = ", ")
species_upd <- species |>
  dplyr::mutate(
    sci_name_gbif_sum = dplyr::case_when(
      grepl("duizendknopen", species) ~ species_sum,
      TRUE ~ sci_name_gbif_acc
    )
  ) |>
  dplyr::distinct(sci_name_gbif_sum, .keep_all = TRUE)
#
# get public ids to distribution map images
maps_ids <- googledrive::drive_ls(
  pattern = "placeholder_1", ### HERE: restriction for testing; update!
  type = "png",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_share_anyone() |>
  googledrive::as_id()
#
#
# combine species info
species_info <- data.frame(
  names = species$sci_name_gbif_sum[1:10], ### HERE: restriction for testing; update!
  maps = maps_ids[1:10] ### HERE: restriction for testing; update!
)
#
#
#
# --- create google apps script which builds forms -------------
#
# import intro text
introtext <- googledrive::drive_read_string(
  file = googledrive::drive_ls(
    path = media_folder_url |> googledrive::as_id(),
    pattern = "introtext"
  ) |>
    googledrive::as_id(),
  type = "text/plain"
)
introtext_upd <- introtext |>
  gsub(pattern = "\\[link overview questions\\]", replacement = pdf_url, x = _)
#
# create app script
# (not possible yet: italics
# https://stackoverflow.com/questions/18389284/text-formatting-for-strings-in-google-documents-from-google-apps-script?rq=3
# multiple options for drop-down?)
#
appsscript_gform <- create_appsscript_gform(
  data_qa = questions_wide |>
    dplyr::filter(
      question_include_in_form == 1
    ),
  name_qtype = "response_format",
  name_q = "question_text",
  name_qexpl = "question_explanation",
  name_qexplfu = "question_explanation_follow_up",
  basename_aoptions = "response_option",
  name_secno = "section_number",
  name_sectitle = "section_title",
  name_qid = "question_id",
  name_areq = "response_required",
  gdrive_destfolder_id = form_folder_url |> googledrive::as_id(),
  species_qtext = "Over welke soort rapporteert u?",
  species_qtext_map = "Is de verspreiding van de soort over Vlaanderen voldoende gekend?",
  species_info = species_info,
  introtext = introtext_upd
)
#
# save script
writeLines(
  appsscript_gform,
  paste0(appscript_path, "appsscript_gform.gs")
)
#
# update dynamic sections
update_appsscript_dynsections(paste0(appscript_path, "appsscript_gform.gs"))
#
#
#
# --- delete previous form version & responses --------------------------------
#
if (FALSE){
  #
  # get form id
  form_ids <- googledrive::drive_find(
    pattern = form_titlebase,
    type = "form",
    shared_drive = "PRJ_MIUS"
  ) |>
    googledrive::as_id()
  #
  # get form reponses id
  #form_responses_id <- googledrive::drive_find(
  #  pattern = form_title,
  #  type = "spreadsheet",
  #  shared_drive = "PRJ_MIUS"
  #) |>
  #  dplyr::pull("id") |>
  #  googledrive::as_id()
  #
  # remove form
  googledrive::drive_rm(form_ids)
  #
  # remove form responses
  #googledrive::drive_rm(form_responses_id)
}
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (only once):
# ---------------------
# create a new file of type "Google Apps Script"
# rename appropriately
#
# manually (whenever the appscript for form creation has been updated):
# ---------------------------------------------------------------------
# open the local "appsscript_gform.gs" file here or in a text editor
# copy and paste its content into the above created apps script project
# more specifically into a .gs file associated with the project
# save the project
# run the function
#
