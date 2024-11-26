rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
# titlebase of forms created
form_titlebase <- "bevraging_test"
#
# id of folder to save responses in
# currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\responses
response_folder_id <- "1Esk5X5J9YKgtZdS4iNQ8g1jpemG6uGcR"
#
# path to locally save google apps scripts
appscript_outpath <- "source/export/survey_experts/appsscripts/"
#
# --- create google apps script which collects data -------------
#
# get form ids
form_ids <- googledrive::drive_find(
  pattern = form_titlebase,
  type = "form",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::as_id()
#
# create apps script
appsscript_linkformstosheet <- create_appsscript_linkformstosheet(
  form_ids = form_ids,
  gdrive_destfolder_id = response_folder_id,
  name_outfile = paste0(form_titlebase, "_responses")
)
#
# save script
writeLines(
  appsscript_linkformstosheet,
  paste0(appscript_outpath, "appsscript_linkformstosheet.gs")
)
#
# --- delete previous sheets --------------------------------
#
if (FALSE){
  responses_id <- googledrive::drive_find(
    pattern = paste0(form_titlebase, "_responses"),
    shared_drive = "PRJ_MIUS",
    type = "spreadsheet"
  ) |>
    googledrive::as_id()
  googledrive::drive_rm(responses_id)
}
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (whenever a form is ready to send out):
# ---------------------------------------------------------------------
# open the local "appsscript_linkformstosheet.gs" file here or in a text editor
# add a new script to the above created apps script project
# copy and paste its content into the apps script project
# more specifically into a .gs file associated with the project
# save the project
# run the function
#
#
#
# --- import data into R --------------------------------
#
# get sheet id
responses_id <- googledrive::drive_find(
  pattern = paste0(form_titlebase, "_responses"),
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  googledrive::as_id()
#
# get sheet tab names
responses_tab_names <- googlesheets4::sheet_names(ss = responses_id) |>
  grep(pattern = "Sheet1", value = TRUE, invert = TRUE, x = _)
#
# read data from all sheet tabs
data_responses_list <- lapply(
  responses_tab_names,
  googlesheets4::read_sheet,
  ss = responses_id,
  .name_repair = function(x) {
    # rename variables
    make.unique(names = x) |>
    gsub(pattern = "\\s+|-", replacement = "_", x) |>
      tolower()
  }
)
#
# combine & reshape data
data_responses <- data_responses_list |>
  dplyr::bind_rows()
colnames_rename <- c(
  timestamp = colnames(data_responses) |> grep(pattern = "timestamp", value = TRUE),
  email = colnames(data_responses) |> grep(pattern = "e_mail", value = TRUE),
  species = colnames(data_responses) |> grep(pattern = "welke_soort", value = TRUE),
  stadium = colnames(data_responses) |> grep(pattern = "invasiestadium", value = TRUE)
  )
data_resp_long <- data_responses |>
  dplyr::rename(colnames_rename) |>
  tidyr::pivot_longer(
    cols = -c(colnames_rename |> names()),
    names_to = "question",
    values_to = "response"
  )
#
# cleaning-up the data
colnames_dupl <- colnames(data_responses) |>
  grep(pattern = "\\.1", value = TRUE)
data_resp_red <- data_resp_long |>
  # rename follow-up questions
  dplyr::mutate(
    question_tmp = dplyr::case_when(
      grepl("column", question) ~ NA_character_,
      TRUE ~ question
    )
  ) |>
  tidyr::fill(question_tmp) |>
  dplyr::mutate(
    question_upd = dplyr::case_when(
      grepl("column", question) ~ paste0('followup_', question_tmp),
      TRUE ~ question
    )
  ) |>
  # remove duplicated sections
  dplyr::group_by(species) |>
  dplyr::mutate(
    keep_row = dplyr::case_when(
      (question_tmp %in% colnames_dupl & is.na(response)) ~ FALSE,
      TRUE ~ TRUE
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(keep_row == TRUE)
# remove sections not filled-in

#
# --- merge response and question data --------------------------------
#



