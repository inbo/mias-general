rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
# adjust: title for to be created form
form_title <- "questionnaire_test"
#
# adjust: id of sheet with questions
# currently: PRJ_meetnetten-IUS\_overkoepelend\bevraging_soortenexperts\questions.gsheet
sheet_id <- "1MikuShtt9mFdb5f2Nzts7pFR5MZ6HR7h-6Lx0bcEF7k"
#
# adjust: name of sheet tab to use
sheet_tab_name <- form_title
#
# keep: id of folder to save form in
# currently: PRJ_meetnetten-IUS\_overkoepelend\bevraging_soortenexperts
form_folder_id <- "1INTfoS4vLtt4QmrwWLjhV43UoWjo8aix"
#
# keep: path to locally save google apps scripts
appscript_outpath <- "source/export/survey_experts"
#
#
# --- import data with questions and answers from google sheet ---------------
#
data_questions <- googlesheets4::read_sheet(
  ss = sheet_id,
  sheet = sheet_tab_name
)
#
#
# --- create google apps script which builds form from above data -------------
#
appsscript_gform <- create_appsscript_gform(
    data_qa = data_questions,
    name_qtype = "Question type",
    name_q = "Question",
    basename_aoptions = "Response option",
    name_secno = "Section number",
    name_sectitle = "Section title",
    form_title = form_title,
    gdrive_destfolder_id = form_folder_id
)
# save script
writeLines(
  appsscript_gform,
  paste0(appscript_outpath, "/appsscript_gform.gs")
  )
#
#
# --- delete previous form version & responses --------------------------------
#
if (FALSE){
  #
  # get form id
  form_id <- googledrive::drive_find(
    pattern = form_title,
    type = "form",
    shared_drive = "PRJ_meetnetten-IUS"
  ) |>
    dplyr::pull("id") |>
    googledrive::as_id()
  #
  # get form reponses id
  form_responses_id <- googledrive::drive_find(
    pattern = form_title,
    type = "spreadsheet",
    shared_drive = "PRJ_meetnetten-IUS"
  ) |>
    dplyr::pull("id") |>
    googledrive::as_id()
  #
  # remove form
  googledrive::drive_rm(form_id)
  #
  # remove form responses
  googledrive::drive_rm(form_responses_id)
}
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (only once):
# ---------------------
# open https:://drive.google.com/drive/folders/form_folder_id
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
#
# --- create google apps script which updates a dropdown menu -------------
#
# get the id of the above created form
form_id <- googledrive::drive_find(
  pattern = form_title,
  type = "form",
  shared_drive = "PRJ_meetnetten-IUS"
  ) |>
  dplyr::pull("id") |>
  googledrive::as_id()
#
#
# get dropdown menu options
names <- get(
  load("data/processed/names_prius.Rda")
) |>
  purrr::pluck("data")
tmp <- rgbif::name_backbone_checklist(
  name_data = names,
  strict = TRUE
)
full_names <- tmp |>
  dplyr::select(contains(c("canonical", "scientific"))) |>
  tidyr::unite(tmp, 1:2, sep = " | ") |>
  dplyr::pull("tmp") |>
  sort()
#
#
# create apps script
appsscript_dropdownupd <- create_appsscript_dropdownupd(
    form_id = form_id,
    question_text = "Which species are you reporting for?",
    aoptions_upd = full_names
)
#
# save script
writeLines(
  appsscript_dropdownupd,
  paste0(appscript_outpath, "/appsscript_dropdownupd.gs")
)
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (whenever the appscript for dropdown updating has been updated):
# ---------------------------------------------------------------------
# open the local "appsscript_dropdownupd.gs" file here or in a text editor
# add a new script to the above created apps script project
# copy and paste its content into the apps script project
# more specifically into a (second) .gs file associated with the project
# save the project
# run the function
#
#
# --- create google apps script which retrieves the form view url -------------
#
# create apps script
appsscript_writeviewurl <- create_appsscript_writeviewurl(
    form_id = form_id,
    gdrive_destfolder_id = form_folder_id,
    name_outtxtfile = paste0(form_title, "_viewurl.txt")
)
#
# save script
writeLines(
  appsscript_writeviewurl,
  paste0(appscript_outpath, "/appsscript_writeviewurl.gs")
)
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (whenever a form is ready to send out):
# ---------------------------------------------------------------------
# open the local "appsscript_writeviewurl.gs" file here or in a text editor
# add a new script to the above created apps script project
# copy and paste its content into the apps script project
# more specifically into a (second) .gs file associated with the project
# save the project
# run the function
#
#
