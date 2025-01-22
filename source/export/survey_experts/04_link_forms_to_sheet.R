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
# --- create google apps script which collects data -------------
#
# get form ids
data_form <- googledrive::drive_ls(
  path = form_folder_url |>
    googledrive::as_id()
) |>
  # species in alphabetical, forms in according order
  dplyr::arrange(name)
form_ids <- data_form |>
  googledrive::as_id()
#
# create apps script
name_sheet_raw <- paste0(form_titlebase, "_responses")
appsscript_linkformstosheet <- create_appsscript_linkformstosheet(
  form_ids = form_ids,
  gdrive_destfolder_id = response_folder_url |>
    googledrive::as_id(),
  name_outfile = name_sheet_raw
)
#
# save script
writeLines(
  appsscript_linkformstosheet,
  paste0(appscript_path, "appsscript_linkformstosheet.gs")
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
# NOTE: this only needs to be done once at some point
# also future responses will appear in the google sheet
