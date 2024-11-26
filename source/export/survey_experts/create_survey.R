rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
# titlebase for forms to be created
form_titlebase <- "bevraging"
#
# id of sheet with questions to be asked in form
# currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\questions.gsheet
sheet_id <- "1MikuShtt9mFdb5f2Nzts7pFR5MZ6HR7h-6Lx0bcEF7k"
#
# id of folder to save form in
# currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\questionnaires_test
form_folder_id <- "1RPd2bbmb6GiTxVz44K26v3jK8VdP8W4K"
#
# path to locally save google apps scripts
appscript_outpath <- "source/export/survey_experts"
#
#
# --- import data with questions and answers from google sheet ---------------
#
# get sheet tab names
# (authentification needed)
sheet_tab_names <- googlesheets4::sheet_names(ss = sheet_id)
#
# read data from all sheet tabs
data_questions_list <- lapply(
  sheet_tab_names,
  googlesheets4::read_sheet,
  ss = sheet_id,
  .name_repair = function(x) {
    # rename variables
    gsub(pattern = "\\s+|-", replacement = "_", x) |>
      tolower()
    }
  )
#
# combine data
data_questions <- data_questions_list |>
  dplyr::bind_rows()
#
# fill missings with last value
data_questions_upd <- data_questions |>
  tidyr::fill(dplyr::starts_with(c("section", "question_id"))) |>
  dplyr::group_by(question_id) |>
  tidyr::fill(dplyr::everything()) |>
  dplyr::ungroup()
#
# reshape response options to wide
data_questions_wide <- data_questions_upd |>
  dplyr::mutate(
    tmp = paste0(dplyr::row_number()),
    .by = question_id
    ) |>
  tidyr::pivot_wider(
    values_from = c(response_option, score_response_option),
    names_from = tmp
  )
#
# define arguments to be used for form overview / form creation
form_args <- list(
  data_qa = data_questions_wide |>
    dplyr::filter(
      question_include_in_form == 1
    ),
  name_qtype = "response_format",
  name_q = "question_text",
  name_qexpl = "question_explanation",
  name_qexplfu = "question_explanation_follow_up",
  basename_aoptions = "response_option",
  name_secno = "section_number",
  name_sectitle = "section_title"
)
#
# --- create questions overview -------------
#
do.call(
  create_overview_gform,
  append(form_args,
         list(
           path_section_template_rmd = "source/export/survey_experts/questions_overview/section_template.Rmd"
         ))
)
rmarkdown::render(
  input = "source/export/survey_experts/questions_overview/master.Rmd",
  output_dir = "source/export/survey_experts/questions_overview/",
  output_file = "questions_overview.pdf"
)
#
# upload PDF
# path currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\media
googledrive::drive_upload(
    media = "source/export/survey_experts/questions_overview/questions_overview.pdf",
    path = googledrive::as_id("1vvnnT_CKx4_Ph1k9rDc1_SXmdhdVMbqv"),
    overwrite = TRUE
  )
#
# retrieve public url
pdf_url <- googledrive::drive_find(
  pattern = "questions_overview",
  type = "pdf",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_share_anyone() |>
  googledrive::drive_link()
#
#
# --- create google apps script which builds forms -------------
#
# get species names
# (there will be one form per species)
names <- get(
  load("data/processed/names_prius.Rda")
) |>
  purrr::pluck("data")  |>
  dplyr::mutate(vernacularName = stringr::str_to_sentence(vernacularName)) |>
  dplyr::select(contains(c("scientific", "vernacular"))) |>
  tidyr::unite(tmp, 1:2, sep = " / ") |>
  dplyr::pull("tmp") |>
  sort()
#
# get public ids to distribution map images
maps_ids <- googledrive::drive_ls(
  pattern = "placeholder",
  type = "png",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_share_anyone() |>
  googledrive::as_id()
#
# combine species info
species_info <- data.frame(
  names = names[c(1,20,30,40)], # for TESTING (DELETE)
  maps = maps_ids
)
#
# write apps script to create forms
#
# TO DO
# not possible yet: italics
# https://stackoverflow.com/questions/18389284/text-formatting-for-strings-in-google-documents-from-google-apps-script?rq=3
# multiple options for drop-down?
#
appsscript_gform <- do.call(
  what = create_appsscript_gform,
  args = append(form_args,
                list(
                  name_qid = "question_id",
                  name_areq = "response_required",
                  form_titlebase = "bevraging_test",
                  gdrive_destfolder_id = form_folder_id,
                  species_qtext = "Over welke soort rapporteert u?",
                  species_qtext_map = "Is de verspreiding van de soort over Vlaanderen voldoende gekend?",
                  species_info = species_info,
                  url_qoverview = pdf_url
                ))
)
#
# save script
writeLines(
  appsscript_gform,
  paste0(appscript_outpath, "/appsscript_gform.gs")
)
#
# update dynamic sections
update_appsscript_dynsections(paste0(appscript_outpath, "/appsscript_gform.gs"))
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
# --- create google apps script which retrieves the form view url -------------
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
# destfolder currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\view_url
appsscript_writeviewurl <- create_appsscript_writeviewurl(
    form_ids = form_ids,
    gdrive_destfolder_id = "1p6eClyAGP_DUeSuKPmzeOyY05c8f5OKu",
    name_outfile = paste0(form_titlebase, "_viewurls")
)
#
# save script
writeLines(
  appsscript_writeviewurl,
  paste0(appscript_outpath, "/appsscript_writeviewurl.gs")
)
#
# --- delete previous sheets --------------------------------
#
viewurl_id <- googledrive::drive_find(
  pattern = paste0(form_titlebase, "_viewurls"),
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  googledrive::as_id()
googledrive::drive_rm(viewurl_id)
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
