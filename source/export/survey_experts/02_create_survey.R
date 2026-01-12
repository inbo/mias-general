rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
lang <- "NL"
source('source/export/survey_experts/00_definitions.R')
#
#
#
# --- load data with questions and answers ---------------
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
  pattern = paste("overview_questions", lang, sep = "_"),
  type = "pdf",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_link()
#
#
# --- get distribution map information -------------
#
# list distribution map files on gdrive (filenames and -ids)
maps_files <- googledrive::drive_ls(
  path = map_folder_url,
  shared_drive = "PRJ_MIUS"
)
#
# load dataframe with file- and speciesnames
maps_files_names <- get(load("media/gbif_occcubes/plot_filepaths.rda")) |>
  dplyr::mutate(
    name = basename(path_to_map)
  )
#
# rename "vespa_velutina" for questionnaire
vespa_velutina_name <- "Vespa velutina nigrithorax Buysson, 1905"
maps_files_names_upd <- maps_files_names |>
  dplyr::mutate(
    species = dplyr::case_when(
      grepl("Vespa velutina", species) ~ vespa_velutina_name,
      TRUE ~ species
    )
  )
#
# join
maps_info <- dplyr::left_join(
  maps_files,
  maps_files_names_upd
)
#
# make map images public (ids don't change)
tmp <- maps_info |>
  dplyr::pull(id) |>
  googledrive::drive_share_anyone() |>
  googledrive::as_id()
tmp %in% maps_info$id
#
# add gdrive urls to plot_filepaths and save
plot_filepaths_gdriveurls <- maps_info |>
  dplyr::mutate(
    gdrive_url = paste0("https://drive.google.com/file/d/", id, "/view")
  ) |>
  dplyr::select(
    tidyselect::all_of(c("species", "path_to_map", "gdrive_url"))
  )
save(plot_filepaths_gdriveurls,
     file = "media/gbif_occcubes/plot_filepaths_gdriveurls.rda")
#
#
#
# --- create google apps script which builds forms -------------
#
# import intro text
introtext <- googledrive::drive_read_string(
  file = googledrive::drive_ls(
    path = media_folder_url |> googledrive::as_id(),
    pattern = paste("introtext", lang, sep = "_")
  ) |>
    googledrive::as_id(),
  type = "text/plain"
)
introtext_upd <- introtext |>
  gsub(pattern = "\\[link overview questions\\]", replacement = pdf_url, x = _)
#
# format names
maps_info <- maps_info |>
  dplyr::mutate(
    species_formatted = gsub(pattern = "'", replacement = "\\\\'", species)
  )
#
# select species if lang EN
if (lang == "EN") {
  maps_info <- maps_info |>
    dplyr::filter(grepl("Zostera", species))
}
#
# create app script (in x parts)
gform_baseargs <- list(
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
  species_qtext =  "Which species are you reporting on?", #"Over welke soort rapporteert u?",
  species_qtext_map = "Is the distribution of the species across Flanders sufficiently known?", #"Is de verspreiding van de soort over Vlaanderen voldoende gekend?",
  introtext = introtext_upd
)
nparts <- ifelse(lang == "EN", 1, 6)
nrows_parts <- nrow(maps_info)/nparts
for (i in 1:nparts) {
  nrows_i <- seq(((i-1) * nrows_parts + 1), (i * nrows_parts))
  gform_args_i <- append(
    gform_baseargs,
    list(
      species_names = maps_info$species_formatted[nrows_i],
      species_maps_ids = maps_info$id[nrows_i]
    )
  )
  appsscript_gform_i <- do.call(create_appsscript_gform, gform_args_i)
  # save script
  writeLines(
    appsscript_gform_i,
    paste0(appscript_path, "appsscript_gform_", lang, "_", i,".gs")
  )
  # update dynamic sections (hardcoded)
  update_appsscript_dynsections(paste0(appscript_path, "appsscript_gform_", lang, "_", i,".gs"), lang)
}
#
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
# create a google apps script project (new file of type "Google Apps Script")
# rename appropriately
#
# manually (whenever the appscript for form creation has been updated):
# ---------------------------------------------------------------------
# open the local "appsscript_gform.gs" file here or in a text editor
# copy and paste its content into the above created apps script project
# more specifically into a .gs file associated with the project
# save the project
# run the function
# if lang EN: fix issues manually "species'" -> "species"
#
# manually (whenever forms have been created):
# ---------------------------------------------------------------------
# make sure forms can be viewed by external users
# open form manually & open tab 'Settings'
# in section 'Responses' deactivate 'Restrict to users in INBO and its trusted organisations'
