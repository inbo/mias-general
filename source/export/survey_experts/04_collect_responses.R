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
name_sheet_raw <- paste0(form_titlebase, "_responses_", Sys.Date())
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
#
#
#
# --- import data into R --------------------------------
#
# get sheet id
responses_id <- googledrive::drive_find(
  pattern =  name_sheet_raw,
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
  function(i){
    googlesheets4::read_sheet(
      ss = responses_id,
      sheet = i,
      .name_repair = function(x) {
        # rename variables
        make.unique(names = x) |>
          gsub(pattern = "\\s+|-", replacement = "_", x) |>
          tolower()
      }
    )
  }
)
#
# --- preprocess data --------------------------------
#
#
# remove empty forms
data_responses_list_filled <- vctrs::list_drop_empty(data_responses_list)
#
# combine data across tabs
data_responses <- data_responses_list_filled |>
  dplyr::bind_rows()
#
# reshape to long and rename
colnames_rename <- c(
  timestamp = colnames(data_responses) |> grep(pattern = "timestamp", value = TRUE),
  email = colnames(data_responses) |> grep(pattern = "e_mail", value = TRUE),
  species = colnames(data_responses) |> grep(pattern = "welke_soort", value = TRUE),
  stadium = colnames(data_responses) |> grep(pattern = "invasiestadium", value = TRUE)
)
data_resp_long_tmp <- data_responses |>
  dplyr::rename(colnames_rename) |>
  tidyr::pivot_longer(
    cols = -tidyr::all_of(colnames_rename |> names()),
    names_to = "question",
    values_to = "response"
  )
#
# cleaning the data
colnames_dupl <- colnames(data_responses) |>
  grep(pattern = "\\.1", value = TRUE)
data_resp_long <- data_resp_long_tmp |>
  # rename follow-up questions
  dplyr::mutate(
    question_tmp = dplyr::case_when(
      grepl("column", question) ~ NA_character_,
      TRUE ~ question
    ),
  ) |>
  tidyr::fill(question_tmp) |>
  dplyr::mutate(
    question_upd = dplyr::case_when(
      grepl("column", question) ~ paste0('followup_', question_tmp),
      TRUE ~ question
    ),
    .after = question
  ) |>
  # remove duplicated & not-filled-in sections
  dplyr::group_by(species) |>
  dplyr::mutate(
    keep_row = dplyr::case_when(
      (question_tmp %in% colnames_dupl & is.na(response)) ~ FALSE,
      TRUE ~ TRUE
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(keep_row == TRUE) |>
  # keep only question_upd
  dplyr::select(-dplyr::one_of(c("question", "question_tmp", "keep_row"))) |>
  # sort species alphabetically
  dplyr::arrange(species)
#
#
# reshape to wide
data_resp_wide <- data_resp_long |>
  tidyr::pivot_wider(
    names_from = question_upd,
    values_from = response

  )
#
# save data locally
save(data_resp_long,
     file = paste0(response_data_path, Sys.Date(), "_responsedata_long.rda") |>
       gsub(pattern = "-", replacement = "_")
     )
save(data_resp_wide,
     file = paste0(response_data_path, Sys.Date(), "_responsedata_wide.rda") |>
       gsub(pattern = "-", replacement = "_")
)
#
# some checks
#
# species unique?
assertthat::are_equal(data_resp_wide$species |> unique() |> length(), nrow(data_resp_wide))
#
#
# --- upload response data to g-drive --------------------------------
#
#
# sheet name
datasheet_name_wide <- paste0(form_titlebase, "_responses_wide_", Sys.Date())
datasheet_name_long <- paste0(form_titlebase, "_responses_long_", Sys.Date())
#
# upload sheet wide
googlesheets4::gs4_create(
  name =  datasheet_name_wide,
  sheets = data_resp_wide
)
# move sheet wide to target folder
tmp_id <- googledrive::drive_find(
  pattern = datasheet_name_wide,
  type = "spreadsheet"
) |> googledrive::as_id()
googledrive::drive_mv(
  file = tmp_id,
  path = response_folder_url |> paste0(x = _, "/")
)
#
if (FALSE) {
  # upload sheet long
  googlesheets4::gs4_create(
    name =  datasheet_name_long,
    sheets = data_resp_long
  )
  # move sheet long to target folder
  tmp_id <- googledrive::drive_find(
    pattern = datasheet_name_long,
    type = "spreadsheet"
  ) |> googledrive::as_id()
  googledrive::drive_mv(
    file = tmp_id,
    path = response_folder_url |> paste0(x = _, "/")
  )
}
#
#
#
# --- update g-sheet that documents emails sent --------------------------------
#
# get g-sheet that documents emails sent
tmp <- googledrive::drive_find(
  pattern = "CONTROL emails sent 2024-12-20",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
) |>
  dplyr::filter(!grepl("responses", name))
emailsheet_name <- tmp$name
emailsheet_id <- tmp |> googledrive::as_id()
mails_sent <- googlesheets4::read_sheet(ss = emailsheet_id)
#
# add information
mails_sent_upd <- mails_sent |>
  # reshape to long format
  tidyr::pivot_longer(
    cols = tidyselect::contains("species"),
    values_to = "species"
  ) |>
  tidyr::drop_na(species) |>
  # join with current responses
  dplyr::left_join(
    x = _,
    y = data_resp_wide |> dplyr::select(c("timestamp", "email", "species")),
    by = c("species")
  ) |>
  # record responses
  dplyr::mutate(
    species_filled_in = ifelse(is.na(timestamp), FALSE, TRUE),
    expert_completed_all = dplyr::case_when(
      sum(species_filled_in) == dplyr::n() ~ TRUE,
      TRUE ~ FALSE
    ),
    expert_completed_some = dplyr::case_when(
      sum(species_filled_in) > 0 & sum(species_filled_in) < dplyr::n() ~ TRUE,
      TRUE ~ FALSE
    ),
    expert_completed_none = dplyr::case_when(
      sum(species_filled_in) == 0 ~TRUE,
      TRUE ~ FALSE
    ),
    .by = expert_emailaddress
  ) |>
  # remove timestamp
  dplyr::select(-timestamp)
#
# reshape to wide
mails_sent_upd_wide <- mails_sent_upd |>
  dplyr::mutate(
    species_no = paste0("species ", dplyr::row_number()),
    .by = expert_emailaddress
  ) |>
  tidyr::pivot_wider(
    data = _,
    id_cols = tidyselect::contains("expert"),
    names_from = species_no,
    values_from = species
  )
#
# upload updated sheet
googlesheets4::gs4_create(
  name =  paste(emailsheet_name, "responses", Sys.Date()),
  sheets = mails_sent_upd_wide
)
# move updated sheet to target folder
tmp_id <- googledrive::drive_find(
  pattern = paste(Sys.Date()),
  type = "spreadsheet"
) |> googledrive::as_id()
googledrive::drive_mv(
  file = tmp_id,
  path = distribution_folder_url |> paste0(x = _, "/")
)
#
#
#
# --- compare recorded responses --------------------------------
#
tmp <- googledrive::drive_find(
  pattern = "CONTROL emails sent 2024-12-20",
  shared_drive = "PRJ_MIUS",
  type = "spreadsheet"
)
#
tmp_id1 <- tmp |>
  dplyr::filter(grepl("2025-01-17", name)) |>
  googledrive::as_id()
tmp_id2 <- tmp |>
  dplyr::filter(grepl("2025-01-20", name)) |>
  googledrive::as_id()
#
ctrl1 <- googlesheets4::read_sheet(ss = tmp_id1)
ctrl2 <- googlesheets4::read_sheet(ss = tmp_id2)
generics::setdiff(ctrl2, ctrl1) |> View()
