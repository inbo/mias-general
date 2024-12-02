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
## --- create google apps script which retrieves the form view url -------------
#
# get form ids
data_form <- googledrive::drive_ls(
  path = form_folder_url |>
    googledrive::as_id()
)
form_ids <- data_form |>
  googledrive::as_id()
#
# create apps script
appsscript_writeviewurl <- create_appsscript_writeviewurl(
  form_ids = form_ids,
  gdrive_destfolder_id = distribution_folder_url |> googledrive::as_id(),
  name_outfile = paste0(form_titlebase, "_viewurls")
)
#
# save script
writeLines(
  appsscript_writeviewurl,
  paste0(appscript_path, "appsscript_writeviewurl.gs")
)
#
#
#
# --- delete previous sheets --------------------------------
#
if (FALSE) {
  viewurl_id <- googledrive::drive_find(
    pattern = paste0(form_titlebase, "_viewurls"),
    shared_drive = "PRJ_MIUS",
    type = "spreadsheet"
  ) |>
    googledrive::as_id()
  googledrive::drive_rm(viewurl_id)
}
#
#
# --- add script to google apps script project and execute script-------------
#
# manually (whenever a form is ready to send out):
# ------------------------------------------------
# open the local "appsscript_writeviewurl.gs" file here or in a text editor
# add a new script to the above created apps script project
# copy and paste its content into the apps script project
# more specifically into a (second) .gs file associated with the project
# save the project
# run the function
#
#
# --- create google sheet for emails --------------------------------
#
# compose sheet content
emailsheet_data <- data_form |>
  dplyr::mutate(
    species = name |>
      gsub(pattern = paste0(form_titlebase, " "), replacement = "", x = _),
  ) |>
  dplyr::select("species") |>
  dplyr::mutate(
    expert_firstname = "expert first name",
    expert_lastname = "expert last name",
    expert_emailaddress = "expert email address",
    .after = species
  )
#
# create sheet (my drive)
googlesheets4::gs4_create(name = paste0(form_titlebase, "_emailadresses"), sheets = emailsheet_data)
#
# move sheet to target folder
emailsheet_id <- googledrive::drive_find(
  pattern = paste0(form_titlebase, "_emailadresses"),
  type = "spreadsheet"
) |> googledrive::as_id()
googledrive::drive_mv(
  file = emailsheet_id,
  path = distribution_folder_url |> paste0(x = _, "/")
  )
#
# delete old sheet versions: manually to not loose email addresses
#
#
#
# --- collect email adresses and prepare email text --------------------------------
#
# manually (whenever a form is ready to send out):
# ------------------------------------------------
# adapt the above created gsheet and gdoc "emailtext"
# currently in folder:
# PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\distribution
#
#
#
# --- compose individual emails --------------------------------
#
# import view urls
data_viewurl <- googlesheets4::read_sheet(
  ss = googledrive::drive_ls(
    path = distribution_folder_url |> googledrive::as_id(),
    pattern = "viewurls"
  ) |>
    googledrive::as_id()
)
#
# import email adresses
data_addresses <- googlesheets4::read_sheet(
  ss = googledrive::drive_ls(
    path = distribution_folder_url |> googledrive::as_id(),
    pattern = "emailadresses"
  ) |>
    googledrive::as_id()
)
#
# combine data
tmp <- data_viewurl |>
  dplyr::mutate(
    species = formtitle |>
      gsub(pattern = paste0(form_titlebase, " "), replacement = "", x = _),
  )
data_email <- dplyr::full_join(tmp, data_addresses)
#
# import email text
email_text <- googledrive::drive_read_string(
  file = googledrive::drive_ls(
    path = distribution_folder_url |> googledrive::as_id(),
    pattern = "emailtext"
  ) |>
    googledrive::as_id(),
  type = "text/plain"
)
#
# extract email body & subject line
email_subjectline <- email_text |>
  gsub(pattern = "(\\[begin subjectline\\] )|( \\[end subjectline\\]).*",
       replacement = "", x = _)
email_body <- email_text |>
  gsub(pattern = ".*(\\[begin email\\] )|(\\[end email\\]).*",
       replacement = "", x = _) |>
  # remove characters
  gsub(pattern = "\\[a\\]", replacement = "", x = _) |>
  # keep line breaks as special characters
  gsub(pattern = "(\\\r\\\n)", replacement = "\\\\n", x = _)
#
# compose individual emails and add to email data
data_email$email_body <- NA
data_email$email_subjectline <- email_subjectline
for (i in seq_along(data_email$viewurl)) {
  if (grepl("@inbo", data_email$expert_emailaddress[i])) {
    name_receiver <- data_email$expert_firstname[i]
    pronoun <- "jouw"
    name_sender <- "Janne"
  } else {
    name_receiver <- paste(data_email$expert_firstname[i], data_email$expert_lastname[i])
    pronoun <- "uw"
    name_sender <- "Janne Adolf"
  }
  email_body_i <- email_body |>
    gsub(pattern = "\\[naam ontvanger\\]", replacement = name_receiver, x = _) |>
    gsub(pattern = "\\[jouw/uw\\]", replacement = pronoun, x = _) |>
    gsub(pattern = "\\[naam afzender\\]", replacement = name_sender, x = _) |>
    gsub(pattern = "\\[link bevraging\\]", replacement = data_email$viewurl[i], x = _)
  data_email$email_body[i] <- email_body_i
}

#
#
# --- create google apps script which sends out emails --------------------------------
#
# write apps script to send emails
appsscript_email <- create_appsscript_sendemail(data_email = data_email)
#
# save script
writeLines(
  appsscript_email,
  paste0(appscript_path, "appsscript_email.gs")
)
#
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
# open the local "appsscript_email.gs" file here or in a text editor
# copy and paste its content into the above created apps script project
# more specifically into a .gs file associated with the project
# save the project
# run the function
