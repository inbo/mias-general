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
# --- collect email adresses and prepare email text --------------------------------
#
# manually (whenever a form is ready to send out):
# ------------------------------------------------
# adapt the relevant gsheet and gdoc "emailtext"
# leave the text highlighted in yellow as is
# currently in folder:
# PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\distribution
#
#
# --- compose individual emails --------------------------------
#
# retrieve public url to overview pdf
pdf_url <- googledrive::drive_find(
  pattern = "overview_questions",
  type = "pdf",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_link()
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
emails <- do.call(
  process_speciessheet,
  species_sheet_args
)
#
# combine data
tmp <- data_viewurl |>
  dplyr::mutate(
    sci_name_gbif_acc = formtitle |>
      gsub(pattern = paste0(form_titlebase, " "), replacement = "", x = _),
  )
data_email <- dplyr::left_join(tmp, emails)
#
# import email text
email_text <- googledrive::drive_read_string(
  file = googledrive::drive_ls(
    path = distribution_folder_url |> googledrive::as_id(),
    pattern = "emailtext"
  ) |>
    googledrive::as_id(),
  type = "text/plain"
) |>
  # remove comments in text
  gsub(pattern = "\\[[a-z]\\]", replacement = "", x = _)
#
# extract subject line
email_subjectline <- email_text |>
  gsub(pattern = "(\\[begin subjectline\\])|(\\[end subjectline\\]).*",
       replacement = "", x = _) |>
  gsub(pattern = "(\\\r\\\n)", replacement = "", x = _)
#
# extract email body
email_body <- email_text |>
  gsub(pattern = ".*(\\[begin email\\])|(\\[end email\\]).*",
       replacement = "", x = _) |>
  # keep line breaks as special characters
  gsub(pattern = "(\\\r\\\n)", replacement = "\\\\n", x = _) |>
  gsub(pattern = "(\\\\n\\\\n\\\\n)", replacement = "\\\\n\\\\n", x = _) |>
  gsub(pattern = "\\[link overview questions\\]", replacement = pdf_url, x = _)
#
# extract formlink text
email_formlink_dummy <- email_body |>
  gsub(pattern = ".*(\\[begin link to form text\\])|(\\[end link to form text\\]).*",
       replacement = "\\1\\2", x = _)
email_formlink <- email_formlink_dummy |>
  gsub(pattern = ".*(\\[begin link to form text\\])|(\\[end link to form text\\]).*",
       replacement = "", x = _)
email_formlink_dummy <- email_formlink_dummy |>
  gsub(pattern = "\\[", replacement = "\\\\[", x = _) |>
  gsub(pattern = "\\\\n", replacement = "\\\\\\\\n", x = _)

#
# compose individual emails and add to email data
adresses_unique <- data_email$expert_email |> unique() ## here
data_distribution <- data.frame(
  expert_emailaddress = character(),
  email_subjectline = character(),
  email_body = character()
)
for (i in seq_along(adresses_unique)) {
  address_i <- adresses_unique[i]
  data_email_i <- data_email |>
    dplyr::filter(expert_email == address_i) |>
    dplyr::arrange(sci_name_gbif_acc)
  #
  if (grepl("@inbo", address_i)) {
    name_institute <- "INBO"
    name_receiver <- data_email_i$expert_firstname[1]
    pronoun_1 <- "je"
    pronoun_2 <- "jouw"
    name_sender <- "Janne & Diederik"
  } else {
    name_institute <- "Instituut voor Natuur- en Bosonderzoek (INBO)"
    name_receiver <- paste(data_email_i$expert_firstname[1], data_email_i$expert_lastname[1])
    pronoun_1 <- "u"
    pronoun_2 <- "uw"
    name_sender <- "Janne Adolf & Diederik Strubbe"
  }
  #
  email_formlink_mult <- rep(email_formlink, nrow(data_email_i))
  for (j in seq_along(email_formlink_mult)){
    email_formlink_mult[j] <- email_formlink_mult[j] |>
      gsub(pattern = "\\[link form\\]", replacement = data_email_i$viewurl[j], x = _) |>
      gsub(pattern = "\\[species name\\]", replacement = data_email_i$sci_name_gbif_acc[j], x = _)
  }
  email_formlink_mult <- paste(email_formlink_mult, collapse = "") |>
    gsub(pattern = "\\\\n", replacement = "\\\\\\\\n", x = _)
  #
  email_body_i <- email_body |>
    gsub(pattern = "\\[name receiver\\]", replacement = name_receiver, x = _) |>
    gsub(pattern = "\\[name institute\\]", replacement = name_institute, x = _) |>
    gsub(pattern = "\\[pronoun1\\]", replacement = pronoun_1, x = _) |>
    gsub(pattern = "\\[pronoun2\\]", replacement = pronoun_2, x = _) |>
    gsub(pattern = "\\[Pronoun1\\]", replacement = stringr::str_to_title(pronoun_1), x = _) |>
    gsub(pattern = "\\[Pronoun2\\]", replacement = stringr::str_to_title(pronoun_2), x = _) |>
    gsub(pattern = "\\[name sender\\]", replacement = name_sender, x = _) |>
    gsub(pattern = email_formlink_dummy, replacement = email_formlink_mult, x = _) |>
    gsub(pattern = "(\\\\n\\\\n\\\\n)", replacement = "\\\\n\\\\n", x = _)
  data_distribution <- dplyr::bind_rows(
    data_distribution,
    data.frame(
      expert_emailaddress = address_i,
      email_subjectline = email_subjectline,
      email_body = email_body_i
    )
  )
}
#
#
# --- create google apps script which sends out emails --------------------------------
#
# write apps script to send emails (test run A)
appsscript_email_test_a <- create_appsscript_sendemail(
  data_email = data_distribution,
  test = TRUE,
  test_address = "janne.adolf@inbo.be"
  )
#
# save script
writeLines(
  appsscript_email_test_a,
  paste0(appscript_path, "appsscript_email_test_a.gs")
)
#
# write apps script to send emails (test run B)
appsscript_email_test_b <- create_appsscript_sendemail(
  data_email = data_distribution |>
    dplyr::mutate(expert_emailaddress = "janne.adolf@inbo.be")
)
#
# save script
writeLines(
  appsscript_email_test_b,
  paste0(appscript_path, "appsscript_email_test_b.gs")
)
#
# write apps script to send emails
appsscript_email <- create_appsscript_sendemail(
  data_email = data_distribution
  )
#
# save script
writeLines(
  appsscript_email,
  paste0(appscript_path, "appsscript_email.gs")
)
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
