#
# --- definitions questions ---------------
#
# id of sheet with questions to be asked in form
# currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\questions.gsheet
sheet_id <- "1MikuShtt9mFdb5f2Nzts7pFR5MZ6HR7h-6Lx0bcEF7k"
#
# path to locally save questions
questions_path <- "source/export/survey_experts/questions/"
#
#
# --- definitions forms ---------------
#
# titlebase for forms to be created
form_titlebase <- "survey"
#
# filename species names
species_sheet_id <- "1dClhdsk1QMHniYv6xcFVTKd6pLtWBDz-8avvK-xZHQ0"
species_sheet_args <- list(
  sheet_id = species_sheet_id,
  tab_variablename = "soortengroep",
  colnames_old = c("soort", "wetenschappelijke naam", "expert email", "expert voornaam", "expert achternaam"),
  colnames_new = c("species", "sci_name", "expert_email", "expert_firstname", "expert_lastname"),
  gbif_namevariable = "sci_name"
)
#
# url of g-drive folder to save forms in
# PRJ_MIUS/_overkoepelend/bevraging_soortenexperts/survey
form_folder_url <- "https://drive.google.com/drive/folders/1_swosPhu5mWcLfhvBw8cg6rHUlneJfWc"
#
# url of g-drive folder with form media
media_folder_url <- "https://drive.google.com/drive/folders/1vvnnT_CKx4_Ph1k9rDc1_SXmdhdVMbqv"

# url of g-drive folder with distribution map pngs
map_folder_url <- "https://drive.google.com/drive/folders/16okbiFiL4jEGT_CL93SbItCvBWKsB2R8"
#
# path to locally save google apps scripts
appscript_path <- "source/export/survey_experts/appsscripts/"
#
#
# --- definitions form distribution---------------
#
# url of g-drive folder to save ingredients for survey distribution in
# currently: PRJ_MIUS/_overkoepelend/bevraging_soortenexperts/distribution
distribution_folder_url <- "https://drive.google.com/drive/folders/1GPNJGmDoaaT4P2U51cUkWBQKR8cvtRnw"
#
#
