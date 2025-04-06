rm(list = ls())
#
# -------------------------------------------------------------------------
#
# authorize access to g-drive (only once)
if (FALSE) googledrive::drive_auth() # make sure to tick all boxes
if (FALSE) googledrive::drive_user() # check user
#
#
# ---define file paths-------------------------------------------------------
#
# root directory of files
path <- "source/export/survey_experts/docu_presentation"
#
# section files
section_files <- list.files(
  path = paste(path, "sections", sep = "/"),
  full.names = TRUE,
  pattern = ".qmd"
)
#
# html presentation
html_file <- list.files(
  path = "output/survey_experts/docu_presentation/",
  full.names = TRUE,
  pattern = "index.html"
)
#
# ---upload to / update on g-drive-------------------------------------------
#
# upload or update?
which_fun <- "trackdown::upload_file" # upload_file OR update_file
#
# upload/update section rmd files (files already uploaded are skipped)
for (i in seq_along(section_files)){
  file_path_i <- section_files[i]
  try(
    do.call(
      what = eval(parse(text = which_fun)),
      args = list(
        file = file_path_i,
        gpath = "trackdown/steering_committee_presentation/sections",
        shared_drive = "PRJ_MIUS"
      ))
  )
}
#
#
# upload/update html presentation
# folder ID is in URL
try(
  googledrive::drive_upload(
    media = html_file,
    path = googledrive::as_id("1-nL25x8H48iIqBkmRVOOwwYCZJuazm2i")
  )
)
#
# ---download from g-drive -------------------------------------------------
#
if (FALSE) {
  trackdown::download_file()
}
#
