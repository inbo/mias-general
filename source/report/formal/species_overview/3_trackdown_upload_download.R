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
# root directory
path <- "source/report/formal/species_overview/"
#
# section files
section_files <- list.files(
  path = paste0(path, "sections"),
  full.names = TRUE
) |>
  grep(pattern = "skeleton", invert = TRUE, value = TRUE)
#
# section skeleton file
skeleton_file <- list.files(
  path = paste0(path, "section_template/skeleton/"),
  full.names = TRUE
)
#
# abstract file
abstract_file <- list.files(
  path = paste0(path, "abstract/"),
  full.names = TRUE
)
#
# pdf report
pdf_file <- list.files(
  path = "output/formal/species_overview/",
  full.names = TRUE,
  pattern = "pdf"
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
        gpath = "trackdown/species_overview/sections",
        shared_drive = "PRJ_meetnetten-IUS"
      ))
  )
}
#
# upload/update skeleton rmd file
try(
  do.call(
    what = eval(parse(text = which_fun)),
    args = list(
      file = skeleton_file,
      gpath = "trackdown/species_overview/section_skeleton",
      shared_drive = "PRJ_meetnetten-IUS"
    ))
)
#
# upload/update abstract rmd file
try(
  do.call(
    what =   eval(parse(text = which_fun)),
    args = list(
      file = abstract_file,
      gpath = "trackdown/species_overview/abstract",
      shared_drive = "PRJ_meetnetten-IUS"
    ))
)
#
# upload/update pdf final report
# folder ID is in URL
try(
  googledrive::drive_upload(
    media = pdf_file,
    path = googledrive::as_id("195yBsTXZ-ow-MG8iT1lw457G8_AwoVnI")
  )
)
#
# ---download from g-drive -------------------------------------------------
#
if (FALSE) {
  trackdown::download_file()
}
#
