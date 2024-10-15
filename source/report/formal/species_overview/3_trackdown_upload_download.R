rm(list = ls())
#
# -------------------------------------------------------------------------
#
# authorize access to g-drive (only once)
if (FALSE) googledrive::drive_auth() # make sure to tick all boxes
if (FALSE) googledrive::drive_user() # check user
#
#
# ---upload to / update on g-drive-------------------------------------------
#
# file list
files_list <- list.files(
  path = "source/report/formal/species_overview/sections",
  full.names = TRUE
)|>
  grep(pattern = "skeleton", invert = TRUE, value = TRUE)
#
# upload or update?
which_fun <- "trackdown::update_file" # upload_file OR update_file
#
# upload/update section rmd files (files already uploaded are skipped)
for (i in seq_along(files_list)){
  file_path_i <- files_list[i]
  try(
    do.call(
      what =   eval(parse(text = which_fun)),
      args = list(
        file = file_path_i,
        gpath = "trackdown/species_overview/sections",
        shared_drive = "PRJ_meetnetten-IUS"
      ))
  )
}
#
# upload/update abstract rmd file
try(
  do.call(
    what =   eval(parse(text = which_fun)),
    args = list(
      file = "source/report/formal/species_overview/abstract/abstract.Rmd",
      gpath = "trackdown/species_overview",
      shared_drive = "PRJ_meetnetten-IUS"
    ))
)
#
# upload/update pdf final report
# folder ID is in URL
try(
  googledrive::drive_upload(
    media = "output/formal/species_overview/species_overview.pdf",
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
