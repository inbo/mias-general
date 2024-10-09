rm(list = ls())
#
# -------------------------------------------------------------------------
#
# authorize access to g-drive (only once)
if (FALSE) googledrive::drive_auth() # make sure to tick all boxes
if (FALSE) googledrive::drive_user() # check user
#
#
# ---upload to g-drive----------------------------------------------------------------------
#
# upload section rmd files (files already uploaded are skipped)
files_list <- list.files(
  path = "source/report/formal/species_overview/sections",
  full.names = TRUE
  )
for (i in seq_along(files_list)){
  file_path_i <- files_list[i]
  try(
    trackdown::upload_file(
    file = file_path_i,
    gpath = "trackdown/species_overview/sections",
    shared_drive = "PRJ_meetnetten-IUS"
    )
  )
}
#
# upload abstract rmd file
try(
  trackdown::upload_file(
    file = "source/report/formal/species_overview/abstract/abstract.Rmd",
    gpath = "trackdown/species_overview",
    shared_drive = "PRJ_meetnetten-IUS"
  )
)
#
# upload pdf final report
# folder ID is in URL
try(
  googledrive::drive_upload(
    media = "output/formal/species_overview/species_overview.pdf",
    path = googledrive::as_id("195yBsTXZ-ow-MG8iT1lw457G8_AwoVnI")
  )
)
#
#
# ---download from g-drive ----------------------------------------------------------------------
#
if (FALSE) {
  trackdown::download_file()
}
#
#
# ---update on g-drive----------------------------------------------------------------------
#
if (FALSE) {
  trackdown::update_file()
}
