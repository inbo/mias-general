rm(list = ls())
#
# -------------------------------------------------------------------------
#
# authorize access to g-drive (only once)
# trackdown version 1.5.1
# https://claudiozandonella.github.io/trackdown/articles/oauth-client-configuration.html
#
#
# ---define file paths-------------------------------------------------------
#
# root directory of files
path <- "source/export/survey_experts/docu_report"
#
# section files
section_files <- list.files(
  path = paste(path, "chapters", sep = "/"),
  full.names = TRUE,
  pattern = ".qmd"
) |>
  grep("EN", x = _, invert = TRUE, value = TRUE)
#
# output file
pdf_file <- list.files(
  path = "output/survey_experts/docu_report/",
  full.names = TRUE,
  pattern = ".pdf"
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
        gpath = "trackdown/report_expert_survey/v3_sections",
        shared_drive = "PRJ_MIUS",
        hide_code = TRUE,
        open = FALSE
      ))
  )
}
#
#
# upload/update output
# folder ID is in URL
try(
  googledrive::drive_upload(
    media = pdf_file,
    # "trackdown/report_expert_survey/v2_output_pdf"
    path = googledrive::as_id("1mzMDE3BDiMOHmQnP-gl2_QbYVRFQfOPP")
  )
)
#
# ---download from g-drive -------------------------------------------------
#
if (FALSE) {
  i <- 1
  file_path_i <- section_files[i]
  try(
    trackdown::download_file(
      file = file_path_i,
      gpath = "trackdown/report_expert_survey/v2_sections_accepted",
      shared_drive = "PRJ_MIUS"
    )
  )

}
#
