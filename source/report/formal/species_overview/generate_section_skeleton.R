rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# get species names
names <- get(
  load("data/processed/names_prius.Rda")
) |>
  purrr::pluck("data")
#
# make (temporary) selection
names_tmp <- names |>
  dplyr::filter(
    grepl(
      "Muntiacus reevesi|Faxonius virilis|Procambarus clarkii",
      scientificName
      )
  )
#
# create section rmds
for (i in seq_along(names_tmp[[1]])) {
  #
  # define names
  name_can_i <- names_tmp$canonicalName[i]
  name_ven_i <- names_tmp$vernacularName[i]
  name_full_i <- paste(name_can_i, name_ven_i, sep = " | ") |>
    gsub(x = _, pattern = " \\| NA", replacement = "")
  filename_i <- name_can_i |>
    gsub(x = _, pattern = " ", replacement = "_") |>
    tolower()
  #
  # define ouput file
  file_i <- paste0(
    "source/report/formal/species_overview/sections/", filename_i, ".Rmd"
  )
  #
  # create new rmd file based on template
  try(
    rmarkdown::draft(
      file = file_i,
      template = "source/report/formal/species_overview/section_template",
      edit = FALSE
    )
  )
  #
  # change first headline
  rmd_tmp <- readLines(con = file_i)
  rmd_tmp[1] <- paste("#", name_full_i)
  writeLines(text = rmd_tmp, con = file_i)
}
