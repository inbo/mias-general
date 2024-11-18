rm(list = ls())
#
# -----------------------------------------------------------------------------
#
# define gbif parameters
scientific_names <- c("Muntiacus reevesi (Ogilby, 1839)", "Faxonius virilis (Hagen, 1870)") # not used
country <- "BE"
area_id <- "BEL.2_1" # flanders
basis_of_record <- c(
  "OBSERVATION",
  "HUMAN_OBSERVATION",
  "MATERIAL_SAMPLE",
  "LITERATURE",
  "PRESERVED_SPECIMEN",
  "UNKNOWN",
  "MACHINE_OBSERVATION"
) # so, everything but c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")
year_end <- lubridate::year(Sys.Date())
year_begin <- year_end - 10
hasCoordinate <- TRUE
#
# download
gbif_download_args <- list(
  #rgbif::pred_in("scientificName", scientific_names),
  rgbif::pred("country", country),
  rgbif::pred("gadm", area_id),
  rgbif::pred_in("basisOfRecord", basis_of_record),
  rgbif::pred_gte("year", year_begin),
  rgbif::pred_lte("year", year_end),
  rgbif::pred("hasCoordinate", hasCoordinate)
  #user = rstudioapi::askForPassword("GBIF username"),
  #pwd = rstudioapi::askForPassword("GBIF password"),
  #email = rstudioapi::askForPassword("Email address for notification")
)
gbif_download_key <- do.call(
  eval(parse(text = "rgbif::occ_download")),
  gbif_download_args
)
#
# check status of download
rgbif::occ_download_meta(key = gbif_download_key) |> purrr::pluck("status")
#
# save download key & metadata
gbif_download_meta <- list(
  key = gbif_download_key,
  args = gbif_download_args,
  date = Sys.Date()
)
save(gbif_download_meta,
     file = paste0(
       "data/raw/gbif_download_",
       gbif_download_meta$key |> as.character(),
       ".rda"
     )
)
