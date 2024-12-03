rm(list = ls())
#
# -----------------------------------------------------------------------------
#
# define polygon flanders + buffer
buffer <- 1000*100 # in meters
fla_borders <- sf::st_read("data/gis/prius/flanders_wgs84.geojson") # flanders
fla_borders_buffer <- sf::st_buffer(x = fla_borders, dist = buffer)
plot_check <- ggplot2::ggplot() + # plot to check
  ggplot2::geom_sf(data = fla_borders_buffer, fill = "blue", size = 0.2) +
  ggplot2::geom_sf(data = fla_borders, fill = "coral", size = 0.2)
fla_borders_buffer_txt <- fla_borders_buffer |>
  sf::st_geometry() |>
  sf::st_as_text() |>
  wk::wkt() |>
  wk::wk_orient()
# CHECK: https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html
#
# define gbif parameters
scientific_names <- get(
  load("data/processed/names_prius.Rda")
) |>
  purrr::pluck("data")  |>
  dplyr::pull(dplyr::contains("scientific"))
country <- "BE" # not used
area_id <- "BEL.2_1" # flanders # not used
# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html
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
  rgbif::pred_in("scientificName", scientific_names),
  #rgbif::pred("country", country),
  #rgbif::pred("gadm", area_id),
  rgbif::pred_within(fla_borders_buffer_txt),
  rgbif::pred_in("basisOfRecord", basis_of_record),
  rgbif::pred_gte("year", year_begin),
  rgbif::pred_lte("year", year_end),
  rgbif::pred("hasCoordinate", hasCoordinate)
  #user = rstudioapi::askForPassword("GBIF username"),
  #pwd = rstudioapi::askForPassword("GBIF password"),
  #email = rstudioapi::askForPassword("Email address for notification")
)
#
#
#
# set up sql query
## see example for species list
## https://raw.githubusercontent.com/damianooldoni/b3cubes-sql-examples/refs/heads/main/examples/other_examples/digital_cat_biodiversity_poland_strepsiptera.json
sql_query <- c("
SELECT
\"year\",
    GBIF_EEARGCode(
      1000,
      decimalLatitude,
      decimalLongitude,
      COALESCE(coordinateUncertaintyInMeters, 1000)
    ) AS eeaCellCode,
    classKey,
    class,
    speciesKey,
    species,
    COUNT(*) AS occurrences,
    MIN(COALESCE(coordinateUncertaintyInMeters, 1000)) AS minCoordinateUncertaintyInMeters,
    MIN(GBIF_TemporalUncertainty(eventDate)) AS minTemporalUncertainty,
    IF(ISNULL(classKey), NULL, SUM(COUNT(*)) OVER (PARTITION BY classKey)) AS classCount
  FROM
    occurrence
  WHERE
    occurrenceStatus = 'PRESENT'
    AND speciesKey IN (4480653, 4480642, 4480637, 4480628, 4480626, 4480624, 9719065)
    AND continent = 'EUROPE'
    AND \"year\" >= 1900
    AND hasCoordinate = TRUE
    AND speciesKey IS NOT NULL
    AND NOT ARRAY_CONTAINS(issue, 'ZERO_COORDINATE')
    AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_OUT_OF_RANGE')
    AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_INVALID')
    AND NOT ARRAY_CONTAINS(issue, 'COUNTRY_COORDINATE_MISMATCH')
    AND LOWER(identificationVerificationStatus) NOT IN (
      'unverified',
      'unvalidated',
      'not validated',
      'under validation',
      'not able to validate',
      'control could not be conclusive due to insufficient knowledge',
      'uncertain',
      'unconfirmed',
      'unconfirmed - not reviewed',
      'validation requested'
      )
  GROUP BY
    \"year\",
    eeaCellCode,
    classKey,
    class,
    speciesKey,
    species
  ORDER BY
    \"year\" DESC,
    eeaCellCode ASC,
    speciesKey ASC;
  ")





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
