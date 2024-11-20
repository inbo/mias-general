rm(list = ls())
#
# -------------------------------------------------------------
#
# definitions
gbif_download_key <- "0021545-241107131044228"
sqlite_file <- paste0("data/gbif_occcubes/",
                      gbif_download_key,
                      "_occurrence.sqlite")
table_name <- "occ"
# (gbif download key see data/raw/)
# (sqlite file data/processed)
#
# open connection to data base
sqlite_occ <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
#
# get geographical data
query <- glue::glue_sql("SELECT {`cols`*} FROM {table}",
                        cols = c("decimalLatitude",
                                 "decimalLongitude",
                                 "coordinateUncertaintyInMeters"),
                        table = table_name,
                        .con = sqlite_occ
)
geodata_df <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::as_tibble() |>
  dplyr::mutate(coordinateUncertaintyInMeters = as.double(coordinateUncertaintyInMeters))
#
# number of occurrences
nrow(geodata_df)
#
# preview occurrences
geodata_df |> head(10)
#
# number of occurrences within each "coordinateUncertaintyInMeters" value
geodata_df |>
  dplyr::group_by(coordinateUncertaintyInMeters) |>
  dplyr::count() |>
  dplyr::arrange(dplyr::desc(dplyr::n())) |>
  View()
#
# number of occurrences without "coordinateUncertaintyInMeters" or zero value
geodata_df |>
  dplyr::filter(is.na(coordinateUncertaintyInMeters) |
                  coordinateUncertaintyInMeters == 0) |>
  dplyr::count()
#
# assign 1000m to occurrences without or zero uncertainty
geodata_df <-
  geodata_df |>
  dplyr::mutate(coordinateUncertaintyInMeters = dplyr::if_else(
    is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters == 0,
    1000.0,
    coordinateUncertaintyInMeters
  ))
#
# also in sqlite table
query <- glue::glue_sql(
  "PRAGMA synchronous = OFF",
  .con = sqlite_occ
)
RSQLite::dbExecute(sqlite_occ, query)
RSQLite::dbBegin(sqlite_occ)
query <- glue::glue_sql(
  "UPDATE {table} SET {`column`} = 1000 WHERE
            {`column`} = '' OR {`column`} = 0",
  table = table_name,
  column = "coordinateUncertaintyInMeters",
  .con = sqlite_occ)
RSQLite::dbExecute(sqlite_occ, query)
RSQLite::dbCommit(sqlite_occ)
#
# get all coordinate uncertainties from sqlite table
query <- glue::glue_sql("SELECT DISTINCT {`column`} FROM {table}",
                        table = table_name,
                        column = "coordinateUncertaintyInMeters",
                        .con = sqlite_occ
)
uncertainty_df <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::as_tibble()
uncertainty_df
#
# check uncertainties
assertthat::assert_that(!0 %in% uncertainty_df)
assertthat::assert_that(!NA %in% uncertainty_df)
#
# ---assign grid to occurrences------------------------------------------------
#
tmpfile_coords <-paste0(
  "data/gbif_occcubes/",
  gbif_download_key,
  "_coordinates_and_uncertainty_epsg_4326.tsv"
)
col_names_geodata_df <- names(geodata_df)
readr::write_tsv(geodata_df, tmpfile_coords, na = "")
remove(geodata_df)
#
# set seed (reproduce data on zenodo)
set.seed(10058400)
#
# define function
reproject_assign <- function(df, pos){
  # Step 1: reprojection
  nrow_df <- nrow(df)
  sp::coordinates(df) <- ~ decimalLongitude + decimalLatitude
  sp::proj4string(df) <- sp::CRS("+init=epsg:4326")
  df <- sp::spTransform(df, sp::CRS("+init=epsg:3035"))
  colnames(df@coords) <- c("x", "y")
  # Step 2: assign occurrence within uncertainty circle
  df@data <-
    df@data |>
    dplyr::mutate(random_angle = runif(nrow_df, 0, 2*pi))
  df@data <-
    df@data |>
    dplyr::mutate(random_r = sqrt(runif(
      nrow_df, 0, 1)) * coordinateUncertaintyInMeters)
  df@data <-
    df@data |>
    dplyr::mutate(x = df@coords[, "x"],
                  y = df@coords[, "y"])
  df@data <-
    df@data |>
    dplyr::mutate(x = x + random_r * cos(random_angle),
                  y = y + random_r * sin(random_angle)) |>
    dplyr::select(-c(random_angle, random_r))

  # Step 3: Find grid cell the occurrence belongs to
  df@data <-
    df@data |>
    dplyr::mutate(eea_cell_code = paste0(
      "1km",
      "E", floor(x/1000),
      "N", floor(y/1000))) |>
    dplyr::select(x, y, coordinateUncertaintyInMeters, eea_cell_code)
  return(df@data)
}
#
# apply reprojection
chunk_size <- 1000000
geodata_df <- readr::read_tsv_chunked(
  file = tmpfile_coords,
  callback = readr::DataFrameCallback$new(reproject_assign),
  chunk_size = chunk_size,
  col_types = readr::cols(.default = readr::col_double()),
  na = ""
)
#
# preview data
geodata_df |> head(n = 10)
#
# remove tmp file
file.remove(tmpfile_coords)
#
# add grid cell code to sqlite file
new_col <- "eea_cell_code"
query <- glue::glue_sql("ALTER TABLE {table} ADD COLUMN {colname} {type}",
                        table = table_name,
                        colname = new_col,
                        type = "CHARACTER",
                        .con = sqlite_occ
)
RSQLite::dbExecute(sqlite_occ, query)
RSQLite::dbBegin(sqlite_occ)
RSQLite::dbExecute(
  sqlite_occ,
  glue::glue_sql(
    "UPDATE {table} SET {`column`} = :eea_cell_code WHERE _ROWID_ = :id",
    table = table_name,
    column = new_col,
    .con = sqlite_occ),
  params = data.frame(
    eea_cell_code = geodata_df$eea_cell_code,
    id = rownames(geodata_df))
)
RSQLite::dbCommit(sqlite_occ)
#
# preview
query <- glue::glue_sql("SELECT * FROM {table} WHERE _ROWID_ <= 10",
                        table = table_name,
                        .con = sqlite_occ)
RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::select(eea_cell_code , dplyr::everything())
#
# close connection
RSQLite::dbDisconnect(sqlite_occ)
