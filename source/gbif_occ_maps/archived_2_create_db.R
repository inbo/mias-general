rm(list = ls())
#
# ---create data base----------------------------------------------------------
#
# gbif download key (see downloads in data/raw/)
gbif_download_key <- "0005016-241126133413365" #adapt manually
#
# paths
gis_data_path <- "data" #"G:/Mijn Drive/gis_data" ## change back
occcubes_data_path <- paste(gis_data_path, "gbif_occcubes", NULL, sep = "/")
#
# download zip file
zip_file <- paste0(occcubes_data_path, gbif_download_key, ".zip")
if (!file.exists(zip_file)) {
  occ <- rgbif::occ_download_get(
    key = gbif_download_key,
    path = occcubes_data_path
  )
}
#
# unzip occurrence text file
occ_file <- paste0(occcubes_data_path, "/", gbif_download_key, "_occurrence.txt")
# remove:
occ_file <- paste0("G:/Mijn Drive/gis_data/gbif_occcubes/",gbif_download_key, "_occurrence.txt")
if (!file.exists(occ_file)) {
  unzip(zipfile = zip_file,
        files = "occurrence.txt",
        exdir = occcubes_data_path)
  file.rename(from = paste0(occcubes_data_path, "occurrence.txt"), to = occ_file)
}
#
# get column names
cols_occ_file <- readr::read_delim(occ_file, "\t", n_max = 1, quote = "") |>
  names()
length(cols_occ_file)
#
# create sqlite file
sqlite_file <- paste0(occcubes_data_path, gbif_download_key, "_occurrence.sqlite")
table_name <- "occ_all"
#
# define storage classes
#
# text
field_types <- rep("TEXT", length(cols_occ_file))
names(field_types) <- cols_occ_file
#
# integers
int_fields <- grep(
  pattern = "Key|DayOfYear|year|month|day",
  x = names(field_types),
  value = TRUE
) |>
  grep(pattern = "datasetKey", x = _, value = TRUE, invert = TRUE)
field_types[which(names(field_types) %in% int_fields)] <- "INTEGER"
# real
real_fields <- grep(
  pattern = "decimal|coordinate|pointRadiusSpatialFit",
  x = names(field_types),
  value = TRUE
)
field_types[which(names(field_types) %in% real_fields)] <- "REAL"
#
# write txt to sqlite
sqlite_occ <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
if (!table_name %in% RSQLite::dbListTables(sqlite_occ)) {
  RSQLite::dbWriteTable(
    conn = sqlite_occ,
    name = table_name,
    sep = "\t",
    value = occ_file,
    row.names = FALSE,
    header = TRUE,
    field.types = field_types,
    overwrite = TRUE
  )
}
#
#
#
# number of cols
cols_occ_db <- RSQLite::dbListFields(sqlite_occ, table_name)
length(cols_occ_db)
#
# number of occurrences
query <- glue::glue_sql(
  "SELECT COUNT(1) FROM {table}",
  table = table_name,
  .con = sqlite_occ
)
n_occs_total <- RSQLite::dbGetQuery(sqlite_occ, query)
n_occs_total
#
# view first 100 rows
query <- glue::glue_sql("SELECT * FROM {table} LIMIT 100",
                        table = table_name,
                        .con = sqlite_occ
)
preview_df <- RSQLite::dbGetQuery(conn = sqlite_occ, query)
preview_df
#
# check geographical coordinates
query <- glue::glue_sql("SELECT * FROM {table} WHERE
                  decimalLatitude IS NULL OR
                  decimalLongitude IS NULL",
                        table = table_name,
                        .con = sqlite_occ)
suspect_coords_df <- RSQLite::dbGetQuery(sqlite_occ, query)
suspect_coords_df
#
# ---filter data--------------------------------------------------------------
#
# columns to use
cols_to_use <- c(
  "gbifID", "scientificName", "kingdom", "phylum", "class", "order", "family",
  "genus", "specificEpithet", "infraspecificEpithet", "taxonRank",
  "taxonomicStatus", "datasetKey", "basisOfRecord", "occurrenceStatus",
  "lastInterpreted", "hasCoordinate", "hasGeospatialIssues", "decimalLatitude",
  "decimalLongitude", "coordinateUncertaintyInMeters", "coordinatePrecision",
  "pointRadiusSpatialFit", "verbatimCoordinateSystem", "verbatimSRS",
  "eventDate", "startDayOfYear", "endDayOfYear", "year", "month", "day",
  "verbatimEventDate", "samplingProtocol", "samplingEffort", "issue",
  "identificationVerificationStatus", "taxonKey", "acceptedTaxonKey",
  "kingdomKey", "phylumKey", "classKey", "orderKey", "familyKey", "genusKey",
  "subgenusKey", "speciesKey", "species"
)
cols_to_use <- cols_to_use[which(cols_to_use %in% cols_occ_db)]
#
# get storage class of columns to use
field_types_subset <- field_types[which(names(field_types) %in% cols_to_use)]
field_types_subset
#
# define filters
issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE",
  "COORDINATE_INVALID",
  "COUNTRY_COORDINATE_MISMATCH"
)
occurrenceStatus_to_discard <- c(
  "absent",
  "excluded"
)
identificationVerificationStatus_to_discard <- c(
  "unverified",
  "unvalidated",
  "not validated",
  "under validation",
  "not able to validate",
  "control could not be conclusive due to insufficient knowledge",
  "uncertain",
  "unconfirmed",
  "unconfirmed - not reviewed",
  "validation requested"
)
#
# create index based on issue columns
idx_occStatus_issue <- "idx_verifStatus_occStatus_issue"
query <- glue::glue_sql(
  "PRAGMA index_list({table_name})",
  table_name = table_name,
  .con = sqlite_occ
)
indexes_all <- RSQLite::dbGetQuery(sqlite_occ, query)
if (!idx_occStatus_issue %in% indexes_all$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {`idx`} ON {table_name} ({`cols_idx`*})",
    idx = idx_occStatus_issue,
    table_name = table_name,
    cols_idx = c("identificationVerificationStatus",
                 "occurrenceStatus",
                 "issue"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# add % for SQLite string matching
issues_to_discard <- paste0("\'%", issues_to_discard, "%\'")
#
# create subquery for issues
issue_condition <- paste("issue NOT LIKE", issues_to_discard, collapse = " AND ")
issue_condition
#
# create new table with filtered data
table_name_subset <- "occ"
if (!table_name_subset %in% RSQLite::dbListTables(sqlite_occ)) {
  RSQLite::dbCreateTable(conn = sqlite_occ,
                         name = table_name_subset,
                         fields = field_types_subset)
  query <- glue::glue_sql(
    "INSERT INTO {small_table} SELECT {`some_cols`*} FROM {big_table} WHERE
  LOWER(identificationVerificationStatus) NOT IN ({unverified*}) AND LOWER(occurrenceStatus) NOT IN ({bad_status*}) AND ", issue_condition,
    small_table = table_name_subset,
    some_cols = names(field_types_subset),
    big_table = table_name,
    unverified = identificationVerificationStatus_to_discard,
    bad_status = occurrenceStatus_to_discard,
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# ---check filtered data--------------------------------------------------------
#
# check that table been created
assertthat::assert_that(
  table_name_subset %in% RSQLite::dbListTables(sqlite_occ)
)
#
# columns
RSQLite::dbListFields(sqlite_occ, name = table_name_subset)
#
# number of occurrences
query <- glue::glue_sql(
  "SELECT COUNT(1) FROM {table}",
  table = table_name_subset,
  .con = sqlite_occ
)
n_occs <- RSQLite::dbGetQuery(sqlite_occ, query)
n_occs_total$`COUNT(1)`
#
# check that there always are geographical coordinates
query <- glue::glue_sql("SELECT * FROM {table} WHERE
                  decimalLatitude IS NULL OR
                  decimalLongitude IS NULL",
                        table = table_name_subset,
                        .con = sqlite_occ)
invalid_coords <- RSQLite::dbGetQuery(sqlite_occ, query)
assertthat::assert_that(nrow(invalid_coords) == 0)
#
# create an index for occurrence status
idx_occStatus <- "idx_occStatus"
query <- glue::glue_sql(
  "PRAGMA index_list({table_name})",
  table_name = table_name_subset,
  .con = sqlite_occ
)
indexes <- RSQLite::dbGetQuery(sqlite_occ, query)
if (!idx_occStatus %in% indexes$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {idx} ON {table_name} ({cols_idx})",
    idx = idx_occStatus,
    table_name = table_name_subset,
    cols_idx = c("occurrenceStatus"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# check occurrence status left in filtered data
query <- glue::glue_sql(
  "SELECT DISTINCT occurrenceStatus FROM {table}",
  table = table_name_subset,
  .con = sqlite_occ
)
RSQLite::dbGetQuery(sqlite_occ, query)
#
# create an index for "issue"
idx_issue <- "idx_issue"
if (!idx_issue %in% indexes$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {idx} ON {table_name} ({cols_idx})",
    idx = idx_issue,
    table_name = table_name_subset,
    cols_idx = c("issue"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# check issues left in filtered data
query <- glue::glue_sql(
  "SELECT DISTINCT issue FROM {table}",
  table = table_name_subset,
  .con = sqlite_occ
)
issues_left <- RSQLite::dbGetQuery(sqlite_occ, query)
issues_left
#
# check whether unwanted issues are left
assertthat::assert_that(
  !any(purrr::map_lgl(issues_to_discard,
                      function(issue) {
                        any(stringr::str_detect(issues_left$issue, issue))
                      }))
)
#
# create index on identification verification status
idx_issue <- "idx_identificationVerificationStatus"
if (!idx_issue %in% indexes$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {idx} ON {table_name} ({cols_idx})",
    idx = idx_issue,
    table_name = table_name_subset,
    cols_idx = c("identificationVerificationStatus"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# check identification verification status left in filtered data
query <- glue::glue_sql("SELECT identificationVerificationStatus, COUNT(*)
                  FROM {table}
                  GROUP BY identificationVerificationStatus
                  ORDER BY 2 DESC",
                        table = table_name_subset,
                        .con = sqlite_occ)
status_verification_left <- RSQLite::dbGetQuery(sqlite_occ, query)
status_verification_left
#
# check indices generated
query <- glue::glue_sql(
  "PRAGMA index_list({table_name})",
  table_name = table_name_subset,
  .con = sqlite_occ
)
RSQLite::dbGetQuery(sqlite_occ, query)
#
# close connection
RSQLite::dbDisconnect(sqlite_occ)

