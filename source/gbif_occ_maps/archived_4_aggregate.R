rm(list = ls())
#
# -------------------------------------------------------------
#
# definitions
gbif_download_key <- "0021545-241107131044228"
sqlite_file <- paste0("data/processed/",
                      gbif_download_key,
                      "_occurrence.sqlite")
table_name <- "occ"
# (gbif download key see data/raw/)
# (sqlite file data/processed)
#
# open connection to data base
sqlite_occ <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
#
# ---aggregate at class level----------------------------------------------------------
#
# create index for class
idx_baseline <- "idx_year_cell_class"
query <- glue::glue_sql(
  "PRAGMA index_list({table_name})",
  table_name = table_name,
  .con = sqlite_occ
)
indexes_all <- RSQLite::dbGetQuery(sqlite_occ, query)
if (!idx_baseline %in% indexes_all$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {`idx`} ON {table_name} ({`cols_idx`*})",
    idx = idx_baseline,
    table_name = table_name,
    cols_idx = c("year",
                 "eea_cell_code",
                 "classKey",
                 "coordinateUncertaintyInMeters"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# group and count occurrences
query <- glue::glue_sql(
  "SELECT {`cols`*}, COUNT(_ROWID_), MIN({`coord_uncertainty`}) FROM {table} GROUP BY {`cols`*}",
  cols = c("year", "eea_cell_code", "classKey"),
  coord_uncertainty = "coordinateUncertaintyInMeters",
  table = table_name,
  .con = sqlite_occ
)
occ_cube_baseline <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::mutate(classKey = as.numeric(classKey)) |>
  dplyr::rename(
    n = "COUNT(_ROWID_)",
    min_coord_uncertainty = "MIN(`coordinateUncertaintyInMeters`)"
  )
#
# preview
occ_cube_baseline |> head()
#
# get class names
class_df <- dplyr::tibble(
  classKey = unique(occ_cube_baseline$classKey))
class_df <-
  class_df |>
  dplyr::mutate(class = purrr::map_chr(
    .data$classKey,
    function(x) {
      if (!is.na(x)) {
        taxon <- rgbif::name_usage(x)
        taxon$data |> dplyr::pull(scientificName)
      } else {
        NA_character_
      }
    })
  )
class_df
#
# ---aggregate at species level----------------------------------------------------------
#
# select species
#
# GRIIS checklist
# (https://www.gbif.org/dataset/6d9e952f-948c-4483-9807-575348147c7e)
griis_checklist_key <- "6d9e952f-948c-4483-9807-575348147c7e"
alien_taxa <- rgbif::name_usage(
  datasetKey = griis_checklist_key,
  limit = 10000)[["data"]] |>
  dplyr::filter(origin == "SOURCE")
#
# count by rank and taxonomic status
alien_taxa |>
  dplyr::group_by(rank, taxonomicStatus) |>
  dplyr::count()
#
# selection
alien_taxa_species <-
  alien_taxa |>
  dplyr::filter(rank %in% c("SPECIES", "GENUS", "FAMILY"),
                taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"))
#
# preview
alien_taxa_species |> head()
#
# get unique nubKeys
alien_taxa_species_key <-
  alien_taxa_species |>
  dplyr::distinct(nubKey) |>
  dplyr::pull(nubKey)
#
# create index for year, eea_cell_code, various keys, coordinate uncertainty
idx_species_year_cell <- "idx_species_genus_family_year_cell"
query <- glue::glue_sql(
  "PRAGMA index_list({table_name})",
  table_name = table_name,
  .con = sqlite_occ
)
indexes_all <- RSQLite::dbGetQuery(sqlite_occ, query)
if (!idx_species_year_cell %in% indexes_all$name) {
  query <- glue::glue_sql(
    "CREATE INDEX {`idx`} ON {table_name} ({`cols_idx`*})",
    idx = idx_species_year_cell,
    table_name = table_name,
    cols_idx = c("year",
                 "eea_cell_code",
                 "speciesKey",
                 "genusKey",
                 "familyKey",
                 "coordinateUncertaintyInMeters"),
    .con = sqlite_occ
  )
  RSQLite::dbExecute(sqlite_occ, query)
}
#
# group by year, eea_cell_code, keys and get for each group lowest uncertainty
query <- glue::glue_sql(
  "SELECT {`cols`*}, COUNT(_ROWID_), MIN({`coord_uncertainty`}) FROM {table} GROUP BY {`cols`*}",
  cols = c("year",
           "eea_cell_code",
           "speciesKey",
           "genusKey",
           "familyKey"),
  coord_uncertainty = "coordinateUncertaintyInMeters",
  table = table_name,
  .con = sqlite_occ
)
occ_cube_species <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::rename(
    n = "COUNT(_ROWID_)",
    min_coord_uncertainty = "MIN(`coordinateUncertaintyInMeters`)"
  )
occ_cube_species <-
  occ_cube_species |>
  dplyr::mutate(speciesKey = as.integer(speciesKey),
                genusKey = as.integer(genusKey),
                familyKey = as.integer(familyKey)) |>
  dplyr::filter(speciesKey %in% alien_taxa_species_key |
                  genusKey %in% alien_taxa_species_key |
                  familyKey %in% alien_taxa_species_key)
#
# remove occurrences at genus or family level
occ_cube_species <-
  occ_cube_species |>
  dplyr::filter(!is.na(speciesKey) & speciesKey != 0)
#
# number of alien species included
paste(
  length(alien_taxa_species_key[
    which(alien_taxa_species_key %in% c(unique(occ_cube_species$speciesKey),
                                        unique(occ_cube_species$genusKey),
                                        unique(occ_cube_species$familyKey)))]),
  "out of",
  length(alien_taxa_species_key)
)
#
# drop genus and family key
occ_cube_species <-
  occ_cube_species |>
  dplyr::select(-c(genusKey, familyKey))
#
# preview
occ_cube_species |> head()
#
# get distinct taxa
query <- glue::glue_sql(
  "SELECT DISTINCT {`cols`*} FROM {table}",
  cols = c("speciesKey",
           "genusKey",
           "familyKey",
           "taxonKey",
           "scientificName"),
  table = table_name,
  .con = sqlite_occ
)
occ_cube_species_taxa <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::mutate(speciesKey = as.integer(speciesKey),
                genusKey = as.integer(genusKey),
                familyKey = as.integer(familyKey)) |>
  dplyr::filter(speciesKey %in% alien_taxa_species_key |
                  genusKey %in% alien_taxa_species_key |
                  familyKey %in% alien_taxa_species_key)
#
# drop taxa at genus or family level
occ_cube_species_taxa <-
  occ_cube_species_taxa |>
  dplyr::filter(!is.na(speciesKey) & speciesKey != 0)
#
#
occ_cube_species_taxa <-
  occ_cube_species_taxa |>
  dplyr::select(-c(genusKey, familyKey))
#
#
occ_cube_species_taxa |>
  dplyr::group_by(speciesKey) |>
  dplyr::add_tally() |>
  dplyr::ungroup() |>
  dplyr::filter(n > 1) |>
  dplyr::select(-n) |>
  dplyr::arrange(speciesKey, taxonKey)
#
#
occ_cube_species_taxa |>
  dplyr::group_by(speciesKey) |>
  dplyr::count() |>
  dplyr::rename(n_taxa = n) |>
  dplyr::left_join(occ_cube_species_taxa, by = "speciesKey") |>
  dplyr::group_by(speciesKey, n_taxa) |>
  dplyr::filter(taxonKey != speciesKey) |>
  dplyr::count() |>
  dplyr::rename(n_taxonKey_not_speciesKey = n) |>
  dplyr::filter(n_taxonKey_not_speciesKey == n_taxa) |>
  dplyr::left_join(occ_cube_species_taxa |>
                     dplyr::filter(speciesKey != taxonKey),
                   by = "speciesKey") |>
  dplyr::ungroup() |>
  dplyr::select(-c(n_taxa, n_taxonKey_not_speciesKey)) |>
  dplyr::arrange(speciesKey, taxonKey)
#
#
taxa_species <-
  occ_cube_species_taxa |>
  # get unique 'speciesKey'
  dplyr::distinct(speciesKey) |>
  # extract speciesKey
  dplyr::pull(speciesKey) |>
  # GBIF query via name_usage
  purrr::map(~rgbif::name_usage(key = .x)) |>
  # Select data
  purrr::map(~.x[["data"]]) |>
  # Select columns of interest
  purrr::map(~dplyr::select(.x,
                            speciesKey,
                            scientificName,
                            rank,
                            taxonomicStatus,
                            kingdom)) |>
  # Merge all taxa in a data.frame
  purrr::reduce(dplyr::full_join) |>
  # rename 'scientificName' to 'species_scientificName'
  dplyr::rename(species_scientificName = scientificName) |>
  # add these columns to original df
  dplyr::right_join(occ_cube_species_taxa, by = "speciesKey") |>
  # group by 'speciesKey'
  dplyr::group_by(speciesKey,
                  species_scientificName,
                  rank,
                  taxonomicStatus,
                  kingdom) |>
  # create 'includes' column
  dplyr::summarize(includes = paste(
    taxonKey,
    scientificName,
    sep = ": ",
    collapse = " | ")) |>
  # rename 'species_scientificName' to 'scientificName'
  dplyr::rename(scientificName = species_scientificName)
taxa_species
#
#
## Aggregate occurrences for infraspecific taxa
#
rank_under_species <- c("SUBSPECIFICAGGREGATE",
                        "SUBSPECIES",
                        "VARIETY",
                        "SUBVARIETY",
                        "FORM",
                        "SUBFORM"
)
alien_taxa_subspecies <-
  alien_taxa |>
  dplyr::filter(rank %in% rank_under_species,
                taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"))
alien_taxa_subspecies
#
#
alien_taxa_subspecies_key <-
  alien_taxa_subspecies |>
  dplyr::distinct(nubKey) |>
  dplyr::pull(nubKey)
#
#
query <- glue::glue_sql(
  "SELECT {`cols`*} FROM {table} WHERE acceptedTaxonKey IN ({subspecies_key*})",
  cols = c("year",
           "eea_cell_code",
           "acceptedTaxonKey",
           "coordinateUncertaintyInMeters"),
  subspecies_key = alien_taxa_subspecies_key,
  table = table_name,
  .con = sqlite_occ
)
occ_cube_subspecies <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::group_by(year, eea_cell_code, acceptedTaxonKey) |>
  dplyr::summarize(
    n = dplyr::n(),
    min_coord_uncertainty = min(coordinateUncertaintyInMeters)) |>
  dplyr::ungroup()
#
#
paste(
  length(alien_taxa_subspecies_key[
    which(alien_taxa_subspecies_key %in%
            unique(occ_cube_subspecies$acceptedTaxonKey))]),
  "out of",
  length(alien_taxa_subspecies_key)
)
#
#
occ_cube_subspecies |> head()
#
#
### Map taxa
#
#
query <- glue::glue_sql(
  "SELECT DISTINCT {`cols`*} FROM {table}
  WHERE acceptedTaxonKey IN ({subspecies_key*})",
  cols = c("taxonKey",
           "acceptedTaxonKey",
           "scientificName"),
  subspecies_key = alien_taxa_subspecies_key,
  table = table_name,
  .con = sqlite_occ
)
occ_cube_subspecies_taxa <-
  RSQLite::dbGetQuery(sqlite_occ, query)
#
#
occ_cube_subspecies_taxa |>
  dplyr::group_by(acceptedTaxonKey) |>
  dplyr::add_tally() |>
  dplyr::filter(n > 1) |>
  dplyr::select(-n) |>
  dplyr::arrange(acceptedTaxonKey)
#
#
occ_cube_subspecies_taxa |>
  dplyr::group_by(acceptedTaxonKey) |>
  dplyr::count() |>
  dplyr::rename(n_taxa = n) |>
  dplyr::left_join(occ_cube_subspecies_taxa, by = "acceptedTaxonKey") |>
  dplyr::group_by(acceptedTaxonKey, n_taxa) |>
  dplyr::filter(taxonKey != acceptedTaxonKey) |>
  dplyr::count() |>
  dplyr::rename(n_taxonKey_not_acceptedKey = n) |>
  dplyr::filter(n_taxonKey_not_acceptedKey == n_taxa) |>
  dplyr::left_join(occ_cube_subspecies_taxa |>
                     dplyr::filter(acceptedTaxonKey != taxonKey),
                   by = "acceptedTaxonKey") |>
  dplyr::ungroup() |>
  dplyr::select(-c(n_taxa, n_taxonKey_not_acceptedKey))
#
#
taxa_subspecies <-
  occ_cube_subspecies_taxa |>
  # get unique 'acceptedTaxonKey'
  dplyr::distinct(acceptedTaxonKey) |>
  # extract acceptedTaxonKey
  dplyr::pull(acceptedTaxonKey) |>
  # GBIF query via name_usage
  purrr::map(~rgbif::name_usage(key = .x)) |>
  # Select data
  purrr::map(~.x[["data"]]) |>
  # Merge all taxa in a data.frame
  purrr::reduce(dplyr::full_join) |>
  # rename 'scientificName' to 'accepted_scientificName'
  dplyr::rename(accepted_scientificName = scientificName)
#
# # HERE
#
# are synonyms present?
if ("acceptedKey" %in% names(taxa_subspecies)) {

  taxa_subspecies <-
    taxa_subspecies |>

    # populate 'acceptedKey' column for not synonyms
    dplyr::mutate(acceptedKey = case_when(
      is.na(acceptedKey) ~ key,
      !is.na(acceptedKey) ~ acceptedKey)
    )
} else {
  taxa_subspecies <-
    taxa_subspecies |>

    # create column 'acceptedKey'
    dplyr::mutate(acceptedKey = key)
}

taxa_subspecies <-
  taxa_subspecies |>

  # select columns of interest
  dplyr::select(acceptedKey, accepted_scientificName, rank, taxonomicStatus, kingdom) |>

  # add columns to original df
  dplyr::right_join(occ_cube_subspecies_taxa,
                    by = c("acceptedKey" = "acceptedTaxonKey")) |>

  # group by accepted taxon
  dplyr::group_by(acceptedKey,
                  accepted_scientificName,
                  rank,
                  taxonomicStatus,
                  kingdom) |>

  # create 'includes' column
  dplyr::summarize(includes = paste(
    taxonKey,
    scientificName,
    sep = ": ",
    collapse = " | ")) |>

  # rename 'accepted_scientificName' to 'scientificName'
  dplyr::rename(scientificName = accepted_scientificName)

taxa_subspecies
#
# HERE
#
alien_taxa_synonyms <-
  alien_taxa |>
  dplyr::filter(!taxonomicStatus %in% c("ACCEPTED", "DOUBTFUL"))
alien_taxa_synonyms
#
#
alien_taxa_synonyms |>
  dplyr::group_by(rank) |>
  dplyr::count() |>
  dplyr::arrange(desc(n))
#
#
alien_taxa_synonyms_key <-
  alien_taxa_synonyms |>
  dplyr::distinct(nubKey) |>
  dplyr::pull(nubKey)
#
#
query <- glue::glue_sql(
  "SELECT {`cols`*} FROM {table} WHERE taxonKey IN ({synonym_key*})",
  cols = c("year",
           "eea_cell_code",
           "taxonKey",
           "coordinateUncertaintyInMeters"),
  synonym_key = alien_taxa_synonyms_key,
  table = table_name,
  .con = sqlite_occ
)
occ_cube_synonym <-
  RSQLite::dbGetQuery(sqlite_occ, query) |>
  dplyr::group_by(year, eea_cell_code, taxonKey) |>
  dplyr::summarize(
    n = dplyr::n(),
    min_coord_uncertainty = min(coordinateUncertaintyInMeters)
  )
#
#
paste(
  length(alien_taxa_synonyms_key[
    which(alien_taxa_synonyms_key %in%
            unique(occ_cube_synonym$taxonKey))]),
  "out of",
  length(alien_taxa_synonyms_key)
)
#
#
occ_cube_synonym |> head()
#
#
### Map taxa
#
taxa_synonym <-
  if (length(alien_taxa_synonyms_key) > 0) {
    # create vector with synonyms keys present in occurrence cube
    alien_taxa_synonyms_key[
      which(alien_taxa_synonyms_key %in%
              unique(occ_cube_synonym$taxonKey))] |>

      # GBIF query via name_usage
      purrr::map(~rgbif::name_usage(key = .x)) |>

      # Select data
      purrr::map(~.x[["data"]]) |>

      # select columns of interest
      purrr::map(~dplyr::select(.x, key, scientificName, rank, taxonomicStatus, kingdom)) |>

      # Merge all taxa in a data.frame
      purrr::reduce(dplyr::full_join) |>

      # rename 'key' to 'taxonKey'
      dplyr::rename(taxonKey = key) |>

      # create 'includes' column
      dplyr::mutate(includes = paste(
        taxonKey,
        scientificName,
        sep = ": ")
      )
  } else {
    NULL
  }
taxa_synonym
#
#
save_path <- "data/gbif_occcubes/"
#
#
readr::write_csv(occ_cube_baseline,
                 paste0(save_path, "gbif_fla_classes_cube.csv"),
                 na = "")
#
#
## Save aggregated occurrences and mapped taxa for Belgium
#
### Merge aggregated data
#
#
head(occ_cube_species)
#
#
head(occ_cube_subspecies)
#
#
head(occ_cube_synonym)
#
#
occ_cube_species <-
  occ_cube_species |>
  dplyr::rename(taxonKey = speciesKey)
#
#
occ_cube_subspecies <-
  occ_cube_subspecies |>
  dplyr::rename(taxonKey = acceptedTaxonKey)
#
#
be_alientaxa_cube <-
  occ_cube_species |>
  dplyr::bind_rows(occ_cube_subspecies) |>
  dplyr::bind_rows(occ_cube_synonym)
#
#
### Merge taxa
#
#
head(taxa_species)
#
#
head(taxa_subspecies)
#
#
head(taxa_synonym)
#
#
taxa_species <-
  taxa_species |>
  dplyr::rename(taxonKey = speciesKey)
#
#
taxa_subspecies <-
  taxa_subspecies |>
  dplyr::rename(taxonKey = acceptedKey)
#
#
taxa <-
  taxa_species |>
  dplyr::bind_rows(taxa_subspecies) |>
  dplyr::bind_rows(taxa_synonym)
#
#
readr::write_csv(be_alientaxa_cube,
                 paste0(save_path, "gbif_fla_alientaxa_cube.csv"),
                 na = "")
#
#
### Save taxa
#
readr::write_csv(taxa,
                 paste0(save_path, "gbif_fla_alientaxa_info.csv"),
                 na = "")
#
#
RSQLite::dbDisconnect(sqlite_occ)
#
#
