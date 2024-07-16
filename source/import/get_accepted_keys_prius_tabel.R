rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# download PrIUS table
if (!grepl("prius", list.files("data/raw"))) {
  download_table_prius()
}
#
# get PrIUS table
table_prius <- get_table_prius() # assumes table has been downloaded
#
# use scientific names to look up taxonKeys
keys_prius <- rgbif::name_backbone_checklist(
  name = table_prius$prius_scientificName,
  verbose = FALSE # TRUE for more rows and status 'DOUBTFUL'
)
#
# check keys
which(keys_prius$usageKey != table_prius$prius_nubKey)
which(keys_prius$status != "ACCEPTED")
#
# update keys
keys_prius_upd <- keys_prius |>
  dplyr::mutate(
    # add checklist key (assuming same row order in table_prius & keys_prius)
    key_prius = table_prius$prius_nubKey,
    # get accepted keys and store in key_accepted
    key_accepted = dplyr::case_when(
      status == "ACCEPTED" ~ usageKey,
      TRUE ~ acceptedUsageKey
    ),
    .after = usageKey
  )
#
# compare keys
if (FALSE) {
  which(keys_prius_upd$key_accepted != keys_prius_upd$usageKey)
  tmp <- which(keys_prius_upd$key_accepted != keys_prius_upd$key_prius)
  keys_prius_upd$key_prius[tmp]
  View(keys_prius_upd[tmp, ])
}
#
# fix "japanese knotweed" (prius key accepted at species level)
keys_prius_upd <- keys_prius_upd |>
  dplyr::mutate(
    key_accepted = dplyr::case_when(
      grepl("japonica", canonicalName) ~ key_prius,
      TRUE ~ key_accepted
    ),
    .after = usageKey
  )
#
# save keys
taxon_keys <- list(
  data = keys_prius_upd |>
    dplyr::select(
      all_of(
        c("key_prius", "key_accepted"))
    ),
  meta = data.frame(
    variablename = c("key_prius", "key_accepted"),
    content = c(
      paste(
        "contains the first codes from the variable \'GBIF codes\'",
        "from the PrIUS table",
        "available under https://zenodo.org/records/7678524"
      ),
      "contains the accepted GBIF taxonKey"
    ),
    date = Sys.Date()
  )
)
save(taxon_keys, file = "data/processed/taxon_keys_prius_accepted.Rda")
