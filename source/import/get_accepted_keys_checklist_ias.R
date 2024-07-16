rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# get IAS checklist
checklist_ias <- get_checklist_ias_gbif(
  taxonomic_status = "ACCEPTED"
)
#
# use scientific names to look up taxonKeys
keys_checklist <- rgbif::name_backbone_checklist(
  name = checklist_ias$scientificName,
  verbose = FALSE # TRUE for more rows and status 'DOUBTFUL'
)
#
# check keys
which(keys_checklist$usageKey != checklist_ias$nubKey)
which(keys_checklist$status != "ACCEPTED")
#
# update keys
keys_checklist_upd <- keys_checklist |>
  dplyr::mutate(
    # add checklist key (assuming same row order checklist_ias & keys_checklist)
    key_checklist = checklist_ias$nubKey,
    # get accepted keys and store in key_accepted
    key_accepted = dplyr::case_when(
      status == "ACCEPTED" ~ usageKey,
      TRUE ~ acceptedUsageKey
    ),
    .after = usageKey
  )
#
# compare keys
which(keys_checklist_upd$key_accepted != keys_checklist_upd$usageKey)
tmp <- which(
  keys_checklist_upd$key_accepted != keys_checklist_upd$key_checklist
  )
keys_checklist_upd$key_checklist[tmp]
#
# save keys
taxon_keys <- list(
  data = keys_checklist_upd |>
    dplyr::select(
      all_of(
        c("key_checklist", "key_accepted"))
    ),
  meta = data.frame(
    variablename = c("key_checklist", "key_accepted"),
    content = c(
      paste(
        "contains the variable \'nubKey\' from the GBIF IAS checklist",
        "available under gbif.org/dataset/79d65658-526c-4c78-9d24-1870d67f8439"
      ),
      "contains the accepted GBIF taxonKey"
    ),
    date = Sys.Date()
  )
)
save(taxon_keys, file = "data/processed/taxon_keys_checklist_accepted.Rda")
