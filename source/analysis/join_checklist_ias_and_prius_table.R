rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# get PrIUS table
table_prius <- get_table_prius() # assumes table has been downloaded
#
# get IAS checklist
checklist_ias <- get_checklist_ias_gbif(
  taxonomic_status = "ACCEPTED"
  )
#
# get accepted taxon keys
taxon_keys_checklist <- get(
  load("data/processed/taxon_keys_checklist_accepted.Rda")
  )
taxon_keys_prius <- get(
  load("data/processed/taxon_keys_prius_accepted.Rda")
  )
#
# join keys
taxon_keys_joint <- dplyr::full_join(
  taxon_keys_checklist$data,
  taxon_keys_prius$data
  ) |>
  dplyr::relocate(
    key_accepted,
    .before = 1
  )
#
# join checklist and PrIUS table
table_joint <- taxon_keys_joint |>
  dplyr::full_join(
    x = _,
    y = table_prius |> dplyr::rename(key_prius = prius_nubKey)
  ) |>
  dplyr::full_join(
    x = _,
    y = checklist_ias |> dplyr::rename(key_checklist = nubKey)
  )
#
# save
save(table_joint, file = "data/processed/table_joint_checklist_prius.Rda")
