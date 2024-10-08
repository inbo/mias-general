rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# get accepted taxon keys
taxon_keys_prius <- get(
  load("data/processed/taxon_keys_prius_accepted.Rda")
)
#
# get names
names_prius_tmp <- get_names_gbif(
  keys = taxon_keys_prius$data$key_accepted
) |>
  dplyr::select(all_of(
    c(
      "canonicalName",
      "scientificName",
      "vernacularName"
    )
  )
  )
#
# save names
names_prius <- list(
data = names_prius_tmp,
meta = data.frame(
  variablename = c(
    "canonicalName",
    "scientificName",
    "vernacularName"
    ),
  content = c(
    "contains the GBIF variable canonicalName",
    "contains the GBIF variable scientificName",
    "contains the GBIF variable vernacularName"
  ),
  date = Sys.Date()
)
)
save(names_prius, file = "data/processed/names_prius.Rda")
