rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
# -------------------------------------------------------------------------
#
# data frame of species names likely to be added to union list during next round
names_tmp <- c(
    "Castor canadensis",
    "Acacia mearnsii",
    "Acridotheres cristatellus",
    "Asterias amurensis",
    "Bipalium kewense",
    "Brachyponera chinensis",
    "Broussonetia papyrifera",
    "Callinectes sapidus",
    "Cervus nippon",
    "Cherax destructor",
    "Cherax quadricarinatus",
    "Cipangopaludina chinensis",
    "Cortaderia selloana",
    "Delairea odorata",
    "Faxonius immunis",
    "Marisa cornuarietis",
    "Misgurnus anguillicaudatus",
    "Misgurnus bipartitus",
    "Mulinia lateralis",
    "Myiopsitta monachus",
    "Nanozostera japonica",
    "Obama nungara",
    "Platydemus manokwari",
    "Pycnonotus jocosus",
    "Tradescantia fluminensis",
    "Vespa mandarinia"
)
#
# get keys
keys_tmp <- rgbif::name_backbone_checklist(name = names_tmp) |>
  select("usageKey") |>
  unlist()
#
# get names
names_ius_add_tmp <- get_names_gbif(
keys = c(keys_tmp)
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
names_ius_add <- list(
  data = names_ius_add_tmp,
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
save(names_ius_add, file = "data/processed/names_ius_add.Rda")
