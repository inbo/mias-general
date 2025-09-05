rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
#
# --- get species list from survey expert sheet ---------------
#
# from file "Prius_data_modified_extended.gsheet"
# g-drive path: PRJ_MIUS\_overkoepelend\data\species
#
species_sheetid_args <- list(
  sheet_id = "1sd-AXrETsRI01XIry5YU2V8Vl-_UU9y2CwgM3y01fTk",
  tab_variablename = NULL,
  colnames_old = c("soort", "species", "groep"),
  colnames_new = c("species", "sci_name", "kingdom"),
  gbif_namevariable = "sci_name"
)

#
# 'sci_name_gbif_acc' and 'key_gbif_acc' identical for both options
species <- do.call(
  process_speciessheet,
  species_sheetid_args
)
#
#
# test: all species unique?
assertthat::are_equal(
  species$key_gbif_acc |> unique() |> length(),
  nrow(species)
)
#
species_upd_tmp1 <- species |>
  dplyr::rename(sci_name_gsheet = "sci_name") |>
  dplyr::select(dplyr::contains(c("unielijst", "kingdom", "taxon", "int_groep", "species", "milieu", "sci_name", "gbif")))
#
#
#
# --- add status information ---------------
#
# file "Prius_data_modified_extended.gsheet" already contains such information
#
species_upd_tmp2 <- species_upd_tmp1 |>
  # 1) indicator for union list
  dplyr::rename(
    tidyselect::any_of(
      c(on_unionlist = "unielijst", on_unionlist_2025 = "unielijst_2025")
      )
    ) |>
  dplyr::mutate(dplyr::across(dplyr::contains("on_unionlist"), as.logical)) |>
  # 2) status in Flanders according to PrIUS report
  dplyr::rename(tidyselect::any_of(
    c(prius_stadium = "int_groep", prius_milieu = "milieu"))
    )
#
#
#
# --- add vernacular names in English, Dutch, French, German ---------------
#
vern_names <- lapply(
  species_upd_tmp2$key_gbif_acc,
  function(x){
    tmp <- try(
      rgbif::name_usage(key = x, data = "vernacularNames") |>
      purrr::pluck("data") |>
      dplyr::filter(grepl("eng|nld|fra|deu", language)) |>
      dplyr::distinct(language, .keep_all = TRUE) |>
      dplyr::select(dplyr::all_of(c("taxonKey", "vernacularName", "language"))) |>
      tidyr::pivot_wider(
        names_from = language,
        values_from = vernacularName,
        names_prefix = "vern_name_gbif_"
      ),
      silent = TRUE
    )
    out <- if (tmp |> inherits(x = _, "try-error")){
      data.frame(
        "taxonKey" = x
      )
    } else {
      tmp
    }
    return(out)
  }) |>
  dplyr::bind_rows()
#
#
species_upd_tmp3 <- dplyr::left_join(
  species_upd_tmp2,
  vern_names,
  by = c("key_gbif_acc" = "taxonKey")
) |>
  dplyr::rename(vern_name_gsheet_nld = "species") |>
  dplyr::relocate(vern_name_gsheet_nld, .before = vern_name_gbif_nld) |>
  dplyr::relocate(vern_name_gbif_eng, vern_name_gbif_fra, .after = vern_name_gbif_nld) |>
  # capitalize vernacular names
  dplyr::mutate(
    dplyr::across(
      dplyr::contains(c("nld", "eng")),
      stringr::str_to_sentence
    )
  )
#
#
#
# --- some manual adaptions ---------------
#
# 1) vespa velutina:
# use sci name (and key) of https://www.gbif.org/species/1311477
# Vespa velutina Lepeletier, 1836
# instead of https://www.gbif.org/species/6247411 (subspecies)
# Vespa velutina nigrithorax Buysson, 1905
vespa_velutina_name <- "Vespa velutina Lepeletier, 1836"
#
# 2) knotweeds:
# group Reynoutria japonica Houtt.,
# Reynoutria sachalinensis (F.Schmidt) Nakai,
# Reynoutria ×bohemica Chrtek & Chrtková"
knotweed_names <- species_upd_tmp3 |>
  dplyr::filter(grepl("Reynoutria", sci_name_gbif_acc)) |>
  dplyr::pull(sci_name_gbif_acc) |>
  _[c(2,3,1)] |> # sort according to naming in survey
  paste(x = _, collapse = ", ")
knotweed_vernnames_eng <- if (TRUE) {
  species_upd_tmp3 |>
    dplyr::filter(grepl("Reynoutria", sci_name_gbif_acc)) |>
    dplyr::pull(vern_name_gbif_eng) |>
    _[c(2,3,1)] |> # sort according to naming in survey
    paste(x = _, collapse = ", ") |>
    gsub(" knotweed", "", x = _) |>
    paste(x = _, "knotweed")
} else {
  "invasive knotweeds"
}
knotweed_vernnames_nld <- if (TRUE) {
  "Japanse, Sachalinse, Bastaard duizendknoop"
} else {
  species_upd_tmp3 |>
    dplyr::filter(grepl("Reynoutria japonica", sci_name_gbif_acc)) |>
    dplyr::pull(vern_name_gsheet_nld)
}
#
# 3) misgurnus mohoity (Dybowski, 1869):
# add vernacular name in Dutch and English
misgurnus_vernname_eng <- "Oriental wheatherfish"
misgurnus_vernname_nld <- "Noord-aziatische modderkruiper"
#
# 4) Ondatra zibethicus (Linnaeus, 1766):
# replace vernacular name in English, which is German
ondatra_vernname_eng <- "Muskrat"
#
# 5) Pachycondyla chinensis (Emery, 1895)
# add vernacular name in Dutch
pachycondyla_vernname_nld <- "Aziatische Naaldmier"

# adapt
species_upd <- species_upd_tmp3 |>
  dplyr::mutate(
    sci_name_gbif_acc_alt = dplyr::case_when(
      grepl("Vespa velutina", sci_name_gbif_acc) ~ vespa_velutina_name,
      grepl("Reynoutria", sci_name_gbif_acc) ~ knotweed_names,
      TRUE ~ NA_character_
    ),
    key_gbif_acc_alt = dplyr::case_when(
      grepl("Vespa velutina", sci_name_gbif_acc) ~ rgbif::name_backbone(vespa_velutina_name)$usageKey,
      TRUE ~ NA_real_
    ),
    vern_name_gbif_eng_alt = dplyr::case_when(
      grepl("Misgurnus mohoity", sci_name_gbif_acc) ~ misgurnus_vernname_eng,
      grepl("Reynoutria", sci_name_gbif_acc) ~ knotweed_vernnames_eng,
      grepl("Ondatra zibethicus", sci_name_gbif_acc) ~ ondatra_vernname_eng,
      TRUE ~ NA_character_
    ),
    vern_name_gbif_nld_alt = dplyr::case_when(
      grepl("Misgurnus mohoity", sci_name_gbif_acc) ~ misgurnus_vernname_nld,
      grepl("Reynoutria", sci_name_gbif_acc) ~ knotweed_vernnames_nld,
      grepl("Pachycondyla", sci_name_gbif_acc) ~ pachycondyla_vernname_nld,
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(-tidyselect::all_of("code int_groep")) |>
  dplyr::relocate(sci_name_gbif_acc_alt, .before = key_gbif_acc) |>
  dplyr::relocate(key_gbif_acc_alt, .after = key_gbif_acc) |>
  dplyr::relocate(vern_name_gbif_eng_alt, .after = vern_name_gbif_eng) |>
  dplyr::relocate(vern_name_gbif_nld_alt, .after = vern_name_gbif_nld) |>
  dplyr::relocate(vern_name_gbif_deu, .after = vern_name_gbif_fra)
#
#
#
# --- save list ---------------
#
# save names
species_list <- list(
  data = species_upd,
  meta = data.frame(
    variablename = c(
      "on_unionlist",
      "on_unionlist_2025",
      "kingdom",
      "taxon",
      "prius_stadium",
      "prius_milieu",
      "sci_name_gsheet",
      "sci_name_gbif",
      "status_sci_name_gbif",
      "sci_name_gbif_acc",
      "status_sci_name_gbif_acc",
      "sci_name_gbif_acc_alt",
      "key_gbif_acc",
      "key_gbif_acc_alt",
      "vern_name_gsheet_nld",
      "vern_name_gbif_nld",
      "vern_name_gbif_nld_alt",
      "vern_name_gbif_eng",
      "vern_name_gbif_eng_alt",
      "vern_name_gbif_fra",
      "vern_name_gbif_deu"
    ),
    content = c(
      "whether the species is on the union list in 2022",
      "whether the species is on the union list in 2025",
      "kingdom of species",
      "taxon of species",
      "stadium in Flanders according to prius report",
      "milieu according to prius report",
      "scientific name in original gsheet",
      "scientific name in GBIF matching scientific name in original gsheet, retrieved via rgbif::name_backbone_checklist",
      "status in GBIF of sci_name_gbif",
      "accepted scientific name in GBIF matching sci_name_gbif",
      "status in GBIF of sci_name_gbif_acc",
      "alternatives for sci_name_gbif_acc",
      "accepted species key in GBIF",
      "alternatives for key_gbif_acc",
      "dutch vernacular name in original gsheet",
      "dutch vernacular name in GBIF",
      "alternatives for vern_name_gbif_nld",
      "english vernacular name in GBIF",
      "alternatives for vern_name_gbif_eng",
      "french vernacular name in GBIF",
      "german vernacular name in GBIF"
    ),
    date = Sys.Date()
  )
)
save(species_list, file = paste0("data/processed/", Sys.Date(), "_species_list.Rda"))
#
#
#
# --- create gsheet for review ---------------
#
load(paste0("data/processed/", Sys.Date(), "_species_list.Rda"))
#
# sheet name
review_sheet_name <- paste0(Sys.Date(), "_CONTROL_species_list")
#
# upload sheet
googlesheets4::gs4_create(
  name =  review_sheet_name,
  sheets = species_list
)
# move sheet to target folder
tmp_id <- googledrive::drive_find(
  pattern = review_sheet_name,
  type = "spreadsheet"
) |> googledrive::as_id()
googledrive::drive_mv(
  file = tmp_id,
  path = "https://drive.google.com/drive/folders/1m0dzMZlIyvHe5CEIyly7Ps47NUT8oRSH" |> paste0(x = _, "/")
)
#
#
#
# --- compare 2 species lists ---------------
#
list_1 <- get(load(paste0("data/processed/2025-04-30_species_list.Rda")))
list_2 <- get(load(paste0("data/processed/2025-09-04_species_list.Rda")))
data_1 <- list_1 |> purrr::pluck("data") |> dplyr::arrange(sci_name_gbif_acc)
data_2 <- list_2 |> purrr::pluck("data") |> dplyr::arrange(sci_name_gbif_acc) |> dplyr::select(- "on_unionlist_2025")


with_width <- function(width, code) {
  withr::local_options(width = width)
  code
}
test <- lapply(
  seq_along(data_1),
  \(i){
    waldo::compare(
      data_1[, i] ,
      data_2[, i],
      max_diffs = nrow(data_1)
    ) |> with_width(
      width = 100,
      code = _
    )
  }
)

test[[7]] # sci_name_gbif
test[[15]] # vern_name_gbif_nld
test[[16]] # vern_name_gbif_nld_alt
test[[17]] # vern_name_gbif_eng
test[[18]] # vern_name_gbif_eng_alt
#test[[19]] # vern_name_gbif_fra
#test[[20]] # vern_name_gbif_deu
#
#
#
# --- only update unionlist 2025 column ---------------
#
species_old_upd_tmp <- data_1 |>
  dplyr::full_join(
    x = _,
    y = list_2 |> purrr::pluck("data") |> dplyr::select(c("sci_name_gbif_acc", "on_unionlist_2025"))
  )

# reduced manual adaptions
# 3) misgurnus mohoity (Dybowski, 1869):
# add vernacular name in Dutch
misgurnus_vernname_nld <- "Noord-aziatische modderkruiper"
#
# 4) Ondatra zibethicus (Linnaeus, 1766):
# replace vernacular name in English, which is German
ondatra_vernname_eng <- "Muskrat"
#
# 5) Pachycondyla chinensis (Emery, 1895)
# add vernacular name in Dutch
pachycondyla_vernname_nld <- "Aziatische Naaldmier"

# adapt
species_old_upd <- species_old_upd_tmp |>
  dplyr::mutate(
    vern_name_gbif_eng_alt = dplyr::case_when(
      grepl("Ondatra zibethicus", sci_name_gbif_acc) ~ ondatra_vernname_eng,
      TRUE ~ vern_name_gbif_eng_alt
    ),
    vern_name_gbif_nld_alt = dplyr::case_when(
      grepl("Misgurnus mohoity", sci_name_gbif_acc) ~ misgurnus_vernname_nld,
      grepl("Pachycondyla", sci_name_gbif_acc) ~ pachycondyla_vernname_nld,
      TRUE ~ vern_name_gbif_nld_alt
    )
  )

# compare
waldo::compare(data_1, species_old_upd)

# save
species_list <- list(
  data = species_old_upd,
  meta = list_2$meta
)
save(species_list, file = paste0("data/processed/2025-04-30_species_list_upd.Rda"))
