rm(list = ls())

# --- get working version species list ------------------

list_1 <- get(load(paste0("data/processed/2025-04-30_species_list.Rda")))
data_1 <- list_1 |> purrr::pluck("data") |> dplyr::arrange(sci_name_gbif_acc)


# --- compare with newer species list ---------------
# list_2 is not used here due to significant changes in vernacular names

list_2 <- get(load(paste0("data/processed/2025-09-04_species_list.Rda")))
data_2 <- list_2 |> purrr::pluck("data") |> dplyr::arrange(sci_name_gbif_acc) |> dplyr::select(- "on_unionlist_2025")

if (FALSE) {

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

}

#
#
# --- only update unionlist 2025 column ---------------
#
species_old_upd_tmp <- data_1 |>
  dplyr::full_join(
    x = _,
    y = list_2 |> purrr::pluck("data") |> dplyr::select(c("sci_name_gbif_acc", "on_unionlist_2025"))
  )

# --- fix capitalization of vernacular names ---------------


# rewind general capitalization vernacular names
firstlower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}
cap_placenames <- function(
    x,
    placenames = c(
      # NL
      "afghaans", "afrikaans", "amerikaans", "australisch", "aziatisch",
      "californisch", "canadese", "chinese", "indische", "japans",
      "nieuw-guinese", "nieuw-zeelandse", "noord-atlantische", "noord-aziatische",
      "noordelijke", "oosterse", "oostelijk", "perzische",
      "sosnowsky", "thaise", "westelijk",
      # EN
      "african", "american", "andean", "asian", "asiatic", "australian",
      "brazilian", "canada", "cape", "carolina", "chilean", "china", "chinese",
      "colombian", "egyptian", "himalayan", "indian", "japanese", "maryland",
      "new guinea", "new zealand", "obama", "persian", "senegal", "sosnowsky"
    )
){
  pattern <- placenames |> paste(x = _, collapse = "|")
  if (grepl(pattern, x)){
    match <- stringr::str_match(x, pattern)
    x_1 <-  match |> stringr::str_to_title()
    x_2 <- sub(match, "X", x)
    x <- sub("X", x_1, x_2)
  }
  x
}

species_old_upd_tmp <- species_old_upd_tmp |>
  dplyr::mutate(
    dplyr::across(
      dplyr::contains(c("nld", "eng")),
      firstlower
    ),
    dplyr::across(
      dplyr::contains(c("nld", "eng")),
      \(x) {sapply(x, cap_placenames) }
    )
  )

species_old_upd_tmp$vern_name_gbif_eng_alt <- stringr::str_replace_all(
  species_old_upd_tmp$vern_name_gbif_eng_alt, "Giant", "giant"
)
species_old_upd_tmp$vern_name_gbif_nld_alt <- stringr::str_replace_all(
  species_old_upd_tmp$vern_name_gbif_nld_alt, "Bastaard", "Boheemse"
)

# --- additional manual adaptions ---------------

# Callosciurus erythraeus
## replace vernacular name in English, which is Spanish
## capitalize vernacular name in Dutch
callosciurusery_vernname_eng <- "Pallas's squirrel"
callosciurusery_vernname_nld <- "Pallas' eekhoorn"

# Callosciurus finlaysonii
## replace vernacular name in English, which is Spanish
callosciurus_vernname_eng <- "Finlayson's squirrel"

# Cervus nippon Temminck
## fix vernacular name in English
cervus_vernname_eng <- "sika deer"

# Cortaderia selloana
## replace vernacular name in English
cortaderia_vernname_eng <- "pampas grass"

# Delairea odorata Lem.
## fix vernacular name in Dutch
delairea_vernname_nld <- "klimopkruiskruid"

# Lysichiton americanus Hultén & H.St.John
## shorten vernacular name in Dutch
lysichiton_vernname_nld <- "moerasaronskelk"

# Marisa cornuarietis
## shorten vernacular name in English
marisa_vernname_eng <- "Colombian ramshorn apple snail"

# misgurnus mohoity (Dybowski, 1869):
## add vernacular name in Dutch and English
misgurnus_vernname_eng <- "amur weatherfish" #"Oriental wheatherfish"
misgurnus_vernname_nld <- "Noord-Aziatische modderkruiper"

# Ondatra zibethicus (Linnaeus, 1766):
## replace vernacular name in English, which is German
ondatra_vernname_eng <- "muskrat"

# Pachycondyla chinensis (Emery, 1895)
## add vernacular name in Dutch
pachycondyla_vernname_nld <- "Aziatische naaldmier"

# Sciurus carolinensis Gmelin
## replace vernacular name in English, which is Spanish
sciuruscaro_vernname_eng <- "eastern grey squirrel"

# Sciurus niger Linnaeus
## replace vernacular name in English, which is Spanish
sciurusniger_vernname_eng <- "fox squirrel"

# Tamias sibiricus
## replace vernacular name in English, which is Spanish
tamais_vernname_eng <- "Siberian chipmunk"


# adapt
species_old_upd <- species_old_upd_tmp |>
  dplyr::mutate(
    vern_name_gbif_eng_alt = dplyr::case_when(
      grepl("Callosciurus erythraeus", sci_name_gbif_acc) ~ callosciurusery_vernname_eng,
      grepl("Callosciurus finlaysonii", sci_name_gbif_acc) ~ callosciurus_vernname_eng,
      grepl("Cervus nippon", sci_name_gbif_acc) ~ cervus_vernname_eng,
      grepl("Cortaderia selloana", sci_name_gbif_acc) ~ cortaderia_vernname_eng,
      grepl("Marisa cornuarietis", sci_name_gbif_acc) ~ marisa_vernname_eng,
      grepl("Misgurnus mohoity", sci_name_gbif_acc) ~ misgurnus_vernname_eng,
      grepl("Ondatra zibethicus", sci_name_gbif_acc) ~ ondatra_vernname_eng,
      grepl("Sciurus carolinensis", sci_name_gbif_acc) ~ sciuruscaro_vernname_eng,
      grepl("Sciurus niger", sci_name_gbif_acc) ~ sciurusniger_vernname_eng,
      grepl("Tamias sibiricus", sci_name_gbif_acc) ~ tamais_vernname_eng,
      TRUE ~ vern_name_gbif_eng_alt
    ),
    vern_name_gbif_nld_alt = dplyr::case_when(
      grepl("Callosciurus erythraeus", sci_name_gbif_acc) ~ callosciurusery_vernname_nld,
      grepl("Delairea odorata", sci_name_gbif_acc) ~ delairea_vernname_nld,
      grepl("Lysichiton americanus", sci_name_gbif_acc) ~ lysichiton_vernname_nld,
      grepl("Misgurnus mohoity", sci_name_gbif_acc) ~ misgurnus_vernname_nld,
      grepl("Pachycondyla chinensis", sci_name_gbif_acc) ~ pachycondyla_vernname_nld,
      TRUE ~ vern_name_gbif_nld_alt
    )
  )

# compare
with_width <- function(width, code) {
  withr::local_options(width = width)
  code
}
test <- lapply(
  seq_along(data_1),
  \(i){
    waldo::compare(
      data_1[, i] ,
      species_old_upd[, i],
      max_diffs = nrow(data_1)
    ) |> with_width(
      width = 100,
      code = _
    )
  }
)
if (FALSE){
  test[[15]]
  test[[16]]
  test[[17]]
  test[[18]]
}


# save
species_list <- list(
  data = species_old_upd,
  meta = list_2$meta
)
save(species_list, file = paste0("data/processed/2025-04-30_species_list_upd.Rda"))
