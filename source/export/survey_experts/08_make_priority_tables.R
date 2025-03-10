
rm(list = ls())

knitr::opts_chunk$set(echo = FALSE, message = FALSE)
options(knitr.kable.NA = '')

list.files("../../functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()

# path to locally saved processed response data 
response_data_path <- "../../../data/survey_experts/"


#
#
#
# --- load response data ---------------
#
res_comb_upd <- get(load(paste0(response_data_path, "results_combined_upd.rda")))
res_meth_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda")))
# check: species are missing: Morone americana, Fundulus heteroclitus
# Lampropeltis getula: "geen" reported
res_meth_options <- get(load((paste0(response_data_path, "recoded_processed/", "results_methods_options.rda"))))
#
res_comb_upd <- res_comb_upd |>
  dplyr::filter(
    # on unionlist
    on_unionlist,
    # not irrelvant according to prius
    !grepl("IRR", prius_stadium),
    # freshwater animals
    grepl("dier", kingdom),
    grepl("freshwater", prius_milieu)
  )
res_meth_recoded <- res_meth_recoded |>
  dplyr::filter(
    species %in% res_comb_upd$species |> unique()
  )
#
#
#
# --- create table: surveillance scope ---------------
#
table_surv <- res_comb_upd |>
  dplyr::select(tidyselect::contains(c("species", "name", "stadium"))) |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  # add surveillance scope via columns
  dplyr::mutate(
    detection = dplyr::case_when(
      grepl("afwezig|sporadisch|wijd", stadium) ~ 1,
      TRUE ~ 0
    ),
    inventory = dplyr::case_when(
      grepl("beperkt", stadium) ~ 1,
      TRUE ~ 0
    ),
    distribution = dplyr::case_when(
      grepl("beperkt|wijd", stadium) ~ 1,
      TRUE ~ 0
    ),
    abundance = dplyr::case_when(
      grepl("beperkt|wijd", stadium) ~ 1,
      TRUE ~ 0
    )
  )
#
#
#
# --- create table: methods ---------------
#
res_meth_options <- res_meth_options |>
  dplyr::rename(
    method = "response_options",
    method_cat = "response_options_cat"
  )
#
# add methods in long format
table_meth_all <- res_meth_recoded |>
  dplyr::rename(method_tmp = "response_text_final") |>
  dplyr::filter(!grepl("followup", question_text)) |>
  dplyr::select(tidyselect::contains(c("question_id", "species", "name", "method"))) |>
  tidyr::crossing(res_meth_options) |>
  dplyr::group_by(species) |>
  dplyr::rowwise() |>
  dplyr::filter(grepl(pattern = method, x = method_tmp)) |>
  dplyr::ungroup()
#
# get best methods
# not full species list???
table_meth_best <- table_meth_all |>
  dplyr::filter(
    grepl("D2", question_id)
  ) |>
  dplyr::mutate(method_best = 1) |>
  dplyr::select(tidyselect::all_of(c("species", "method", "method_best")))
#
# add information reported for best method
table_info <- res_comb_upd |>
  dplyr::filter(grepl("D3|D4|D5|D6|D7", question_id)) |>
  dplyr::filter(!grepl("followup", question_text)) |>
  # prep reshape to wide (HERE add pre "meth_")
  dplyr::mutate(
    property = dplyr::case_when(
      grepl("Sensitiviteit", question_text_short) ~ "sensitivity",
      grepl("Specificiteit", question_text_short) ~ "specificity",
      grepl("Kosten", question_text_short) ~ "costs",
      grepl("Scope", question_text_short) ~ "scope",
      grepl("Veldprotocol", question_text_short) ~ "protocol"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = species,
    names_from = property,
    values_from = response_text
  )
table_meth_best_upd <- dplyr::full_join(
  x = table_meth_best,
  y = table_info
)
#
# add info best method as additional column
table_meth <- dplyr::full_join(
  x = table_meth_all |>
    dplyr::filter(
      grepl("D1", question_id)
    ),
  y = table_meth_best_upd
)
#
#
#
# --- create table step: monitoring area ---------------
#
# add management locations here as well?
#
table_area <- res_comb_upd |>
  dplyr::filter(grepl("A1|B1", question_id), !grepl("followup", question_text)) |>
  # filter out sporadically present and question on distribution area
  dplyr::filter(!(grepl("B1", question_id) & grepl("sporadisch", stadium))) |>
  dplyr::select(
    tidyselect::contains(c("question_id","species", "name", "stadium"))
    | tidyselect::ends_with("response_text")
  ) |>
  # monitoring area
  dplyr::mutate(
    area = dplyr::case_when(
      grepl("afwezig", stadium) ~ "introduction locations",
      grepl("sporadisch", stadium) ~ "introduction and historical locations",
      grepl("beperkt|wijd", stadium) ~ "distribution area"
    )
  ) |>
  # monitoring area known?
  dplyr::rename(
    area_known = "response_text"
  ) |>
  tidyr::drop_na(area_known)
#
#
#
# --- create base table ---------------
#
table_base <- dplyr::full_join(
  # remove cells based on invasion stadium
  table_surv,
  table_meth |> dplyr::select(tidyselect::ends_with(c("species", "method", "scope")))
) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(c("detection", "inventory", "distribution", "abundance")),
      \(x) dplyr::case_when(
        x == 1 ~ method,
        TRUE ~ NA_character_
      )
    )
  ) |>
  # remove cells based on suitability (scope) of method
  dplyr::mutate(
    abundance = dplyr::case_when(
      grepl("aan- of afwezigheid", scope) ~ NA_character_,
      is.na(scope) ~ NA_character_,
      TRUE ~ abundance
    )
  ) |>
  # remove cells based on status in conservation area
  dplyr::full_join(
    x = _,
    y = table_area
  ) |>
  dplyr::mutate(
    detection = dplyr::case_when(
      grepl("NAT", prius_stadium) ~ NA_character_,
      TRUE ~ detection
    )
  ) |>
  # remove cells based on surveillance area
  dplyr::mutate(
    inventory = dplyr::case_when(
      grepl("verspreiding is voldoende gekend", area_known) ~ NA_character_,
      TRUE ~ inventory
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(c("distribution", "abundance")),
      \(x) dplyr::case_when(
        grepl("verspreiding is niet voldoende gekend|weet het niet", area_known) ~ NA_character_,
        TRUE ~ x
      )
    )
  ) |>
  dplyr::select(
    tidyselect::contains(c("name_nld","detection", "inventory", "distribution", "abundance", "species", "stadium"))
  )


# prepare for display
table_base_upd <- table_base |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::contains(c("species", "stadium")),
      \(x) kableExtra::cell_spec(
        x = x,
        color = "lightgrey"
      )
    )
  )


make_table <- function(
    data_table
) {
  knitr::kable(
    data_table,
    format = "html",
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed", "hover"),
      full_width = FALSE, 
      position = "left",
      font_size = 11
    ) |>
    kableExtra::column_spec(
      column = 2, 
      background = "#FFFFAF"
    ) |>
    kableExtra::column_spec(
      column = 3, 
      background = "#FFC5D6"
    ) |>
    kableExtra::column_spec(
      column = 4, 
      background = "#DD9EDA"
    ) |>
    kableExtra::column_spec(
      column = 5, 
      background = "#C9C9FB"
    ) |>
    kableExtra::collapse_rows(
      columns = 1,
      valign = "top"
    ) 
}

make_table(table_base_upd)



#
#
#
# --- filter base table based on urgency ---------------
#
# calculate mean urgency
res_urge <- res_comb_upd |>
  dplyr::filter(
    grepl("urge", score_crit)
  ) |>
  dplyr::group_by(species) |>
  dplyr::mutate(m_urge = do.call("mean", list(response_score, na.rm = TRUE))) |>
  dplyr::filter(dplyr::row_number() == 1) |>
  dplyr::ungroup() |>
  dplyr::select(tidyselect::starts_with(c("species","m","vern"))) |>
  dplyr::arrange(dplyr::desc(m_urge))
#
#
table_filtered_1 <- dplyr::full_join(
  table_base,
  res_urge
) |>
  # adapt cell appearance
  dplyr::mutate(
    color = dplyr::case_when(
      m_urge >= 3.0 ~ "black",
      TRUE ~ "grey50"
    ),
    bold = dplyr::case_when(
      m_urge >= 3.0 ~ TRUE,
      TRUE ~ FALSE
    ),
    strikeout = dplyr::case_when(
      m_urge >= 3.0 ~ FALSE,
      TRUE ~ TRUE
    )
  ) |> dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(c("detection", "inventory", "distribution", "abundance")),
      \(x) dplyr::case_when(
        !is.na(x) ~ kableExtra::cell_spec(
          x = x,
          color = color,
          bold = bold,
          strikeout = strikeout
        ),
        is.na(x) ~ x
      )
    )
  ) |> 
  dplyr::mutate(
    dplyr::across(
      tidyselect::contains(c("species", "stadium")),
      \(x) kableExtra::cell_spec(
        x = x,
        color = "lightgrey"
      )
    )
  ) |>
  dplyr::select(-tidyselect::all_of(c("m_urge", "vern_name_eng", "color", "bold", "strikeout")))


make_table(table_filtered_1)

