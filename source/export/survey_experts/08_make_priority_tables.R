rm(list = ls())

list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()

# path to locally saved processed response data
response_data_path <- "data/survey_experts/"
#
#
#
# --- load response data ---------------
#
res_comb_upd <- get(load(paste0(response_data_path, "results_combined_upd.rda")))
res_meth_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_methods_recoded.rda")))
res_meth_options <- get(load((paste0(response_data_path, "recoded_processed/", "results_methods_options.rda"))))
res_moni_recoded <- get(load(paste0(response_data_path, "recoded_processed/", "results_monitoring_recoded.rda")))
res_moni_options <- get(load(paste0(response_data_path, "recoded_processed/", "results_monitoring_options.rda")))
#
# check species
assertthat::are_equal(
  res_meth_recoded$species |> unique() |> sort(),
  res_comb_upd$species |> unique() |> sort()
  )
#
#
#
# --- create table: surveillance scopes ---------------
#
table_scope <- res_comb_upd |>
  dplyr::select(tidyselect::all_of(c("species", "stadium"))) |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  #dplyr::mutate(
  #  scope_detection = NA_real_,
  #  scope_inventory = NA_real_,
  #  scope_distribution = NA_real_,
  #  scope_abundance = NA_real_
  #)
  tidyr::crossing(
    scope_type = c(
      "detection",
      "inventory",
      "distribution",
      "abundance",
      "distribution_management",
      "abundance_management"
      )
  ) |>
  dplyr::mutate(
    scope_boolean = NA_real_,
    scope_motivation = NA_character_
    )
#
#
#
# --- create table: monitoring area ---------------
#
#
table_area  <- res_comb_upd |>
  dplyr::filter(grepl("A1|B1", question_id), !grepl("followup", question_text)) |>
  # which monitoring area
  dplyr::mutate(
    area = dplyr::case_when(
      grepl("A1", question_id) ~ "intro",
      grepl("B1", question_id) ~ "dist"
    )
  ) |>
  # monitoring area known?
  dplyr::rename(area_known = "response_text") |>
  tidyr::drop_na(area_known) |>
  tidyr::pivot_wider(
    id_cols = c("species", "stadium", "prius_stadium"),
    names_from = "area",
    values_from = "area_known",
    names_prefix  = "area_"
  )
#
#
#
# --- create table: management ---------------
#
#
table_management  <- res_comb_upd |>
  dplyr::filter(grepl("E", question_id), !grepl("followup", question_text)) |>
  # species managed or not
  tidyr::pivot_wider(
    id_cols = "species",
    names_from = "question_id",
    values_from = "response_text"
  ) |>
  dplyr::rename(
    management_exists = "E1",
    management_evaluation = "E2"
  )
#
#
#
# --- create table: methods ---------------
#
#
# prepare method options data for crossing
res_meth_options <- res_meth_options |>
  dplyr::rename(
    method_all = "response_options",
    method_category = "response_options_cat"
  )
#
# cross species and method options and get methods per species (long format)
table_method_all <- res_meth_recoded |>
  dplyr::filter(!grepl("followup", question_text)) |>
  dplyr::rename(method_tmp = "response_text_final") |>
  tidyr::crossing(res_meth_options) |>
  dplyr::rowwise() |>
  dplyr::filter(grepl(pattern = method_all, x = method_tmp)) |>
  dplyr::ungroup() |>
  dplyr::select(tidyselect::all_of(c("question_id", "species", "method_all", "method_category")))
#
# get best methods per species
table_method_best <- table_method_all |>
  dplyr::filter(
    grepl("D2", question_id)
  ) |>
  dplyr::mutate(method_best = 1) |>
  dplyr::select(tidyselect::all_of(c("species", "method_all", "method_best")))
#
# get additional information reported for best method / per species
table_method_best_info <- res_comb_upd |>
  dplyr::filter(grepl("D3|D4|D5|D6|D7", question_id)) |>
  dplyr::filter(!grepl("followup", question_text)) |>
  dplyr::mutate(
    property = dplyr::case_when(
      grepl("Sensitiviteit", question_text_short) ~ "method_sensitivity",
      grepl("Specificiteit", question_text_short) ~ "method_specificity",
      grepl("Kosten", question_text_short) ~ "method_costs",
      grepl("Scope", question_text_short) ~ "method_scope",
      grepl("Veldprotocol", question_text_short) ~ "method_protocol"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = species,
    names_from = property,
    values_from = response_text
  )
table_method_best_upd <- dplyr::full_join(
  x = table_method_best,
  y = table_method_best_info
)
# missing species:
# Lampropeltis getula (Linnaeus, 1766): "geen" reported as best method
# Marisa cornuarietis (Linnaeus, 1758): mistake during manual recoding, now adapted
#
# add info best method as additional column
table_method <- dplyr::full_join(
  x = table_method_all |>
    dplyr::filter(
      grepl("D1", question_id)
    ),
  y = table_method_best_upd
) |>
  dplyr::select(tidyselect::contains(c("species", "method")))
#
#
#
# --- create table: monitoring ---------------
#
# monitoring
table_moni <- res_moni_recoded |>
  dplyr::filter(!grepl("followup", question_text)) |>
  dplyr::rename(monitoring_tmp = "response_text_final") |>
  tidyr::crossing(monitoring_struc = res_moni_options) |>
  dplyr::rowwise() |>
  dplyr::filter(grepl(pattern = monitoring_struc, x = monitoring_tmp)) |>
  dplyr::ungroup() |>
  dplyr::select(tidyselect::all_of(c("species", "monitoring_struc")))
#
# opportunistic observations
table_obs <- res_comb_upd |>
  dplyr::filter(grepl("D10", question_id), !grepl("followup", question_text)) |>
  dplyr::rename(
    monitoring_opport = "response_text"
  ) |>
  dplyr::select(tidyselect::all_of(c("species", "monitoring_opport")))
#
# join tables
# to be used in future: as additional method with category "monitoring"?
# also extend by scope?
table_monitoring <- dplyr::full_join(
    x = table_moni,
    y = table_obs
  )
#
#
#
# --- create table: survey priority scores ---------------
#
# calculate mean response scores
table_scores <- res_comb_upd |>
  dplyr::filter(question_scored |> as.logical(), !section_skipped) |>
  dplyr::group_by(species) |>
  dplyr::mutate(m_score = mean(response_score, na.rm = TRUE)) |>
  dplyr::filter(dplyr::row_number() == 1) |>
  dplyr::ungroup() |>
  dplyr::select(tidyselect::all_of(c("species", "m_score"))) |>
  dplyr::arrange(dplyr::desc(m_score))
#
# --- create base table skeleton ---------------
#
#
table_base_skeleton <- dplyr::full_join(
  x = table_scope,
  y = table_area
) |> dplyr::full_join(
  x = _,
  y = table_management
) |> dplyr::full_join(
  x = _,
  y = table_obs
) |> dplyr::full_join(
  x = _,
  y = table_scores
) |> dplyr::full_join(
    x = _,
    y = table_method
  )
#
#
# --- define conditions for base table - step 1 ---------------
#
# scope per species based on invasion stadium
expr_stadium_1 <- '
  (grepl("afwezig|sporadisch", stadium) &
     grepl("detection", scope_type)) |
  (grepl("beperkt", stadium) &
     grepl("inventory|distribution|abundance", scope_type)) |
  (grepl("wijd", stadium) &
     grepl("distribution|abundance", scope_type)) |
  (grepl("wijd", stadium) & grepl("detection", scope_type) &
     grepl("BUI", prius_stadium))
   '
#
# scope per species based on invasion stadium (FIX)
expr_area_1 <- '
  (grepl("beperkt", stadium) &
     grepl("verspreiding is voldoende gekend", area_dist) &
     grepl("distribution|abundance", scope_type)) |
  (grepl("beperkt", stadium) &
     !grepl("verspreiding is voldoende gekend", area_dist) &
     grepl("inventory|distribution|abundance", scope_type))
     '
# define specific expr_area_0
#
#
#
# --- define conditions for base table - step 2 ---------------
#
#
#
#
#
# --- define base table ---------------
#
table_base <- table_base_skeleton |>
  dplyr::rowwise() |>
  #
  # ----- step 1 ----------------------------------------
  #
  dplyr::mutate(
    #
    #  scope per species based on invasion stadium
    scope_boolean = dplyr::case_when(
      eval(parse(text = expr_stadium_1)) ~ 1,
      TRUE ~ scope_boolean
      ),
    scope_motivation = dplyr::case_when(
      eval(parse(text = expr_stadium_1)) ~ scope_motivation,
      TRUE ~ paste(scope_motivation, "not invasion stadium", sep = ",") #"scope irrelevant due to invasion stadium", sep = ",")
      ),
    #
    # scope per species based on whether surveillance area is known
    scope_boolean = dplyr::case_when(
      eval(parse(text = expr_area_1)) ~ 1,
      TRUE ~ scope_boolean
    ),
    scope_motivation = dplyr::case_when(
      eval(parse(text = expr_area_1)) ~ scope_motivation,
      TRUE ~ paste(scope_motivation, "not area", sep = ",")#"scope irrelevant due to surveillance area not being known", sep = ",")
    )
    ) |>
  dplyr::ungroup() |>
  #
  ##
  ##
  ##
  ## OLD STUFF
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

