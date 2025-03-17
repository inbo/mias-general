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
  dplyr::select(tidyselect::all_of(
    c("species", "vern_name_nld", "kingdom", "prius_milieu", "stadium", "prius_stadium")
    )) |>
  dplyr::distinct(species, .keep_all = TRUE) |>
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
    scope_boolean_motivation = NA_character_,
    scope_prior = NA_character_,
    scope_prior_motivation = NA_character_
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
# --- define conditions for base table (step 1) ---------------
#
#
# set scope to 1...
#
# ... per species based on invasion stadium (and 0 otherwise)
stadium_scope_1_expr <- '
  (grepl("afwezig|sporadisch", stadium) &
     grepl("detection", scope_type)) |
  (grepl("beperkt", stadium) &
     grepl("inventory|distribution|abundance", scope_type)) |
  (grepl("wijd", stadium) &
     grepl("distribution|abundance", scope_type)) |
  (grepl("wijd", stadium) &
     grepl("detection", scope_type) &
     grepl("BUI", prius_stadium))
     '
stadium_scope_0_motivation <-
  "scope not relevant due to invasion stadium"
#
#
# set scope to 0...
#
#
# ... per species based on whether surveillance area is known
area_scope_0_expr <- '
  (grepl("beperkt", stadium) &
     grepl("verspreiding is voldoende gekend", area_dist) &
     grepl("inventory", scope_type))
     '
area_scope_0_motivation <-
  "scope not relevant as distribution area is known"
#
#
# ... per species based on whether management exists
management_exits_scope_0_expr <- '
  (!grepl("ja", management_exists) &
     grepl("management", scope_type))
     '
management_exits_scope_0_motivation <-
  "scope not relevant as species is not managed"
#
#
# ... per species based on info necessary to evaluate management
management_eval_scope_0_expr <- '
  (grepl("aan- of afwezigheid", management_evaluation) &
     grepl("management", scope_type) &
     grepl("abundance", scope_type)) |
  (grepl("populatiegrootte", management_evaluation) &
     grepl("management", scope_type) &
     grepl("distribution", scope_type)
  )
  '
management_eval_scope_0_motivation <-
  "scope not relevant as information not necessary to evaluate management"
#
#
# ... per method based on whether method is suitable
method_scope_0_expr <- '
  (grepl("aan- of afwezigheid", method_scope) &
     grepl("abundance", scope_type))
     '
method_scope_0_motivation <-
  "scope not relevant as method to measure abundance is not available"
#
#
#
# --- define conditions for filtering base table (step 3) ---------------
#
#
# set scope to low priority
#
#
# ... per species based on monitoring available
# monitoring_scope_lowprior_expr
# monitoring_scope_lowprior_motivation
#
# ... per species for which opportunistic observations are representative
observation_scope_lowprior_expr <- '
  (grepl("^hoge representativiteit", monitoring_opport) &
     scope_boolean == 1)
     '
observation_scope_lowprior_motivation <-
  "scope low priority as opportunistic observations are considered representative"
#
#
# ... per species depending on whether surveillance area is known
area_scope_lowprior_expr <- '
  (((grepl("afwezig", stadium) &
       grepl("detection", scope_type) &
       grepl("ongekend|weet het niet", area_intro)) |
      (grepl("sporadisch", stadium) &
         grepl("detection", scope_type) &
         grepl("ongekend|weet het niet", area_intro) &
         grepl("niet voldoende gekend|weet het niet", area_dist)) |
      (grepl("beperkt", stadium) &
         grepl("distribution|abundance", scope_type) &
         grepl("niet voldoende gekend|weet het niet", area_dist))) &
     scope_boolean == 1)
     '
area_scope_lowprior_motivation <-
  "scope low priority as surveillance area is not known"
#
#
# ... per species depending on survey global priority score
m_score_cutoff <- table_base_skeleton$m_score |> median()
globalscore_scope_lowprior_expr <- '
  (m_score < m_score_cutoff &
     scope_boolean == 1)
     '
globalscore_scope_lowprior_motivation <-
  "scope low priority as global priority score is smaller than cutoff"
#
#
#
# --- define function to create base / filtered table ---------------
#
create_base_filtered_table <- function(
    .table_base_skeleton,
    .stadium_scope_1_expr = stadium_scope_1_expr,
    .area_scope_0_expr = area_scope_0_expr,
    .management_exits_scope_0_expr = management_exits_scope_0_expr,
    .management_eval_scope_0_expr = management_eval_scope_0_expr,
    .method_scope_0_expr = method_scope_0_expr,
    .observation_scope_lowprior_expr = observation_scope_lowprior_expr,
    .area_scope_lowprior_expr = area_scope_lowprior_expr,
    .globalscore_scope_lowprior_expr = globalscore_scope_lowprior_expr,
    .base = TRUE
){
  if (.base){
    .observation_scope_lowprior_expr = "grepl('foo', stadium)"
    .area_scope_lowprior_expr = "grepl('foo', stadium)"
    .globalscore_scope_lowprior_expr = "grepl('foo', stadium)"
  }

  .table_base_skeleton |>
    #
    # ---- base table ---------------------------
  #
  dplyr::rowwise() |>
    dplyr::mutate(
      #
      # set scope to 1 ...
      # ... per species based on invasion stadium
      scope_boolean = dplyr::case_when(
        eval(parse(text = .stadium_scope_1_expr)) ~ 1,
        TRUE ~ 0
      ),
      scope_boolean_motivation = dplyr::case_when(
        eval(parse(text = .stadium_scope_1_expr)) ~ scope_boolean_motivation,
        TRUE ~
          paste(scope_boolean_motivation, stadium_scope_0_motivation, sep = ",")
      ),
      # set scope to 0 ...
      # .. per species based on whether surveillance area is known
      scope_boolean = dplyr::case_when(
        eval(parse(text = .area_scope_0_expr)) ~ 0,
        TRUE ~ scope_boolean
      ),
      scope_boolean_motivation = dplyr::case_when(
        eval(parse(text = .area_scope_0_expr)) ~
          paste(scope_boolean_motivation, area_scope_0_motivation, sep = ","),
        TRUE ~ scope_boolean_motivation
      ),
      # ... per species based on whether management exists
      scope_boolean = dplyr::case_when(
        eval(parse(text = .management_exits_scope_0_expr)) ~ 0,
        TRUE ~ scope_boolean
      ),
      scope_boolean_motivation = dplyr::case_when(
        eval(parse(text = .management_exits_scope_0_expr)) ~
          paste(scope_boolean_motivation, management_exits_scope_0_motivation, sep = ","),
        TRUE ~ scope_boolean_motivation
      ),
      # ... per species based on info necessary to evaluate management
      scope_boolean = dplyr::case_when(
        eval(parse(text = .management_eval_scope_0_expr)) ~ 0,
        TRUE ~ scope_boolean
      ),
      scope_boolean_motivation = dplyr::case_when(
        eval(parse(text = .management_eval_scope_0_expr)) ~
          paste(scope_boolean_motivation, management_eval_scope_0_motivation, sep = ","),
        TRUE ~ scope_boolean_motivation
      ),
      # ... per method based on whether method is suitable
      scope_boolean = dplyr::case_when(
        eval(parse(text = .method_scope_0_expr)) ~ 0,
        TRUE ~ scope_boolean
      ),
      scope_boolean_motivation = dplyr::case_when(
        eval(parse(text = .method_scope_0_expr)) ~
          paste(scope_boolean_motivation, method_scope_0_motivation, sep = ","),
        TRUE ~ scope_boolean_motivation
      )
    ) |>
    dplyr::ungroup() |>
    # clean scope_boolean_motivation
    dplyr::mutate(
      scope_boolean_motivation = gsub("NA,", "", scope_boolean_motivation)
    ) |>
    #
    # ---- filtered table ---------------------------
  #
  dplyr::rowwise() |>
    dplyr::mutate(
      scope_prior = dplyr::case_when(
        scope_boolean == 1 & !.base ~ "highprior",
        TRUE ~ NA_character_
      ),
      #
      # set scope to low prior ...
      # ... per species for which opportunistic observations are representative
      scope_prior = dplyr::case_when(
        eval(parse(text = .observation_scope_lowprior_expr)) ~ "lowprior",
        TRUE ~ scope_prior
      ),
      scope_prior_motivation = dplyr::case_when(
        eval(parse(text = .observation_scope_lowprior_expr)) ~
          paste(scope_prior_motivation, observation_scope_lowprior_motivation, sep = ","),
        TRUE ~ scope_prior_motivation
      ),
      #
      # ... per species depending on whether surveillance area is known
      scope_prior = dplyr::case_when(
        eval(parse(text = .area_scope_lowprior_expr)) ~ "lowprior",
        TRUE ~ scope_prior
      ),
      scope_prior_motivation = dplyr::case_when(
        eval(parse(text = .area_scope_lowprior_expr)) ~
          paste(scope_prior_motivation, area_scope_lowprior_motivation, sep = ","),
        TRUE ~ scope_prior_motivation
      ),
      #
      # ... per species depending on survey global priority score
      scope_prior = dplyr::case_when(
        eval(parse(text = .globalscore_scope_lowprior_expr)) ~ "lowprior",
        TRUE ~ scope_prior
      ),
      scope_prior_motivation = dplyr::case_when(
        eval(parse(text = .globalscore_scope_lowprior_expr)) ~
          paste(scope_prior_motivation, globalscore_scope_lowprior_motivation, sep = ","),
        TRUE ~ scope_prior_motivation
      )
    ) |>
    dplyr::ungroup() |>
    # clean scope_prior_motivation
    dplyr::mutate(
      scope_prior_motivation = gsub("NA,", "", scope_prior_motivation)
    )
}
#
#
#
# --- create base table ---------------

table_base <- create_base_filtered_table(
  .table_base_skeleton = table_base_skeleton
  )
#

#
# --- create filtered table ---------------
#
table_base_filtered <- create_base_filtered_table(
  .table_base_skeleton = table_base_skeleton,
  .base = FALSE
)
#
#
#
# --- illustrate table creation ---------------
#
# base table components
#
table_base_illu_stadium <- table_base_skeleton |>
  dplyr::filter(eval(parse(text = stadium_scope_1_expr))) |>
  dplyr::distinct(stadium)
#
table_base_illu_prius_stadium <- table_base_skeleton |>
  dplyr::filter(grepl("wijd", stadium) & grepl("BUI", prius_stadium)) |>
  dplyr::distinct(stadium, prius_stadium) |>
  tibble::add_row() |>
  tidyr::fill(stadium)
#
table_base_illu_area <- table_base_skeleton |>
  dplyr::filter(eval(parse(text = area_scope_0_expr))) |>
  dplyr::select(c(area_dist, stadium)) |>
  dplyr::distinct(stadium, .keep_all = TRUE) |>
  tibble::add_row() |>
  tidyr::fill(stadium)
#
table_base_illu_management_exists <- table_base_skeleton |>
  # show for limited established species with known distribution only
  dplyr::filter(grepl("beperkt", stadium) &
                  grepl("verspreiding is voldoende gekend", area_dist)) |>
  dplyr::select(c(management_exists, stadium, area_dist)) |>
  # management_exists != "yes": duplicated / 2 options (1 used for m_score)
  dplyr::distinct(management_exists, stadium, .keep_all = TRUE)
#
table_base_illu_management_eval <- table_base_skeleton |>
  dplyr::filter(
    eval(parse(text = management_eval_scope_0_expr)) &
      !eval(parse(text = management_exits_scope_0_expr))
  ) |>
  dplyr::select(c(management_evaluation, management_exists)) |>
  # management_eval == "abundance": duplicated / 2 options
  dplyr::distinct(management_evaluation, .keep_all = TRUE)
#
table_base_illu_method <- table_base_skeleton |>
  dplyr::filter(eval(parse(text = method_scope_0_expr)) &
                  # show for widely spread species only
                  grepl("wijd", stadium)) |>
  dplyr::select(c(method_scope, stadium)) |>
  dplyr::distinct(method_scope, .keep_all = TRUE) |>
  tibble::add_row() |>
  tidyr::fill(stadium) |>
  dplyr::mutate(prius_stadium = NA_character_)
#
#
# filtered table components
#
table_filtered_illu_area <- table_base_skeleton |>
  # manual reduction/adaption of area_scope_lowprior_expr
  dplyr::filter(
    (grepl("afwezig", stadium) &
       grepl("detection", scope_type) &
       # 2nd option replaces NA value due to add_row
       # grepl("wijdverspreid", area_intro): duplicated / 2 options (1 for m_score)
       grepl("ongekend|wijdverspreid", area_intro)) |
      # this combination currently not in data and thus not shown
      (grepl("sporadisch", stadium) &
         grepl("detection", scope_type) &
         grepl("weet het niet", area_intro) &
         grepl("weet het niet", area_dist))
  ) |>
  dplyr::distinct(area_intro, area_dist, stadium)
#
table_filtered_illu_obs <- table_base_skeleton |>
  dplyr::filter(
    # manual reduction/adaption of observation_scope_lowprior_expr
    # 2nd option replaces NA value due to add_row
    grepl("^hoge|lage", monitoring_opport) &
      # show for limited established, not managed species only
      grepl("beperkt", stadium) &
      grepl("neen", management_exists)
  ) |>
  dplyr::select(c(monitoring_opport, stadium, management_exists)) |>
  dplyr::distinct(monitoring_opport, .keep_all = TRUE)
#
# skeleton table
table_base_illu_skeleton <- table_base_illu_stadium |>
  dplyr::full_join(
    x = _,
    y = table_base_illu_prius_stadium
  ) |>
  dplyr::full_join(
    x = _,
    y = table_base_illu_area
  ) |>
  dplyr::full_join(
    x = _,
    y = table_filtered_illu_area
  )|>
  dplyr::full_join(
    x = _,
    y = table_base_illu_management_exists
  ) |>
  dplyr::full_join(
    x = _,
    y = table_base_illu_management_eval
  ) |>
  dplyr::full_join(
    x = _,
    y = table_filtered_illu_obs
  ) |>
  dplyr::left_join(
    x = _,
    y = table_base_illu_method
  )|>
  dplyr::mutate(
    m_score = dplyr::case_when(
      (grepl("weet het niet", management_exists)|
         grepl("zowel specifieke als ook wijdverspreide", area_intro) |
         grepl("sporadisch", stadium) |
         grepl("absolute populatiegrootte", management_evaluation)
      ) ~ 0,
      TRUE ~ 1000
    )
  ) |>
  dplyr::mutate(
    species = paste("species", dplyr::row_number())
  ) |>
  tidyr::crossing(
    method_all = c("method A")
  ) |>
  tidyr::crossing(
    table_scope |> dplyr::distinct(dplyr::pick(tidyselect::starts_with("scope")))
  )
#
#
args_create_base_filtered_table <- list(
  .table_base_skeleton = table_base_illu_skeleton,
  .stadium_scope_1_expr = stadium_scope_1_expr,
  .area_scope_0_expr = "grepl('foo', stadium)",
  .management_exits_scope_0_expr = "grepl('foo', stadium)",
  .management_eval_scope_0_expr = "grepl('foo', stadium)",
  .method_scope_0_expr = "grepl('foo', stadium)",
  .observation_scope_lowprior_expr = "grepl('foo', stadium)",
  .area_scope_lowprior_expr = "grepl('foo', stadium)",
  .globalscore_scope_lowprior_expr = "grepl('foo', stadium)"
)

table_base_illu_1 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

args_create_base_filtered_table$.area_scope_0_expr <- area_scope_0_expr
table_base_illu_2 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
  )

args_create_base_filtered_table$.management_exits_scope_0_expr <- management_exits_scope_0_expr
table_base_illu_3 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

args_create_base_filtered_table$.management_eval_scope_0_expr <- management_eval_scope_0_expr
table_base_illu_4 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

args_create_base_filtered_table$.method_scope_0_expr <- method_scope_0_expr
table_base_illu_5 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

table_base_illu_list <- list(
  stadium = table_base_illu_1,
  area = table_base_illu_2,
  management_exists = table_base_illu_3,
  management_eval = table_base_illu_4,
  method = table_base_illu_5
)
#
args_create_base_filtered_table$.base = FALSE
args_create_base_filtered_table$.observation_scope_lowprior_expr <- observation_scope_lowprior_expr
table_filtered_illu_1 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

args_create_base_filtered_table$.area_scope_lowprior_expr <- area_scope_lowprior_expr
table_filtered_illu_2 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

args_create_base_filtered_table$.globalscore_scope_lowprior_expr <- globalscore_scope_lowprior_expr
table_filtered_illu_3 <- do.call(
  create_base_filtered_table,
  args_create_base_filtered_table
)

table_filtered_illu_list <- list(
  observation = table_filtered_illu_1,
  area = table_filtered_illu_2,
  globalscore = table_filtered_illu_3
)

# --- save results ---------------
#
save(
  table_base_filtered,
  file = paste0(response_data_path, "tables/", "table_base_filtered.rda")
)
save(
  table_base_illu_list,
  file = paste0(response_data_path, "tables/", "table_base_illustration.rda")
)

save(
  table_filtered_illu_list,
  file = paste0(response_data_path, "tables/", "table_filtered_illustration.rda")
)
