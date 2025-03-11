rm(list = ls())

options(knitr.kable.NA = '')

# path to locally saved processed response data
response_data_path <- "data/survey_experts/"

load(paste0(response_data_path, "tables/", "table_base_filtered.rda"))


table_base_filtered_upd <- table_base_filtered |>
  #
  # add sumbols for scope boolean motivation
  dplyr::mutate(
    scope_boolean_symbol = NA_character_,
    .after = scope_boolean_motivation
    ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    scope_boolean_symbol = dplyr::case_when(
      grepl("invasion stadium", scope_boolean_motivation) ~
        paste(scope_boolean_symbol, "\\circ", sep = "\\\\ "),
      TRUE ~ scope_boolean_symbol
    ),
    scope_boolean_symbol = dplyr::case_when(
      grepl("area is known", scope_boolean_motivation) ~
        paste(scope_boolean_symbol, "\\#", sep = "\\\\ "),
      TRUE ~ scope_boolean_symbol
    ),
    scope_boolean_symbol = dplyr::case_when(
      grepl("not managed", scope_boolean_motivation) ~
        paste(scope_boolean_symbol, "\\dagger", sep = "\\\\ "),
      TRUE ~ scope_boolean_symbol
    ),
    scope_boolean_symbol = dplyr::case_when(
      grepl("evaluate management", scope_boolean_motivation) ~
        paste(scope_boolean_symbol, "\\ddagger", sep = "\\\\ "),
      TRUE ~ scope_boolean_symbol
    ),
    scope_boolean_symbol = dplyr::case_when(
      grepl("measure abundance", scope_boolean_motivation) ~
        paste(scope_boolean_symbol, "\\bot", sep = "\\\\ "),
      TRUE ~ scope_boolean_symbol
    )
    )|>
  #
  # add sumbols for scope prior motivation
  dplyr::mutate(
    scope_prior_symbol = NA_character_,
    .after = scope_prior_motivation
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    scope_prior_symbol = dplyr::case_when(
      grepl("opportunistic observations", scope_prior_motivation) ~
        paste(scope_prior_symbol, "\\triangle", sep = "\\\\ "),
      TRUE ~ scope_prior_symbol
    ),
    scope_prior_symbol = dplyr::case_when(
      grepl("area is not known", scope_prior_motivation) ~
        paste(scope_prior_symbol, "\\times", sep = "\\\\ "),
      TRUE ~ scope_prior_symbol
    ),
    scope_prior_symbol = dplyr::case_when(
      grepl("global priority score", scope_prior_motivation) ~
        paste(scope_prior_symbol, "\\ast", sep = "\\\\ "),
      TRUE ~ scope_prior_symbol
    )
  )|>
  #
  # final formatting symbols
  dplyr::mutate(
    scope_boolean_symbol = gsub("NA\\\\\\\\", "", scope_boolean_symbol),
    scope_boolean_symbol = dplyr::case_when(
      !is.na(scope_boolean_symbol) ~ paste0("$", scope_boolean_symbol, "$"),
      TRUE ~ NA_character_
    ),
    scope_prior_symbol = gsub("NA\\\\\\\\", "", scope_prior_symbol),
    scope_prior_symbol = dplyr::case_when(
      !is.na(scope_prior_symbol) ~ paste0("$", scope_prior_symbol, "$"),
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::ungroup()



table_base_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 ~ method_all,
      scope_boolean == 0 ~ scope_boolean_symbol
    ),
    .after = scope_boolean
  )

table_filtered_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 & grepl("highprior", scope_prior) ~ kableExtra::cell_spec(x = method_all, bold = TRUE),
      scope_boolean == 1 & grepl("lowprior", scope_prior) ~ scope_prior_symbol,
      scope_boolean == 0 ~ kableExtra::cell_spec(x = scope_boolean_symbol, color = "lightgrey")
    ),
    .after = scope_prior
  ) |>
  dplyr::select(!tidyselect::starts_with("scope_boolean"))



# before passing data to table function: factorize!

make_table_display <- function(
    data_table,
    cols_id = c(
      "species",
      "vern_name_nld",
      "stadium"
      ),
    cols_addon = c(
      "m_score"
    ),
    cols_sort = c(
      "prius_milieu",
      "stadium",
      "m_score"
    )
){
  data_table |>
    # add scope sorting variable:
    # collapse scope_type per species if scope_symbol == NA
    # consider number of symbols
    tidyr::pivot_wider(
      id_cols = !tidyselect::starts_with("scope_"),
      names_from = scope_type,
      values_from = scope_verbose,
      names_prefix = "scope_"
    ) |>
    dplyr::arrange(
      dplyr::pick(
        tidyselect::all_of(cols_sort)
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(cols_id) |
        tidyselect::all_of(c(
          "scope_detection",
          "scope_inventory",
          "scope_distribution",
          "scope_abundance",
          "scope_distribution_management",
          "scope_abundance_management"
          )) |
        tidyselect::all_of(cols_addon) |
        tidyselect::all_of(cols_sort)
    ) |>
    knitr::kable(
      x = _,
      format = "html",
      escape = FALSE
    ) |>
    kableExtra::column_spec(
      column = seq_along(cols_id),
      background = "#FFFFAF"
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed", "hover"),
      full_width = FALSE,
      position = "left",
      font_size = 11
    ) |>
  kableExtra::collapse_rows(
    columns = seq_along(cols_id),
    valign = "top"
  )
}


make_table_display(
  table_base_upd |>
    dplyr::filter(grepl("plant", kingdom))
  )

make_table_display(
  table_base_upd |>
    dplyr::filter(grepl("dier", kingdom))
)

make_table_display(
  table_filtered_upd |>
    dplyr::filter(grepl("dier", kingdom))
)



















