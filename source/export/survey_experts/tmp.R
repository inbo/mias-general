rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()

options(knitr.kable.NA = '')

# path to locally saved processed response data
response_data_path <- "data/survey_experts/"

load(paste0(response_data_path, "tables/", "table_base_filtered.rda"))

load(paste0(response_data_path, "tables/", "table_base_illustration.rda"))



symbols_base_list <- list(
  stadium = "\\circ",
  area = "\\#",
  management_exists = "\\dagger",
  management_eval = "\\ddagger",
  method = "\\bot"
)

add_symbols <- function(
    .table_base_filtered,
    .symbols_base_list = symbols_base_list
){
  .table_base_filtered |>
    #
    # add symbols for scope boolean motivation
    dplyr::mutate(
      scope_boolean_symbol = NA_character_,
      .after = scope_boolean_motivation
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      scope_boolean_symbol = dplyr::case_when(
        grepl("invasion stadium", scope_boolean_motivation) ~
          paste(scope_boolean_symbol, .symbols_base_list$stadium, sep = ", "),
        TRUE ~ scope_boolean_symbol
      ),
      scope_boolean_symbol = dplyr::case_when(
        grepl("area is known", scope_boolean_motivation) ~
          paste(scope_boolean_symbol, .symbols_base_list$area, sep = ", "),
        TRUE ~ scope_boolean_symbol
      ),
      scope_boolean_symbol = dplyr::case_when(
        grepl("not managed", scope_boolean_motivation) ~
          paste(scope_boolean_symbol, .symbols_base_list$management_exists, sep = ", "),
        TRUE ~ scope_boolean_symbol
      ),
      scope_boolean_symbol = dplyr::case_when(
        grepl("evaluate management", scope_boolean_motivation) ~
          paste(scope_boolean_symbol, .symbols_base_list$management_eval, sep = ", "),
        TRUE ~ scope_boolean_symbol
      ),
      scope_boolean_symbol = dplyr::case_when(
        grepl("measure abundance", scope_boolean_motivation) ~
          paste(scope_boolean_symbol, .symbols_base_list$method, sep = ", "),
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
          paste(scope_prior_symbol, "\\triangle", sep = ", "),
        TRUE ~ scope_prior_symbol
      ),
      scope_prior_symbol = dplyr::case_when(
        grepl("area is not known", scope_prior_motivation) ~
          paste(scope_prior_symbol, "\\times", sep = ", "),
        TRUE ~ scope_prior_symbol
      ),
      scope_prior_symbol = dplyr::case_when(
        grepl("global priority score", scope_prior_motivation) ~
          paste(scope_prior_symbol, "\\ast", sep = ", "),
        TRUE ~ scope_prior_symbol
      )
    )|>
    #
    # final formatting symbols
    dplyr::mutate(
      scope_boolean_symbol = gsub("NA, ", "", scope_boolean_symbol),
      scope_boolean_symbol = dplyr::case_when(
        !is.na(scope_boolean_symbol) ~ paste0("$", scope_boolean_symbol, "$"),
        TRUE ~ NA_character_
      ),
      scope_prior_symbol = gsub("NA, ", "", scope_prior_symbol),
      scope_prior_symbol = dplyr::case_when(
        !is.na(scope_prior_symbol) ~ paste0("$", scope_prior_symbol, "$"),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup()
}


table_base_filtered_upd <- add_symbols(table_base_filtered)
table_base_illu_list_upd <- lapply(table_base_illu_list, add_symbols)


# add scope verbose
#
table_base_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 ~ method_all,
      scope_boolean == 0 ~ scope_boolean_symbol
    ),
    .after = scope_boolean
  )
#
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
#
table_base_illu_list_upd <- lapply(
  table_base_illu_list_upd,
  \(x)
  dplyr::mutate(.data = x,
                scope_verbose = dplyr::case_when(
                  scope_boolean == 1 ~ "",
                  scope_boolean == 0 ~ scope_boolean_symbol
                ),
                .after = scope_boolean
  )
)
#
# highlight symbols
table_base_illu_list_upd <- mapply(
  \(table, symbol) {
    dplyr::mutate(
      .data = table,
      scope_verbose = dplyr::case_when(
        grepl(symbol, scope_verbose) ~ kableExtra::cell_spec(
          x = scope_verbose,
          color = "coral",
          bold = TRUE
        ),
        TRUE ~ scope_verbose
      )
    )
  },
  table_base_illu_list_upd,
  symbols_base_list |>
    purrr::modify_at("management_exists", \(x) "\\\\dagger") |>
    purrr::modify_at("management_eval", \(x) "\\\\ddagger") |>
    purrr::modify_at("method", \(x) "\\\\bot"),
  SIMPLIFY = FALSE
)


# table notes
footnote_base <- table_base_upd |>
  dplyr::filter(!grepl(",", scope_boolean_motivation)) |>
  dplyr::select(c(scope_verbose, scope_boolean_motivation)) |>
  tidyr::drop_na() |>
  dplyr::distinct(scope_boolean_motivation, .keep_all = TRUE) |>
  dplyr::arrange(
    match(
      scope_verbose,
      symbols_base_list |> unlist() |> unname() |> paste0("$", x = _, "$")
    )
  )


#factorize
args_factorize <- list(
  varnames = c("scope_type", "stadium"),
  varlevels = list(c("detection",
                     "inventory",
                     "distribution",
                     "abundance",
                     "distribution_management",
                     "abundance_management") |> rev(),
                   c("afwezig",
                     "beperkt gevestigd",
                     "sporadisch aanwezig",
                     "wijdverspreid")
  )
)
#
table_base_upd <- do.call("factorize", append(args_factorize, list(dataframe =   table_base_upd)))

table_base_illu_list_upd <- lapply(
  table_base_illu_list_upd,
  \(x) do.call("factorize", append(args_factorize, list(dataframe = x)))
)







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
      "scope_type", # helper needed for the 2 scope variables
      "scope_boolean",
      #"prius_milieu",
      "stadium",
      "m_score"
    ),
    footnote_data = footnote_base
){
  data_table_wide <- data_table |>
    # add scope sorting variable:
    # collapse scope_type per species if scope_symbol == NA
    # consider number of symbols
    dplyr::arrange(
      dplyr::pick(
        tidyselect::any_of(cols_sort)
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = !tidyselect::starts_with("scope_"),
      names_from = scope_type,
      values_from = scope_verbose,
      names_prefix = "scope_"
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
        tidyselect::any_of(cols_sort)
    )
  #
  knitr::kable(
    x = data_table_wide,
    format = "html",
    escape = FALSE,
    col.names = colnames(data_table_wide) |>
      gsub(pattern = "scope_", replacement = "", x = _) |>
      gsub(pattern = "_", replacement = " ", x = _) |>
      stringr::str_wrap(width = nchar("distribution"))
  ) |>
    kableExtra::column_spec(
      column = #grep("scope", colnames(data_table_wide)),
        c(seq_along(cols_id), seq_along(cols_addon) + length(cols_id) + 6),
      background = "grey97"
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
    ) |>
    kableExtra::add_header_above(
      header = c(
        " " = length(cols_id),
        "scope" = grepl("scope", colnames(data_table_wide)) |> sum(),
        " " = length(cols_addon)
        ),
      extra_css = "border-bottom: 2px solid;"
    ) |>
    kableExtra::row_spec(
      row = 0 ,
      extra_css = 'vertical-align: top !important;'
      ) |>
    kableExtra::footnote(
      symbol = footnote_data$scope_boolean_motivation,
      symbol_manual = footnote_data$scope_verbose,
      escape = FALSE
    )
}




make_table_display(
  table_base_upd |>
    dplyr::filter(grepl("plant", kingdom)),
  footnote_data = footnote_base
)

make_table_display(
  table_base_upd |>
    dplyr::filter(grepl("dier", kingdom))
)

make_table_display(
  table_filtered_upd |>
    dplyr::filter(grepl("dier", kingdom))
)

table_base_illu_list_display <- mapply(
  \(table, index) make_table_display(
    data_table = table,
    cols_id = c("species", "stadium"),
    cols_addon = NULL,
    cols_sort = c("stadium"),
    footnote_data = footnote_base[1:index,]
  ),
  table_base_illu_list_upd,
  table_base_illu_list_upd |> seq_along(),
  SIMPLIFY = FALSE
)

#
table_base_illu_list_display$stadium
table_base_illu_list_display$area
table_base_illu_list_display$management_exists
table_base_illu_list_display$management_eval
table_base_illu_list_display$method


table_base_illu_list_display$stadium |>
  kableExtra::add_header_above(
    header = c(" " = 4, "bla" = 3)
  )




