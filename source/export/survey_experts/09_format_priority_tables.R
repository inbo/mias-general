
# --- load-data-and-functions ---------------

options(knitr.kable.NA = '')
options(knitr.table.format = "html")

if (!exists("functions_path")) {
  functions_path <- "source/functions/"
}
list.files(functions_path, full.names = TRUE) |>
  lapply(source) |>
  invisible()

if (!exists("response_data_path")) {
  response_data_path <- "data/survey_experts/"
}
load(paste0(response_data_path, "tables/", "table_base_filtered.rda"))
load(paste0(response_data_path, "tables/", "table_base_illustration.rda"))
load(paste0(response_data_path, "tables/", "table_filtered_illustration.rda"))


# --- define-colors ---------------

color_hl = "#FA8775"
color_meth_a = "#0000FF"
color_meth_b = "#FFB14E" # "#CD34B5"

#
#
# --- round priority scores to 2 digits ---------------
#
table_base_filtered <- table_base_filtered |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(c("m_score", "m_score_feas", "m_score_urge")),
      \(x) round(x, digits = 2)
    )
  )
#
#
# --- define-function-add-symbols ---------------

add_symbols <- function(
    .table_base_filtered,
    .symbols_base_list = symbols_base_list,
    .symbols_filtered_list = symbols_filtered_list
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
          paste(scope_prior_symbol, .symbols_filtered_list$observation, sep = ", "),
        TRUE ~ scope_prior_symbol
      ),
      scope_prior_symbol = dplyr::case_when(
        grepl("area is not known", scope_prior_motivation) ~
          paste(scope_prior_symbol, .symbols_filtered_list$area, sep = ", "),
        TRUE ~ scope_prior_symbol
      ),
      scope_prior_symbol = dplyr::case_when(
        grepl("feasibility and urgency scores are smaller", scope_prior_motivation) ~
          paste(scope_prior_symbol, .symbols_filtered_list$score_a, sep = ", "),
        grepl("urgency score is larger but feasibility score is smaller", scope_prior_motivation) ~
          paste(scope_prior_symbol, .symbols_filtered_list$score_b, sep = ", "),
        grepl("feasibility score is larger but urgency score is smaller", scope_prior_motivation) ~
          paste(scope_prior_symbol, .symbols_filtered_list$score_c, sep = ", "),
        TRUE ~ scope_prior_symbol
      )
    )|>
    #
    # final formatting symbols
    dplyr::mutate(
      scope_boolean_symbol = gsub("NA, ", "", scope_boolean_symbol),
      scope_prior_symbol = gsub("NA, ", "", scope_prior_symbol)#,
    ) |>
    dplyr::ungroup()
}

#

# --- add-symbols ---------------

symbols_base_list <- list(
  stadium = "$\\circ$",
  area = "$\\#$",
  management_exists = "$\\dagger$",
  management_eval = "$\\ddagger$",
  method = "$\\bot$"
)
symbols_filtered_list <- list(
  observation = "$\\triangle$",
  area = "$\\times$",
  score_a = "$\\ast\\ast$",
  score_b = "$u\\ast$",
  score_c = "$f\\ast$"
)
table_base_filtered_upd <- add_symbols(table_base_filtered)
table_base_illu_list_upd <- lapply(table_base_illu_list, add_symbols)
table_filtered_illu_list_upd <- lapply(table_filtered_illu_list, add_symbols)

#

# --- add-scope-verbose ---------------

# full table base
table_base_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 ~ method_all,
      scope_boolean == 0 ~ scope_boolean_symbol
    ),
    .after = scope_boolean
  )
#
# illustration table base
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
# full table filtered
table_filtered_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 & grepl("highprior", scope_prior) ~
        kableExtra::cell_spec(x = method_all, bold = TRUE),
      scope_boolean == 1 & grepl("lowprior", scope_prior) ~
        paste(scope_prior_symbol, kableExtra::cell_spec(x = method_all, color = "lightgrey")),
      scope_boolean == 0 ~
        kableExtra::cell_spec(x = scope_boolean_symbol, color = "lightgrey")
    ),
    .after = scope_prior
  ) |>
  dplyr::select(!tidyselect::starts_with("scope_boolean"))
#
# illustration table filtered
table_filtered_illu_list_upd <- lapply(
  table_filtered_illu_list_upd,
  \(x) dplyr::mutate(
    .data = x,
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 & grepl("highprior", scope_prior) ~
        kableExtra::cell_spec(x = method_all, bold = TRUE),
      scope_boolean == 1 & grepl("lowprior", scope_prior) ~
        # setup so that later highlighting does not break
        paste0(kableExtra::cell_spec(x = method_all, color = "lightgrey"), ", ", scope_prior_symbol ),
      #scope_prior_symbol,
      scope_boolean == 0 ~
        kableExtra::cell_spec(x = scope_boolean_symbol, color = "lightgrey")
    ),
    .after = scope_prior
  )
)

#

# --- highlight-symbols-illustration ---------------

tmp_fun <- function(table, symbol){
  dplyr::mutate(
    .data = table,
    scope_verbose = dplyr::case_when(
      grepl(symbol, scope_verbose) ~ paste0(
        # remove text after last comma
        sub("([^,]*)$"," ", scope_verbose),
        kableExtra::cell_spec(
          # remove text before and including last comma
          x = sub('.*\\,', "", scope_verbose),
          color = color_hl,
          bold = TRUE
        )
      ),
      TRUE ~ scope_verbose
    )
  )

}
table_base_illu_list_upd <- mapply(
  tmp_fun,
  table_base_illu_list_upd,
  # prep for grepl
  symbols_base_list |>
    lapply(X = _, FUN = \(x)
           gsub(pattern = "\\$", replacement = "", x = x) |>
             gsub(pattern = "\\\\d", replacement = "\\\\\\\\d", x = _) |>
             gsub(pattern = "\\\\b", replacement = "\\\\\\\\b", x = _)
    ),
  SIMPLIFY = FALSE
)
table_filtered_illu_list_upd <- mapply(
  tmp_fun,
  table_filtered_illu_list_upd,
  # prep for grepl
  symbols_filtered_list |>
    lapply(X = _, FUN = \(x)
           gsub(pattern = "\\$", replacement = "", x = x) |>
             gsub(pattern = "\\\\t", replacement = "\\\\\\\\t", x = _) |>
             gsub(pattern = "\\\\a", replacement = "\\\\\\\\a", x = _)
    ),
  SIMPLIFY = FALSE
)


#

# --- define-table-footnotes ---------------

footnote_base <- table_base_upd |>
  dplyr::filter(!grepl(",", scope_boolean_motivation)) |>
  dplyr::select(c(scope_verbose, scope_motivation = scope_boolean_motivation)) |>
  tidyr::drop_na() |>
  dplyr::distinct(scope_motivation, .keep_all = TRUE) |>
  dplyr::arrange(
    match(
      scope_verbose,
      symbols_base_list |> unlist() |> unname()
    )
  )
footnote_filtered <- table_filtered_upd |>
  dplyr::filter(!grepl(",", scope_prior_motivation)) |>
  dplyr::select(c(scope_verbose = scope_prior_symbol, scope_motivation = scope_prior_motivation)) |>
  tidyr::drop_na() |>
  dplyr::distinct(scope_motivation, .keep_all = TRUE) |>
  dplyr::arrange(
    match(
      scope_verbose,
      symbols_filtered_list |> unlist() |> unname()
    )
  )
footnote_combined <- dplyr::bind_rows(footnote_base, footnote_filtered)
#

# --- factorize-data-for-display ---------------

args_factorize <- list(
  varnames = c("scope_type", "stadium", "prius_milieu", "taxon"),
  varlevels = list(c("detection",
                     "inventory",
                     "distribution",
                     "abundance",
                     "distribution_management",
                     "abundance_management"),
                   c("afwezig",
                     "sporadisch aanwezig",
                     "beperkt gevestigd",
                     "wijdverspreid"),
                   c(
                     "freshwater",
                     "freshwater, brackishwater",
                     "freshwater, brackishwater, marine",
                     "freshwater, terrestrial",
                     "terrestrial",
                     "terrestrial, brackishwater",
                     "terrestrial, freshwater, brackishwater",
                     "marine",
                     "marine, brackisch, freshwater",
                     "brackishwater, marine"
                   ),
                   c(
                     "plant",
                     "bruinwier",
                     "kreeftachtige",
                     "vis",
                     "stekelhuidige",
                     "zoogdier",
                     "vogel",
                     "reptiel",
                     "amfibie",
                     "weekdier",
                     "platworm",
                     "insect"
                   )
  )
)
#
table_base_upd <- do.call("factorize", append(args_factorize, list(dataframe =   table_base_upd)))
table_filtered_upd <- do.call("factorize", append(args_factorize, list(dataframe =   table_filtered_upd)))
#
table_base_illu_list_upd <- lapply(
  table_base_illu_list_upd,
  \(x) do.call("factorize", append(args_factorize, list(dataframe = x)))
)
table_filtered_illu_list_upd <- lapply(
  table_filtered_illu_list_upd,
  \(x) do.call("factorize", append(args_factorize, list(dataframe = x)))
)

#

# --- define-function-make-table-display ---------------

make_table_display <- function(
    data_table,
    cols_id = c(
      "species",
      "vern_name_nld",
      "stadium"
    ),
    cols_addon = c(
      "m_score_feas",
      "m_score_urge"
    ),
    footnote_data = footnote_base
){
  data_table_wide <- data_table |>
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
        tidyselect::all_of(cols_addon)
    )
  #
  knitr::kable(
    x = data_table_wide,
    format = "html",
    escape = FALSE,
    col.names = colnames(data_table_wide) |>
      gsub(pattern = "scope_", replacement = "", x = _) |>
      gsub(pattern = "_", replacement = " ", x = _) |>
      stringr::str_wrap(width = nchar("abundance")), #|>
    #gsub(pattern = "\\n", replacement = "\\\\n", x = _),
    table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
  ) |>
    # background of id and addon cols
    kableExtra::column_spec(
      column =
        c(seq_along(cols_id), seq_along(cols_addon) + length(cols_id) + 6),
      background = "grey97"
    ) |>
    # borders of scope cols
    kableExtra::column_spec(
      column = c(length(cols_id) + 1:6),
      border_right = "2px solid #f7f7f7"
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed", "hover"), # "responsive"
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
      extra_css = "border-bottom: 1.5px solid"
    ) |>
    kableExtra::row_spec(
      row = 0 ,
      # align headers top
      extra_css = 'vertical-align: top !important; padding: 8px;'
    ) |>
    kableExtra::row_spec(
      row = 1:nrow(data_table_wide),
      # row height
      extra_css = 'padding: 4px;'
    ) |>
    kableExtra::footnote(
      symbol = footnote_data$scope_motivation,
      symbol_manual = footnote_data$scope_verbose,
      escape = FALSE
    )
}

#
