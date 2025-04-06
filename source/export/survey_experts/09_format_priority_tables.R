## this script is sourced in: docu/plan_of_action.qmd

# --- load-data-and-functions ---------------

# rm(list = ls())

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
    # add symbols for scope prior motivation
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
#
# --- define symbols ---------------
#
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
#
#
# --- add symbols ---------------
#
table_base_filtered_upd <- add_symbols(table_base_filtered)
table_base_illu_list_upd <- lapply(table_base_illu_list, add_symbols)
table_filtered_illu_list_upd <- lapply(table_filtered_illu_list, add_symbols)
#
#
# --- add-scope-verbose ---------------
#
# full table base
table_base_upd <- table_base_filtered_upd |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 ~ method_all,
      scope_boolean == 0 ~ scope_boolean_symbol
    ),
    .after = scope_boolean
  ) |>
  dplyr::ungroup()

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
# full table filtered (FIX colors later)
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
# illustration table filtered  (FIX colors later)
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
#
# --- highlight-symbols-illustration ---------------
#
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
    magrittr::extract(x = _, seq_along(table_filtered_illu_list_upd)) |>
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
#
#
# --- define background colors scope verbose ---------------
#
color_bg_1 <- "#D4D4FA"
color_bg_2 <- "#FFC5F7"
color_bg_3 <- "#FEFDD1"
#
colors_base_list <- setNames(
  list(color_bg_1, color_bg_1, color_bg_1, color_bg_1, color_bg_2),
  symbols_base_list |> unlist() |> unname()
)
colors_filtered_list <- setNames(
  list(color_hl, color_hl, color_hl, color_hl, color_hl),
  symbols_filtered_list |> unlist() |> unname()
)
#
define_background <- function(
    .string,
    patternlist = colors_base_list
){
  i <- purrr::map(names(patternlist), ~ which(
    stringr::str_starts(
      .string |>
        gsub(pattern = "\\$", replacement = "", x = _ )|>
        gsub(pattern = "\\\\", replacement = "", x = _ ),
      .x |>
        gsub(pattern = "\\$", replacement = "", x = _ )|>
        gsub(pattern = "\\\\", replacement = "", x = _ ),
    ) == 1
  )
  ) |>
    as.integer() |>
    na.omit()
  patternlist[[i]]
}
#
#
#
# --- add colors scope verbose ---------------
#
# FIX coloring not working
if (FALSE) {
  # full table base
  table_base_upd <- table_base_upd |>
    dplyr::rowwise() |>
    # add background color
    # https://stackoverflow.com/questions/76520944/how-to-use-case-when-with-rowwise-for-evaluating-missing-values
    dplyr::mutate(
      scope_verbose_background = ifelse(
        scope_boolean == 1,
        "white",
        define_background(.string = scope_boolean_symbol, patternlist = colors_base_list)
      )
    ) |>
    dplyr::mutate(
      dplyr::case_when(
        scope_boolean == 0 ~ kableExtra::cell_spec(
          x = scope_verbose,
          background = scope_verbose_background
        ),
        scope_boolean == 0 ~ scope_verbose
      )
    )
}
#
#
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

# --- define function to make base or filtered tables for display ---------------

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

# --- create list of cumulative illustrative base tables for display ---------------

table_base_illu_list_display <- mapply(
  \(table, index) {
    table |>
      dplyr::arrange(stadium, species) |>
      make_table_display(
        data_table = _,
        cols_id = c("species", "stadium"),
        cols_addon = NULL,
        footnote_data = footnote_base[1:index,] |>
          # color last row of footnote data
          dplyr::mutate(
            dplyr::across(
              tidyselect::starts_with("scope"),
              \(y) dplyr::case_when(
                dplyr::row_number() == dplyr::n() ~ kableExtra::cell_spec(x = y, color = color_hl),
                TRUE ~ y
              )
            )
          )
      )
  },
  table_base_illu_list_upd,
  table_base_illu_list_upd |> seq_along(),
  SIMPLIFY = FALSE
)

# --- create final illustrative base table for display ---------------


table_base_illu_final_display <- table_base_illu_list_upd |>
  dplyr::last() |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 ~ method_all,
      TRUE ~ scope_verbose
    ),
    # don't highlight symbol
    scope_verbose = gsub(
      color_hl |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      "black" |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      scope_verbose)
  ) |>
  dplyr::arrange(stadium, species) |>
  make_table_display(
    data_table = _,
    cols_id = c("species", "stadium"),
    cols_addon = NULL,
    footnote_data = footnote_base
  )

# --- create list of cumulative illustrative filtered tables for display ---------------

table_filtered_illu_list_display <- mapply(
  \(table, index) {
    table |>
      dplyr::arrange(stadium, species) |>
      make_table_display(
        data_table = _,
        cols_id = c("species", "stadium"),
        cols_addon = NULL,
        footnote_data = footnote_filtered[1:index,] |>
          # color last row of footnote
          dplyr::mutate(
            dplyr::across(
              tidyselect::starts_with("scope"),
              \(y) dplyr::case_when(
                dplyr::row_number() == dplyr::n() ~ kableExtra::cell_spec(x = y, color = color_hl),
                TRUE ~ y
              )
            )
          )
      )
  },
  table_filtered_illu_list_upd,
  table_filtered_illu_list_upd |> seq_along(),
  SIMPLIFY = FALSE
)

# --- create final illustrative filtered table for display ---------------

# HERE symbols score disappeared
table_filtered_illu_final_display <- table_filtered_illu_list_upd |>
  dplyr::last() |>
  dplyr::mutate(
    # don't highlight symbol
    scope_verbose = gsub(
      color_hl |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      "black" |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      scope_verbose)
  ) |>
  dplyr::arrange(stadium, species) |>
  make_table_display(
    data_table = _,
    cols_id = c("species", "stadium"),
    cols_addon = NULL,
    footnote_data = footnote_filtered
  )


# --- create full base table for display ---------------

# sort base table
table_base_upd <- table_base_upd |>
  dplyr::arrange(
    scope_boolean |> dplyr::desc(),
    scope_type,
    prius_milieu,
    stadium,
    taxon,
    m_score_feas |> dplyr::desc()
  )


kingdom_list <- setNames(
  list("plant", "dier"),
  c("plant", "dier")
)
id_cols_display <- c("species", "vern_name_nld", "taxon", "on_unionlist",  "prius_milieu", "stadium")

table_base_upd_display_list <- lapply(
  kingdom_list,
  \(x){
    table_base_upd |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = c("m_score_feas", "m_score_urge"),
        footnote_data = footnote_base
      )
  }
)

# --- create full filtered table for display ---------------

# sort filtered table
table_filtered_upd <- table_filtered_upd |>
  dplyr::arrange(
    scope_prior,
    scope_type,
    prius_milieu,
    stadium,
    taxon,
    m_score_feas |> dplyr::desc()
  )


table_filtered_upd_display_list <- lapply(
  kingdom_list,
  \(x){
    table_filtered_upd |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = c("m_score_feas", "m_score_urge"),
        footnote_data = footnote_combined
      )
  }
)

# --- define functions to prepare and display synergy tables ---------------
#
#
id_cols_syn_display <- c("species", "vern_name_nld", "taxon", "on_unionlist", "stadium", "prius_stadium", "prius_milieu")
#
# make list with data frames containing a (species , milieu) x methods per scope
make_table_syn_list <- function(
    table_filtered,
    cols_addon = c("stadium", "prius_stadium", "taxon", "on_unionlist")
){
  methods <- setNames(
    object = table_filtered$method_all |> unique() |> as.list(),
    nm = table_filtered$method_all |> unique() |>
      gsub("\\:", "", x = _) |> gsub("\\s", "_", x = _)
  )
  #
  # reshape to dataframe with species list per scope type and method
  table_grouped <- lapply(
    methods,
    function(table, method) {
      table |>
        tidyr::pivot_wider(
          id_cols = c(scope_type, prius_milieu),
          names_from = method_all,
          values_from = species
        ) |>
        dplyr::select(
          tidyselect::all_of(c("scope_type", "prius_milieu", method))
        ) |>
        tidyr::unnest(data = _, cols = method) |>
        dplyr::rename(species = method) |>
        dplyr::mutate(
          method = method,
          n_species = dplyr::n(),
          .by = scope_type
        )
    },
    table = table_filtered
  ) |>
    dplyr::bind_rows() |>
    dplyr::left_join(
      x = _,
      y = table_filtered |>
        dplyr::distinct(
          dplyr::across(
            tidyselect::all_of(
              c("species", "vern_name_nld", cols_addon)
            )
          )
        )
    )
  #
  # arrange according to scope_type and n_species in method
  table_grouped <- do.call(
    what = factorize,
    args = args_factorize |> append(list(dataframe = table_grouped))
  ) |>
    dplyr::arrange(
      scope_type,
      n_species |> dplyr::desc()
    )
  #
  # reshape to list containing a (species , milieu) x methods data frame per scope
  scope_type_list <- setNames(
    object = table_grouped$scope_type |> levels() |> as.list(),
    nm = table_grouped$scope_type |> levels()
  )
  table_grouped_list <- lapply(
    scope_type_list,
    function(scope, table){
      table |>
        dplyr::filter(scope_type == scope) |>
        tidyr::pivot_wider(
          id_cols = c("scope_type", "species", "vern_name_nld", "prius_milieu", cols_addon),
          names_from = method,
          names_prefix = "method_",
          values_from = n_species
        )
    },
    table = table_grouped
  )

}
#
# format data frames for display
# add row with number of species displayed per method
make_table_syn_display_prep <- function(
    table,
    cols_id = id_cols_syn_display,
    color = color_hl
) {
  table_prep <- table |>
    dplyr::select(tidyselect::all_of(cols_id), tidyselect::starts_with("method_"))
  # move number of species to added last row
  added_row <- table_prep |>
    tibble::add_row(.before = 1) |>
    tidyr::fill(tidyselect::starts_with("method_"), .direction = "downup") |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("method_"), paste0
      )
    ) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      species = kableExtra::cell_spec(
        x = "Number of species covered",
        italic = TRUE
      )
    )
  table_prep <- table_prep |>
    dplyr::arrange(
      prius_milieu,
      taxon
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("method_"),
        \(x) dplyr::case_when(
          !is.na(x) ~ kableExtra::cell_spec(
            x = " ",
            background = color,
            # fill entire cell (4px )
            extra_css = paste(
              "margin: -4px",
              "padding: 8px",
              "display: flex",
              NULL,
              sep = "; "
            )
          ),
          TRUE ~ NA_character_
        )
      )
    )
  if (nrow(table_prep) > 0){
    table_prep <- table_prep |>
      dplyr::bind_rows(x = _, y = added_row)
  }
}
#
# function to display tables
make_table_syn_display <- function(
    table_prior,
    table_secondary,
    table_tertiary = NULL,
    table_rest = NULL,
    cols_id = id_cols_syn_display
) {
  # combine tables
  table_12 <- if (!is.null(table_secondary)) {
    dplyr::bind_rows(table_prior, table_secondary)
  } else {
    table_prior
  }
  table_123 <- if (!is.null(table_tertiary)) {
    dplyr::bind_rows(table_12, table_tertiary)
  } else {
    table_12
  }
  table_1234 <- if (!is.null(table_rest)) {
    dplyr::bind_rows(table_123, table_rest)
  } else {
    table_123
  }
  table_comb <- table_1234
  #
  #
  # update column names (not rotate the id_cols)
  colnames_upd <- c(
    colnames(table_comb)[seq_along(cols_id)] |> kableExtra::cell_spec(
      x = _,
      extra_css = "writing-mode: horizontal-tb !important;  transform: rotate(180deg) !important;"
    ),
    colnames(table_comb)[(length(cols_id) + 1): ncol(table_comb)] |>
      gsub(pattern = "method_", replacement = "", x = _)
  )
  #
  #
  table_comb |>
    knitr::kable(
      x = _,
      format = "html",
      escape = FALSE,
      col.names = colnames_upd,
      table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
    ) |>
    # background of id cols
    kableExtra::column_spec(
      column = seq_along(cols_id),
      background = "grey97",
      #width_min = "220px",
      extra_css = "white-space: nowrap;"
    ) |>
    # borders of method cols
    #  width
    kableExtra::column_spec(
      column = (length(cols_id) + 1):ncol(table_comb),
      width_min = "20px",
      width_max = "20px",
      include_thead = TRUE,
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
        "method" = ncol(table_comb) - length(cols_id)
      ),
      extra_css = "border-bottom: 1.5px solid"
    ) |>
    # rotate column names pertaining to methods
    kableExtra::row_spec(
      row = 0,
      extra_css =
        "writing-mode: vertical-lr;  transform: rotate(180deg); white-space: nowrap; padding: 5px;"
    ) |>
    # adjust row height
    kableExtra::row_spec(
      row = 1:nrow(table_comb),
      extra_css = 'padding: 4px;'
    ) |>
    # uncolor summary rows
    kableExtra::row_spec(
      row = c(
        nrow(table_prior),
        if(!is.null(table_secondary)) nrow(table_12),
        if(!is.null(table_tertiary)) nrow(table_123),
        nrow(table_comb)
        ),
      background = "white"
    ) |>
    kableExtra::group_rows(
      group_label = "Primary species",
      start_row = 1,
      end_row = nrow(table_prior)
    ) |>
    (\(x)
     if (!is.null(table_secondary)) {
       x |>
         kableExtra::group_rows(
           group_label = "Secondary species",
           start_row = nrow(table_prior) + 1,
           end_row = nrow(table_12)
         )
     } else {
       x
     }
    )() |>
    (\(x)
     if (!is.null(table_tertiary)) {
       x |>
         kableExtra::group_rows(
           group_label = "Tertiary species",
           start_row = nrow(table_12) + 1,
           end_row = nrow(table_123)
         )
     } else {
       x
     }
    )() |>
    (\(x)
     if (!is.null(table_rest)) {
       x |>
         kableExtra::group_rows(
           group_label = "Remaining species",
           start_row = nrow(table_123) + 1,
           end_row = nrow(table_1234)
         )
     } else {
       x
     }
    )()
}
#
# function to make list of tables to display, across scopes
make_table_syn_display_list <- function(
    .table_filtered,
    .kingdom,
    rest = FALSE,
    .remove_n_1 = FALSE
) {
  # high prior species
  table_kingdom_prior_list <- make_table_syn_list(
    table_filtered = .table_filtered |>
      dplyr::filter(grepl(.kingdom, kingdom)) |>
      dplyr::filter(grepl("highprior", scope_prior)),
    remove_n_1 = .remove_n_1
  ) |>
    lapply(
      X = _,
      FUN = make_table_syn_display_prep
    )
  # low prior species with m_urge < cutoff but m_feas > cutoff
  table_kingdom_secondary_list <- try(
    make_table_syn_list(
      table_filtered = .table_filtered |>
        dplyr::filter(grepl(.kingdom, kingdom)) |>
        dplyr::filter(
          grepl("lowprior", scope_prior) &
            scope_prior_motivation == "scope low priority as feasibility score is larger but urgency score is smaller than cutoff"
        ),
      remove_n_1 = .remove_n_1
    ) |>
      lapply(
        X = _,
        FUN = make_table_syn_display_prep,
        color = "gray"
      ),
    silent = TRUE
  )
  # low prior species with m_urge > cutoff but m_feas < cutoff
  table_kingdom_tertiary_list <- try(
    make_table_syn_list(
      table_filtered = .table_filtered |>
        dplyr::filter(grepl(.kingdom, kingdom)) |>
        dplyr::filter(
          grepl("lowprior", scope_prior) &
            scope_prior_motivation == "scope low priority as urgency score is larger but feasibility score is smaller than cutoff"
        ),
      remove_n_1 = .remove_n_1
    ) |>
      lapply(
        X = _,
        FUN = make_table_syn_display_prep,
        color = "gray"
      ),
    silent = TRUE
  )
  # remaining species
  table_kingdom_rest_list <- if (rest) {
    try(
      make_table_syn_list(
        table_filtered = .table_filtered |>
          tidyr::drop_na(method_all) |> # no best method for gewone koningsslang
          dplyr::filter(
            grepl(.kingdom, kingdom) &
              grepl("lowprior", scope_prior) &
              scope_prior_motivation != "scope low priority as urgency score is larger but feasibility score is smaller than cutoff" &
              scope_prior_motivation != "scope low priority as feasibility score is larger but urgency score is smaller than cutoff"
          ),
        remove_n_1 = .remove_n_1
      ) |>
        lapply(
          X = _,
          FUN = make_table_syn_display_prep,
          color = "gray"
        ),
      silent = TRUE
    )
  } else {
    NULL
  }
  #
  tmp <- vctrs::list_drop_empty(table_kingdom_prior_list)
  scope_type_list_kingdom <- setNames(
    object = tmp |> names() |> as.list(),
    nm = tmp |> names()
  )
  table_grouped_kingdom_display_list <- lapply(
    scope_type_list_kingdom,
    \(x) make_table_syn_display(
      table_prior = table_kingdom_prior_list[[x]],
      table_secondary = if (inherits(table_kingdom_secondary_list, "try-error")) {
        NULL
      } else {
        table_kingdom_secondary_list[[x]]
      },
      table_tertiary = if (inherits(table_kingdom_tertiary_list, "try-error")) {
        NULL
      } else {
        table_kingdom_tertiary_list[[x]]
      },
      table_rest = if (inherits(table_kingdom_rest_list, "try-error")) {
        NULL
      } else {
        table_kingdom_rest_list[[x]]
      }
    )
  )
  return(table_grouped_kingdom_display_list)
}

# --- create full synergy tables for display - plants & animals ---------------

table_syn_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_upd,
  .kingdom = "plant",
  rest = TRUE
)
table_syn_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_upd,
  .kingdom = "dier",
  rest = TRUE
)

# --- define-function-make-table-species-display ---------------

make_table_species_display <- function(
    data_table,
    cols_id = c(
      "species",
      "vern_name_nld",
      "kingdom",
      "taxon",
      "on_unionlist",
      "stadium",
      "prius_stadium",
      "prius_milieu"
      ),
    cols_addon = c(
      "m_score_feas",
      "m_score_urge"
    )
){
  data_table_upd <- data_table |>
    dplyr::select(
      tidyselect::all_of(cols_id) |
        tidyselect::all_of(cols_addon)
    )
  #
  knitr::kable(
    x = data_table_upd,
    format = "html",
    escape = FALSE,
    col.names = colnames(data_table_upd) |>
      gsub(pattern = "_", replacement = " ", x = _),
    table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
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
    kableExtra::row_spec(
      row = 1:nrow(data_table_upd),
      # row height
      extra_css = 'padding: 4px;'
    )
}



# --- scenario 1- ANB priorities - get species ---------------

# get species

species_strings_anb <- c("modderkruiper",
                         "eekhoorn",
                         "klauwkikker",
                         "stekelstaart",
                         "ibis",
                         "nerts",
                         "wasbeer$",
                         "muntjak",
                         "stierkikker",
                         "tijgermug",
                         "sikahert",
                         "moerasslak",
                         "prachtslang",
                         "vogelkers",
                         "aziatische hoornaar",
                         "gans",
                         "schildpad",
                         "kreeft",
                         "duizendknoop",
                         "Beverrat",
                         "Muskusrat"
)
species_anb <- table_base_filtered |>
  dplyr::filter(
    grepl(
      paste(species_strings_anb, collapse = "|"),
      vern_name_nld,
      ignore.case = TRUE
    ) | (kingdom == "plant" & grepl("freshwater|marine", prius_milieu))
  ) |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)

species_anb_display <- species_anb |>
  dplyr::arrange(
    kingdom |> dplyr::desc(),
    on_unionlist |> dplyr::desc(),
    stadium,
    prius_milieu,
    taxon,
    m_score_feas |> dplyr::desc()
  ) |>
  make_table_species_display(
    data_table = _
  )


# --- scenario 1 - ANB priorities - filtered tables ---------------
#

table_filtered_anb <- table_filtered_upd |>
   dplyr::filter(grepl(
    paste(species_anb$vern_name_nld, collapse = "|"),
    vern_name_nld
  ))


table_filtered_anb_display_list <- lapply(
  kingdom_list,
  \(x){ table_filtered_anb |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = c("m_score_feas", "m_score_urge"),
        footnote_data = footnote_combined
      )
  }
)
if (FALSE){
  #select scopes: any of
  table_filtered_anb_prior_display_list <- lapply(
    kingdom_list,
    \(x){ table_filtered_anb_prior |>
        dplyr::filter(grepl(x, kingdom)) |>
        make_table_display(
          data_table = _,
          cols_id = id_cols_display,
          cols_addon = c("m_score_feas", "m_score_urge"),
          footnote_data = footnote_combined
        )
    }
  )
}



# --- scenario 1 - ANB priorities - synergy tables - plants & animals ---------------


table_syn_anb_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_anb,
  .kingdom = "plant",
  .remove_n_1 = FALSE
)
table_syn_anb_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_anb,
  .kingdom = "dier",
  .remove_n_1 = FALSE
)


# --- scenario 2 - detection - get species ---------------
#
#
species_det <- table_filtered_upd |>
  dplyr::filter(
    grepl("detection", scope_type) &
      grepl("highprior", scope_prior)
  ) |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)
#
#
# --- scenario 2 - detection - filtered tables ---------------
#
#
table_filtered_det <- table_filtered_upd |>
  dplyr::filter(grepl(
    paste(species_det$vern_name_nld, collapse = "|"),
    vern_name_nld
  ))

table_filtered_det_display_list <- lapply(
  kingdom_list,
  \(x){ table_filtered_det |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = c("m_score_feas", "m_score_urge"),
        footnote_data = footnote_combined
      )
  }
)
#
# --- scenario 2 - detection - synergy tables - plants & animals ---------------
#
table_syn_det_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_det,
  .kingdom = "plant",
  .remove_n_1 = FALSE
)
table_syn_det_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_det,
  .kingdom = "dier",
  .remove_n_1 = FALSE
)
#
# --- scenario 3 - distribution /not detection - get species ---------------
#
species_dist <- table_filtered_upd |>
  dplyr::filter(
    !grepl("detection", scope_type) &
      grepl("highprior", scope_prior)
  ) |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)

# --- scenario 3 - distribution /not detection - filtered tables ---------------
#
table_filtered_dist <- table_filtered_upd |>
  dplyr::filter(grepl(
    paste(species_dist$vern_name_nld, collapse = "|"),
    vern_name_nld
  ))

table_filtered_dist_display_list <- lapply(
  kingdom_list,
  \(x){ table_filtered_dist |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = c("m_score_feas", "m_score_urge"),
        footnote_data = footnote_combined
      )
  }
)

#
# --- scenario 3 - distribution / not detection - synergy tables - plants & plants ---------------
#
table_syn_dist_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_dist,
  .kingdom = "plant"
)
table_syn_dist_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_dist,
  .kingdom = "dier"
)

