## this script is sourced in: docu/plan_of_action.qmd

# --- load-data-and-functions ---------------

# rm(list = ls())

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

# --- define table formatting ---------------

if (!exists("is_html")){
  is_html <- TRUE
}
.format <- ifelse(is_html, "html", "latex")
.escape <- FALSE
.bootstrap_options <- if (is_html) {c("condensed", "hover") } else {"basic"}
.latex_options <- if (is_html) {"basic"} else {c("repeat_header", "striped")}
.booktabs <- if (is_html) {FALSE} else {TRUE}
.longtable <- if (is_html) {FALSE} else {TRUE}
.font_size <- if (exists("mode_source") && grepl("presentation", mode_source)) {
  15
} else if (is_html) {
  12
} else {
  6
}
.col_id_species_width <- ifelse(is_html, "75px", "50pt")
.col_id_width <- ifelse(is_html, "75px", "30pt")
.col_scope_width <- ifelse(is_html, "60px", "35pt")
.col_addon_width <- ifelse(is_html, "50px", "20pt")
.col_method_width <- ifelse(is_html, "20px", "2pt")


options(knitr.kable.NA = '')
options(knitr.table.format = .format)




# colors also different?
# check tables here and in overleaf

# --- define-colors ---------------

color_hl = if (exists("mode_source") && grepl("presentation", mode_source)) {
  "#FFB14E"
} else {
  INBOtheme::inbo_hoofd #"#FA8775"
}
color_meth_a = "#0000FF"
color_meth_b = "#FFB14E" # "#CD34B5"

color_verylightgrey <-"#FAFAFA" #"#F8F8F8"
color_lightgrey <-"#D3D3D3"
color_grey <-"#A9A9A9"
color_black <- "#000000"
#
#
#
# --- escape special characters in scientific names  ---------------

if (!is_html) {
  table_base_filtered <- table_base_filtered |>
    dplyr::mutate(
      species = species |>  gsub("&", "\\\\&", x = _)
    )
}

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

# full table filtered
table_filtered_upd <- table_base_filtered_upd |>
  dplyr::group_by(species, scope_type) |>
  dplyr::mutate(
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 & grepl("highprior", scope_prior) ~
        kableExtra::cell_spec(
          format = .format,
          # paste methods so that they end up in one table cell
          x = method_all |> paste0(x = _, collapse = ", "),
          bold = TRUE,
          escape = .escape
          ),
      scope_boolean == 1 & grepl("lowprior", scope_prior) ~
        paste(scope_prior_symbol,
              kableExtra::cell_spec(
                format = .format,
                # paste methods so that they end up in one table cell
                x = method_all |> paste0(x = _, collapse = ", ") |> paste0(", ", x = _),
                color = color_grey,
                escape = .escape)
              ),
      scope_boolean == 0 ~
        kableExtra::cell_spec(
          format = .format,
          x = scope_boolean_symbol,
          color = color_grey,
          escape = .escape
          )
    ),
    .after = scope_prior
  ) |>
  dplyr::ungroup() |>
  dplyr::select(!tidyselect::starts_with("scope_boolean"))
#
# illustration table filtered  (FIX colors later)
table_filtered_illu_list_upd <- lapply(
  table_filtered_illu_list_upd,
  \(x) dplyr::mutate(
    .data = x,
    scope_verbose = dplyr::case_when(
      scope_boolean == 1 & grepl("highprior", scope_prior) ~
        kableExtra::cell_spec(format = .format, x = method_all, bold = TRUE, escape = .escape),
      scope_boolean == 1 & grepl("lowprior", scope_prior) ~
        # setup so that later highlighting does not break
        # paste0(kableExtra::cell_spec(format = .format, x = method_all, color = color_grey, escape = .escape), ", ", scope_prior_symbol ),
        paste0(
          scope_prior_symbol,
          kableExtra::cell_spec(
            format = .format,
            x = method_all |> paste0(", ", x = _),
            color = color_grey,
            escape = .escape
            )
          ),
      #scope_prior_symbol,
      scope_boolean == 0 ~
        kableExtra::cell_spec(format = .format, x = scope_boolean_symbol, color = color_grey, escape = .escape)
    ),
    .after = scope_prior
  )
)
#
#
# --- highlight-symbols-illustration ---------------

if (FALSE) {

tmp_fun <- function(table, symbol){
  dplyr::mutate(
    .data = table,
    scope_verbose = dplyr::case_when(
      grepl(symbol, scope_verbose) ~ paste0(
        # remove text after last comma
        sub("([^,]*)$"," ", scope_verbose),
        kableExtra::cell_spec(
          format = .format,
          # remove text before and including last comma
          x = sub('.*\\,', "", scope_verbose),
          color = color_hl,
          bold = TRUE,
          escape = .escape
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

}
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
  ) |>
  # quadrupel \ needed for .format = latex
  # https://github.com/haozhu233/kableExtra/issues/120
  (\(x)
   if (!is_html) {
     x |>
       dplyr::mutate(
         scope_verbose = scope_verbose |> gsub(pattern = "\\\\", replacement = "\\\\\\\\", x = _)
       )
   } else {
     x
   }
  )()
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
  ) |>
  (\(x)
   if (!is_html) {
     x |>
       dplyr::mutate(
         scope_verbose = scope_verbose |> gsub(pattern = "\\\\", replacement = "\\\\\\\\", x = _)
       )
   } else {
     x
   }
  )()
footnote_combined <- dplyr::bind_rows(footnote_base, footnote_filtered)
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
                   c("absent",
                     "sporadically present",
                     "established to limited extend",
                     "widespread"),
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
                     "brownweed",
                     "crustacean",
                     "fish",
                     "echinoderm",
                     "mammal",
                     "bird",
                     "reptile",
                     "amphibian",
                     "mollusk",
                     "flatworm",
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

# --- define-function-make-table-species-display ---------------

make_table_species_display <- function(
    data_table,
    cols_id = c(
      "species",
      "vern_name_eng",
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
    ),
    cols_linebreak = c(
      "species",
      "vern_name_eng",
      "stadium",
      "prius_stadium",
      "prius_milieu"
    )
){
  data_table_upd <- data_table |>
    dplyr::select(
      tidyselect::all_of(cols_id) |
        tidyselect::all_of(cols_addon)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains(cols_linebreak),
        \(x){
          y <- stringr::str_wrap(string = x, width = 25)
          z <- if (!is_html) {
            kableExtra::linebreak(y, double_escape = FALSE)
            #gsub(pattern = "\\n", replacement = "\\\\\\\\", y)
            } else{
              y
            }
          return(z)
        }
        )
      )
  knitr::kable(
    x = data_table_upd,
    format = .format,
    escape = .escape,
    booktabs = .booktabs,
    longtable = .longtable,
    linesep = "",
    col.names = colnames(data_table_upd) |>
      gsub(pattern = "_", replacement = " ", x = _),
    table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = .bootstrap_options,
      latex_options = .latex_options,
      full_width = FALSE,
      position = "left",
      font_size = .font_size
    ) |>
    #kableExtra::collapse_rows(
    #  columns = seq_along(cols_id),
    #  valign = "top"
    #) |>
    # row height
    kableExtra::row_spec(
      row = 1:nrow(data_table_upd),
      extra_css = if (is_html) 'padding: 4px;' else NULL
    ) #|>
    # alternating colors
    #kableExtra::row_spec(
    #  row = seq(1, nrow(data_table_upd), 2),
    #  background = color_verylightgrey
    #)
}



# --- define function to make base or filtered tables for display ---------------

make_table_display <- function(
    data_table,
    cols_id = c(
      "species",
      "vern_name_eng",
      "stadium"
    ),
    cols_addon = c("m_score_feas", "m_score_urge"),
    footnote_data = footnote_base
){
  data_table_wide <- data_table |>
    dplyr::distinct(species, scope_type, .keep_all = TRUE) |>
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
  .header <- c(
    length(cols_id),
    grepl("scope", colnames(data_table_wide)) |> sum(),
    if (length(cols_addon) > 0) length(cols_addon) else NULL
    )
  names(.header) <- c(" ", "scope", if (length(cols_addon) > 0) " " else NULL)
  knitr::kable(
    x = data_table_wide,
    format = .format,
    escape = .escape,
    booktabs = .booktabs,
    longtable = .longtable,
    linesep = "",
    col.names = colnames(data_table_wide) |>
      gsub(pattern = "scope_", replacement = "", x = _) |>
      gsub(pattern = "_", replacement = " ", x = _) |>
      stringr::str_wrap(width = nchar("abundance")), #|>
      #(\(x)
      # if (!is_html) {
      #   gsub(pattern = "\\n", replacement = "\\\\n", x)
      # } else {
      #   x
      # }
      #)(),
    table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = .bootstrap_options,
      latex_options = .latex_options,
      full_width = FALSE,
      position = "left",
      font_size = .font_size
    ) |>
    # width of id and addon cols
    kableExtra::column_spec(
      column = 1,
      width = .col_id_species_width
    ) |>
    kableExtra::column_spec(
      column = seq_along(cols_id)[-1],
      width = .col_id_width
    ) |>
    kableExtra::column_spec(
      column = length(cols_id) + 6 + seq_along(cols_addon),
      width = .col_addon_width
    ) |>
    # borders & width of scope cols
    kableExtra::column_spec(
      column = c(length(cols_id) + 0:6),
      border_right = ifelse(is_html, "2px solid grey97", TRUE),
      width = .col_scope_width
    ) |>
    #kableExtra::collapse_rows(
    #  columns = seq_along(cols_id),
    #  valign = "top"
    #) |>
    kableExtra::add_header_above(
      header = .header,
      extra_css = if (is_html) "border-bottom: 1.5px solid" else NULL
    ) |>
    kableExtra::row_spec(
      row = 0 ,
      # align headers top
      extra_css = if (is_html) 'vertical-align: top !important; padding: 8px;' else NULL
    ) |>
    kableExtra::row_spec(
      row = 1:nrow(data_table_wide),
      hline_after = FALSE,
      # row height
      extra_css = if (is_html) 'padding: 4px;' else NULL
    ) |>
    kableExtra::row_spec(
      row = nrow(data_table_wide),
      extra_css = if (is_html) "border-bottom: 1.5px solid" else NULL
    ) |>
    kableExtra::footnote(
      symbol = footnote_data$scope_motivation,
      symbol_manual = footnote_data$scope_verbose,
      escape = .escape
    )
}

# --- define functions to prepare and display synergy tables ---------------
#
#
id_cols_syn_display <- if (exists("mode_source") && grepl("presentation", mode_source)) {
  c("vern_name_eng", "taxon", "on_unionlist", "stadium", "prius_stadium", "prius_milieu")
} else {
  c("species", "vern_name_eng", "stadium", "taxon", "prius_milieu")
  # "taxon", "on_unionlist",
}
#
# make list with data frames containing a (species , milieu) x methods per scope
make_table_syn_list <- function(
    table_filtered,
    cols_addon = c("stadium", "prius_stadium", "taxon", "on_unionlist"),
    remove_n_1 = FALSE
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
        # needed in case cells are not lists (no method-overlap)
        # to parallel keep_empty = FALSE in unnest
        tidyr::drop_na(species) |>
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
              c("species", "vern_name_eng", cols_addon)
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
  # remove species if there is no method overap
  if (remove_n_1){
    table_grouped <- table_grouped |>
      dplyr::filter(
        n_species > 1
      )
  }
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
          id_cols = c("scope_type", "species", "vern_name_eng", "prius_milieu", cols_addon),
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
    color = color_hl,
    add_species_number = FALSE
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
      dplyr::across(1,
                    \(x){
                      x = kableExtra::cell_spec(
                        format = .format,
                        x = "Number of species covered",
                        italic = TRUE,
                        escape = .escape
                      )
                    }
      )
    )
  table_prep <- table_prep |>
    dplyr::arrange(prius_milieu) |> # taxon
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("method_"),
        \(x) dplyr::case_when(
          !is.na(x) ~ kableExtra::cell_spec(
            format = .format,
            x = " ",
            background = color,
            # fill entire cell (4px )
            extra_css = if (is_html) {
              paste(
                "margin: -4px",
                "padding: 8px",
                "display: flex",
                NULL,
                sep = "; "
              )
            } else {
              NULL
            },
            escape = .escape
          ),
          TRUE ~ NA_character_
        )
      )
    )
  if (nrow(table_prep) > 0 & add_species_number){
    table_prep <- table_prep |>
      dplyr::bind_rows(x = _, y = added_row)
  }
  if (nrow(table_prep) == 0){
    table_prep <- NULL
  }
  return(table_prep)
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
    dplyr::bind_rows(
      table_prior,
      table_secondary |>
        dplyr::select(tidyselect::any_of(colnames(table_prior)))
    )
  } else {
    table_prior
  }
  table_123 <- if (!is.null(table_tertiary)) {
    dplyr::bind_rows(
      table_12,
      table_tertiary |>
        dplyr::select(tidyselect::any_of(colnames(table_prior)))
    )
  } else {
    table_12
  }
  table_1234 <- if (!is.null(table_rest)) {
    dplyr::bind_rows(
      table_123,
      table_rest |>
        dplyr::select(tidyselect::any_of(colnames(table_prior)))
    )
  } else {
    table_123
  }
  table_comb <- table_1234
  #
  #
  # update column names
  # html: not rotate the id_cols, rest is rotated later
  colnames_upd <- c(
    colnames(table_comb)[seq_along(cols_id)] |>
      gsub(pattern = "_", replacement = " ", x = _) |>
      kableExtra::cell_spec(
        format = .format,
        x = _,
        extra_css = if (is_html) {
          "writing-mode: horizontal-tb !important;  transform: rotate(180deg) !important;"
        } else {
          NULL
        },
        escape = .escape
      ),
    colnames(table_comb)[(length(cols_id) + 1): ncol(table_comb)] |>
      gsub(pattern = "method_", replacement = "", x = _) |>
      # if latex: rotate
      (\(x)
       if (!is_html) {
         paste0("\\rotatebox{90}{", x, "}")
       } else {
         x
       }
      )()
  )
  #
  #
  table_comb |>
    knitr::kable(
      x = _,
      format = .format,
      escape = .escape,
      booktabs = .booktabs,
      longtable = .longtable,
      linesep = "",
      col.names = colnames_upd,
      table.attr = 'data-quarto-disable-processing="true"' # if quarto HERE
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = .bootstrap_options,
      latex_options = .latex_options |>
        grep("striped", x = _, invert = TRUE, value = TRUE),
      full_width = FALSE,
      position = "left",
      font_size = .font_size
    ) |>
    # width of id cols
    kableExtra::column_spec(
      column = 1,
      width = .col_id_species_width,
      extra_css = if (is_html) "white-space: nowrap;" else NULL
    ) |>
    kableExtra::column_spec(
      column = seq_along(cols_id)[-1],
      width = .col_id_width,
      extra_css = if (is_html) "white-space: nowrap;" else NULL
    ) |>
    # borders of method cols
    kableExtra::column_spec(
      column = length(cols_id):ncol(table_comb),
      border_right = ifelse(is_html, "2px solid grey97", TRUE)
    ) |>
    #  width of method cols
    kableExtra::column_spec(
      column = (length(cols_id) + 1):ncol(table_comb),
      width = if (is_html) NULL else .col_method_width,
      width_min = if (is_html) .col_method_width else NULL,
      width_max = if (is_html) .col_method_width else NULL,
      include_thead = ifelse(is_html, TRUE, FALSE),
      border_right = ifelse(is_html, "2px solid grey97", TRUE)
    ) |>
    #kableExtra::collapse_rows(
    #  columns = seq_along(cols_id),
    #  valign = "top"
    #) |>
    kableExtra::add_header_above(
      header = c(
        " " = length(cols_id),
        "method" = ncol(table_comb) - length(cols_id)
      ),
      extra_css = if (is_html) "border-bottom: 1.5px solid" else NULL
    ) |>
    # if html rotate column names pertaining to methods
    kableExtra::row_spec(
      row = 0,
      extra_css = if (is_html){
        "writing-mode: vertical-lr;  transform: rotate(180deg); white-space: nowrap; padding: 5px;"
      } else {
        NULL
      }
    ) |>
    # adjust row height & add hline after
    kableExtra::row_spec(
      row = 1:nrow(table_comb),
      extra_css = if (is_html) 'padding: 4px;' else NULL,
      hline_after = ifelse(is_html, FALSE, TRUE)
    ) |>
    kableExtra::group_rows(
      group_label = "Primary species",
      start_row = 1,
      end_row = nrow(table_prior),
      background = "white",
      hline_after = TRUE,
      indent = FALSE
    ) |>
    (\(x)
     if (!is.null(table_secondary)) {
       x |>
         kableExtra::group_rows(
           group_label = "Secondary species",
           start_row = nrow(table_prior) + 1,
           end_row = nrow(table_12),
           background = "white",
           hline_after = TRUE,
           indent = FALSE
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
           end_row = nrow(table_123),
           background = "white",
           hline_after = TRUE,
           indent = FALSE
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
           end_row = nrow(table_1234),
           background = "white",
           hline_after = TRUE,
           indent = FALSE
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
        table_kingdom_secondary_list[[x]] ## these NULL vs. dataframe with 0 rows depending on whether added_row is added?
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

# --- illustration - create list of cumulative illustrative base tables for display ---------------


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
                dplyr::row_number() == dplyr::n() ~ kableExtra::cell_spec(format = .format, x = y, color = color_hl, escape = .escape),
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

# --- illustration - create final illustrative base table for display ---------------


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

# --- illustration - create list of cumulative illustrative filtered tables for display ---------------

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
                dplyr::row_number() == dplyr::n() ~ kableExtra::cell_spec(format = .format, x = y, color = color_hl, escape = .escape),
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

# --- illustration - create final illustrative filtered table for display ---------------


table_filtered_illu_final_display <- table_filtered_illu_list_upd |>
  dplyr::last() |>
  dplyr::mutate(
    # don't highlight symbol
    scope_verbose = scope_verbose |> gsub(
      # html
      color_hl |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      "black" |> grDevices::col2rgb() |> as.vector() |> append(255) |>
        paste(x = _, collapse = ", "),
      x = _
      ) |>
      # latex
      gsub(
        color_hl |> gsub("#", "", x = _),
        "000000",
        x = _
      )
  ) |>
  dplyr::arrange(stadium, species) |>
  (\(x)
   if (exists("mode_source") && grepl("presentation", mode_source)) {
     x |>
       dplyr::filter(!grepl("M|N", species)) |>
       dplyr::select(-"stadium")
   } else {
     x
   }
  )() |>
  make_table_display(
    data_table = _,
    cols_id = if (exists("mode_source") && grepl("presentation", mode_source)) {
      c("species")
      } else{
        c("species", "stadium")
      } ,
    cols_addon = NULL,
    footnote_data = footnote_combined
  )


# --- all species - create species display table ---------------

# get species
species_display <- table_base_upd |>
  dplyr::distinct(species, vern_name_eng, .keep_all = TRUE) |>
  dplyr::arrange(species) |>
  make_table_species_display(
    data_table = _,
    cols_addon = NULL
  )



# --- all species - create full base table for display ---------------

# sort base table
table_base_upd <- table_base_upd |>
  dplyr::arrange(
    scope_boolean |> dplyr::desc(),
    scope_type,
    prius_milieu,
    stadium,
    #taxon,
    m_score_feas |> dplyr::desc()
  )


kingdom_list <- setNames(
  list("plant", "animal"),
  c("plant", "animal")
)
id_cols_display <- c("species", "vern_name_eng", "prius_milieu", "stadium")
# "taxon", "on_unionlist"

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

# --- all species - create full filtered table for display ---------------

# sort filtered table
table_filtered_upd <- table_filtered_upd |>
  dplyr::arrange(
    scope_prior,
    scope_type,
    prius_milieu,
    stadium,
    #taxon,
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
        cols_addon = NULL,
        footnote_data = footnote_combined
      )
  }
)

# --- all species - create full synergy tables for display - plants & animals ---------------

table_syn_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_upd,
  .kingdom = "plant",
  rest = FALSE
)
table_syn_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_upd,
  .kingdom = "animal",
  rest = FALSE
)

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
species_anb_tmp <- table_base_filtered |>
  dplyr::filter(
    grepl(
      paste(species_strings_anb, collapse = "|"),
      vern_name_nld,
      ignore.case = TRUE
    ) | (kingdom == "plant" & grepl("freshwater|marine", prius_milieu))
  ) |>
  dplyr::filter(grepl("highprior|lowprior", scope_prior))
species_anb <- species_anb_tmp |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)
# at leat one scope high prior
species_anb_highprior <- species_anb_tmp |>
  dplyr::filter(grepl("highprior", scope_prior)) |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)
# all low prior
species_anb_lowprior <- species_anb_tmp |>
  dplyr::filter(grepl("lowprior", scope_prior)) |>
  dplyr::filter(!vern_name_nld %in% species_anb_highprior$vern_name_nld) |>
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
table_filtered_anb_prior <- table_filtered_anb |>
  dplyr::filter(grepl("highprior", scope_prior))

table_filtered_anb_display_list <- lapply(
  kingdom_list,
  \(x){ table_filtered_anb |>
      dplyr::filter(grepl(x, kingdom)) |>
      make_table_display(
        data_table = _,
        cols_id = id_cols_display,
        cols_addon = NULL,
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
  .kingdom = "animal",
  .remove_n_1 = FALSE
)


# --- scenario 2 - detection - get species ---------------
#
#
species_det <- table_filtered_upd |>
  dplyr::filter(
    grepl("detection", scope_type) &
      grepl("highprior|lowprior", scope_prior)
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
        cols_addon = NULL,
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
  .kingdom = "animal",
  .remove_n_1 = FALSE
)
#
# --- scenario 3 - distribution / not detection - get species ---------------
#
species_dist <- table_filtered_upd |>
  dplyr::filter(
    !grepl("detection", scope_type) &
      grepl("highprior|lowprior", scope_prior)
  ) |>
  dplyr::distinct(species, vern_name_nld, .keep_all = TRUE)


# --- scenario 3 - distribution / not detection - filtered tables ---------------
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
        cols_addon = NULL,
        footnote_data = footnote_combined
      )
  }
)

#
# --- scenario 3 - distribution / not detection - synergy tables - plants & animals ---------------
#
table_syn_dist_plants_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_dist,
  .kingdom = "plant"
)
table_syn_dist_animals_display_list  <- make_table_syn_display_list(
  .table_filtered = table_filtered_dist,
  .kingdom = "animal"
)

