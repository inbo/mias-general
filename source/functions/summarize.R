summarize <- function(
    res_data,
    variable = "response_score",
    grepl_crit = "feas|urge",
    group_by_what = "species",
    fun = "mean",
    cols_id_prefix = c("species","vern", "stadium","on_union")
){
  res_mean <- res_data |>
    dplyr::filter(
      grepl(grepl_crit, score_crit)
    ) |>
    # fun output per group
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_by_what))) |>
    dplyr::mutate(m = do.call(fun, list(get(variable), na.rm = TRUE))) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::select(c(tidyselect::starts_with(cols_id_prefix), "m")) |>
    dplyr::arrange(dplyr::desc(m)) |>
    dplyr::rename_with(~ paste0("m_", grepl_crit), "m")
}
