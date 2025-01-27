rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions & preparations ---------------
#
source('source/export/survey_experts/00_definitions.R')
#
#
# response data
load(paste0(response_data_path, "results_combined.rda"))
#
# define subsets of response data
res_scored <- res_comb |>
  dplyr::filter(question_scored |> as.logical(), !section_skipped)
res_open <- res_comb |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped)
#
#
#
# --- summarize responses (functions) ---------------
#
#
# mean function
sum_mean <- function(
    res_scored,
    crit,
    group_by_what = "species"
){
  res_mean <- res_scored |>
    dplyr::filter(
      grepl(crit, score_crit)
    ) |>
    dplyr::mutate(
      mean = mean(response_score, na.rm = TRUE),
      .by = group_by_what
    ) |>
    dplyr::mutate(
      mean = mean(mean),
      .by = "species"
    ) |>
    dplyr::distinct(species, .keep_all = TRUE) |>
    dplyr::select(tidyselect::contains(c("species","mean","union", "ven"))) |>
    dplyr::arrange(desc(mean)) |>
    dplyr::rename_with(~ paste0("mean_", crit), "mean")
}
#
# function to factorize variables
factorize <- function(
    res,
    varnames,
    varlevels
) {
  for (i in seq_along(varnames)) {
    res  <- res  |>
      dplyr::mutate(
        !!varnames[i] := factor(get(varnames[i]), levels = varlevels[[i]])
      )
  }
  return(res)
}
#
#
#
# --- plot ranking according to mean scores ---------------
#
# grand means urgency & feasibility
res_mean_feas <- sum_mean(res_scored, "feas")
res_mean_urge <- sum_mean(res_scored, "urge")
res_mean <- dplyr::full_join(res_mean_feas, res_mean_urge) |>
  dplyr::mutate(
    mean_feasurge = mean(c(mean_feas, mean_urge)),
    .by = species
  ) |> dplyr::arrange(dplyr::desc(mean_feasurge))
#
# section group means urgency & feasibility
res_mean_feas_gm <- sum_mean(res_scored, "feas", group_by_what = c("species", "section_no"))
res_mean_urge_gm <- sum_mean(res_scored, "urge", group_by_what = c("species", "section_no"))
res_mean_gm <- dplyr::full_join(res_mean_feas_gm, res_mean_urge_gm) |>
  dplyr::mutate(
    mean_feasurge = mean(c(mean_feas, mean_urge)),
    .by = species
  ) |> dplyr::arrange(dplyr::desc(mean_feasurge))
#
#
# convert to plot data
res_plot_tmp <- res_mean |>
  dplyr::mutate(
    mean_max = dplyr::case_when(
      mean_feas > mean_urge ~ mean_feas,
      TRUE ~ mean_urge
  ))
res_plot <- factorize(
  res_plot_tmp,
  c("species", "ven_name_eng", "ven_name_nld"),
  list(
    res_mean$species |> unique() |> rev(),
    res_mean$ven_name_eng |> unique() |> rev(),
    res_mean$ven_name_nld |> unique() |> rev()
  )
)
#
# plot
ggplot2::ggplot(
  res_plot,
  ggplot2::aes(x = mean_feasurge, y = species, color = on_unionlist)) +
  ggplot2::geom_point() +
  ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = mean_max), linetype = "dashed") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = mean_feasurge)) +
  ggplot2::geom_point(ggplot2::aes(x = mean_feas), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = mean_feas), shape = "F") +
  ggplot2::geom_point(ggplot2::aes(x = mean_urge), color = "white", size = 2) +
  ggplot2::geom_point(ggplot2::aes(x = mean_urge), shape = "U") +
  ggplot2::scale_y_discrete(labels = res_plot$ven_name_nld |> levels()) +
  ggplot2::coord_cartesian(xlim = c(
    res_scored$response_score |> na.omit() |> min(),
    res_scored$response_score |> na.omit() |> max()
    )) +
  ggplot2::theme_bw() +
  NULL
#
#
# --- plot score categories 'unknown' vs. rest ---------------
#
# convert to plot data
res_plot_tmp <- res_scored |>
  dplyr::mutate(
    question_text_trunc = stringr::str_trunc(string = question_text, width = 40),
    score_category = dplyr::case_when(
      !grepl("ongekend|ik weet het niet", response_text) ~ "rest",
      TRUE ~ response_text |> gsub(" ", "", x = _)
    )
  ) |>
  # score category proportions per species
  dplyr::mutate(
    n_scores_category = dplyr::n(),
    .by = c(species, score_category)
  ) |>
  dplyr::mutate(
    n_scores_tot = dplyr::n(),
    .by = species
  ) |>
  dplyr::mutate(prop_scores = n_scores_category / n_scores_tot) |>
  tidyr::pivot_wider(
    data = _,
    names_from = score_category,
    values_from = prop_scores,
    names_prefix = "prop_scores_"
  ) |>
  dplyr::group_by(species) |>
  tidyr::fill(tidyselect::contains("prop_scores"), .direction = "downup") |>
  dplyr::ungroup()

# check whether proportions sum to 1
test <- res_plot_tmp |>
  dplyr::mutate(dplyr::across(tidyselect::contains("prop_scores"), \(x) tidyr::replace_na(x, 0))) |>
  dplyr::rowwise() |>
  dplyr::mutate(prop_check = rowSums(dplyr::across(tidyselect::starts_with("prop_scores"))))
assertthat::are_equal(sum(test$prop_check), nrow(test))
#
# convert character vars to factors for ordering
res_plot <- factorize(
  res_plot_tmp,
  c("question_text", "question_text_trunc", "section_title", "species", "ven_name_nld"),
  list(
    res_plot_tmp$question_text |> unique(),
    res_plot_tmp$question_text_trunc |> unique(),
    res_plot_tmp$section_title |> unique() |> _[match(
        seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
        res_plot_tmp$section_no |> unique()
      )],
    res_mean$species |> unique() |> rev(),
    res_mean$ven_name_nld |> unique() |> rev()
  )
)
#
# plot
ggplot2::ggplot(
  res_plot,
  ggplot2::aes(y = species)
) +
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = 0, xmax = prop_scores_ongekend + prop_scores_ikweethetniet),
    color = "#EA5F94",
    size = 2
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = prop_scores_ongekend + prop_scores_ikweethetniet,
                 xmax = prop_scores_ongekend + prop_scores_ikweethetniet + prop_scores_rest),
    color = "lightgrey",
    size = 2
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  ) +
  #ggplot2::scale_color_manual(values = c("#EA5F94", "#EA5F94", "lightgrey")) +
  ggplot2::coord_cartesian(xlim = c(0,1)) +
  ggplot2::scale_y_discrete(labels = res_plot$ven_name_nld |> levels())
#
#
ggplot2::ggplot(
  # group by stadium?
  res_plot,
  ggplot2::aes(
    x = question_text, y = species,
    fill = score_category, color = score_category, shape = score_category
  )
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(section_title),
    scales = "free",
    space = "free"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  ) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21)) +
  ggplot2::scale_color_manual(values = c("#EA5F94", "#EA5F94", "lightgrey")) +
  ggplot2::scale_fill_manual(values = c("#EA5F94", "#EA5F94", "white")) +
  ggplot2::scale_x_discrete(labels = res_plot$question_text_trunc |> levels()) +
  ggplot2::scale_y_discrete(labels = res_plot$ven_name_nld |> levels())

