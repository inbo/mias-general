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
# species list
load("data/processed/2025-01-27_species_list.Rda")
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
  res_sum <- res_scored |>
    dplyr::filter(
      grepl(crit, score_crit)
    ) |>
    dplyr::summarise(
      mean = mean(response_score, na.rm = TRUE),
      .by = group_by_what
    ) |>
    dplyr::summarise(
      mean = mean(mean),
      .by = "species"
    ) |>
    dplyr::arrange(desc(mean)) |>
    dplyr::rename_with(~ paste0("mean_", crit), "mean")
}
#
#
# --- summarize responses ---------------
#
# grand means urgency & feasibility
res_sum_feas <- sum_mean(res_scored, "feas")
res_sum_urge <- sum_mean(res_scored, "urge")
res_sum <- dplyr::full_join(res_sum_feas, res_sum_urge) |>
  dplyr::mutate(
    mean_feasurge = mean(c(mean_feas, mean_urge)),
    .by = species
  ) |> dplyr::arrange(dplyr::desc(mean_feasurge))
#
# section group means urgency & feasibility
res_sum_feas_gm <- sum_mean(res_scored, "feas", group_by_what = c("species", "section_no"))
res_sum_urge_gm <- sum_mean(res_scored, "urge", group_by_what = c("species", "section_no"))
res_sum_gm <- dplyr::full_join(res_sum_feas_gm, res_sum_urge_gm) |>
  dplyr::mutate(
    mean_feasurge = mean(c(mean_feas, mean_urge)),
    .by = species
  ) |> dplyr::arrange(dplyr::desc(mean_feasurge))
#
#
# --- plot score categories 'unknown' vs. rest ---------------
#
# convert to plot data
res_plot_tmp <- res_scored |>
  dplyr::mutate(
    # question_no = dplyr::row_number(), .by = c(species, section_no)
    question_text_trunc = stringr::str_trunc(string = question_text, width = 40),
    score_category = dplyr::case_when(
      !grepl("ongekend|ik weet het niet", response_text) ~ "rest",
      TRUE ~ response_text
    )
  ) |>
  # score category proportions per species
  dplyr::mutate(
    n_scores_category = dplyr::n(),
    .by = c(species, score_category)
  ) |>
  dplyr::mutate(
    n_scores = dplyr::n(),
    .by = species
  ) |>
  dplyr::mutate(
    prop_scores_category = n_scores_category / n_scores,
    prop_scores_rest = dplyr::case_when(
      grepl("rest", score_category) ~ prop_scores_category,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::group_by(species) |>
  tidyr::fill(prop_scores_rest, .direction = "downup") |>
  dplyr::ungroup()

#
# convert character vars to factors for ordering
res_plot <- res_plot_tmp |>
  dplyr::mutate(
    question_text = factor(
      question_text,
      levels = res_plot_tmp$question_text |> unique()
    ),
    question_text_trunc = factor(
      question_text_trunc,
      levels = res_plot_tmp$question_text_trunc |> unique()
    ),
    section_title = factor(
      section_title,
      levels = res_plot_tmp$section_title |> unique() |>
        _[match(
          seq(min(res_plot_tmp$section_no), max(res_plot_tmp$section_no)),
          res_plot_tmp$section_no |> unique()
        )]
    ),
    species = factor(
      species,
      levels = res_plot_tmp |>
        dplyr::arrange(prop_scores_rest) |>
        dplyr::pull(species) |> unique() |> rev()
    )
  )

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
  ggplot2::scale_x_discrete(labels = res_plot$question_text_trunc |> levels())

