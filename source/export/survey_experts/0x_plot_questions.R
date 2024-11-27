rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
# path to questions
questions_file <- "source/export/survey_experts/questions_long.rda"
#
# path
save_path <- "source/export/survey_experts/results/"
#
#
# --- import data with questions and answers ---------------
#
questions_long <- get(load(questions_file))
#
#
# --- visualize scores ---------------
#
# prepare data for plotting
questions_tmp <- questions_long |>
  # filter rows
  dplyr::filter(question_include_in_form == 1, question_use_for_ranking == 1) |>
  tidyr::drop_na(score_response_option) |>
  # insert line breaks in questions & shorten responses
  dplyr::mutate(index = dplyr::row_number()) |>
  dplyr::mutate(
    question_text = question_text |>
      stringr::str_wrap(width = 60) |>
      paste(x = _, NULL, collapse = "\n"),
    response_option = response_option |>
      stringr::str_trunc(string = _, width = 65),
    .by = index
  ) |>
  dplyr::select(-index)
# order questions & sections
levels_q <- questions_tmp$question_text |> unique() |> rev()
levels_s <- questions_tmp$section_title |> unique()
questions_plot <- questions_tmp |>
  dplyr::mutate(
    question_factor = factor(question_text, levels = levels_q),
    section_factor = factor(section_title, levels = levels_s)
    ) |>
  # merge labels based on response scores
  tidyr::pivot_wider(
    values_from = response_option,
    names_from = score_response_option,
    values_fn = function(x) paste(x, collapse = "\n")
  ) |>
  tidyr::pivot_longer(
    cols = paste(1:4),
    names_to = "score",
    values_to = "response"
  ) |>
  dplyr::mutate(
    score = as.numeric(score)
  )
#
plot <- ggplot2::ggplot(
  data = questions_plot,
  mapping = ggplot2::aes(x = score, y = question_factor)) +
  ggplot2::geom_rect(
    ggplot2::aes(fill = section_factor),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.05
    ) +
  ggplot2::geom_label(
    ggplot2::aes(label = response),
    hjust = 0,
    size = 3,
    angle = 0,
    alpha = .9
    ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(section_factor),
    scales = "free",
    space = "free") +
  ggplot2::scale_x_continuous(
    breaks = 1:4,
    labels = paste(1:4, c("\nKleine haalbaarheid / nood", "", "", "\nGrote haalbaarheid / nood"))
    ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_cartesian(xlim = c(0.5,5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    legend.position = "none"
    ) +
  cols4all::scale_fill_discrete_c4a_cat(palette = "carto.pastel")
ggplot2::ggsave(paste0(save_path, "question_overview_scoring.pdf"), width = 600, height = 250, units = "mm")





