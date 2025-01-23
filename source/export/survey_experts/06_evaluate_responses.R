rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
source('source/export/survey_experts/00_definitions.R')
#
#
#
# --- load questions and response data ---------------
#
q_file <- list.files(
  questions_path,
  pattern = "long.rda",
  full.names = TRUE
)
q_long <- get(load(q_file))
#
res_file <- list.files(
  response_data_path,
  pattern = "long.rda",
  full.names = TRUE
) |>
  # sort & select most recent
  sort(decreasing = TRUE) |>
  _[1]
res_long <- get(load(res_file))
#
#
#
# --- reshape questions according to structure of responses ---------------
#
q_long_upd_tmp <- q_long |>
  # filter relevant questions
  dplyr::filter(
    question_include_in_form |> as.logical(),
    !grepl("Vooraf", section_title)
  ) |>
  # organize questions and questions_followup in long format
  dplyr::mutate(
    question_text_fu = dplyr::case_when(
      !is.na(question_explanation_follow_up) ~ paste("followup", question_text),
      TRUE ~ NA_character_
    ),
    .after = question_text
  ) |>
  tidyr::pivot_longer(
    cols = starts_with("question_text"),
    names_to = "tmp",
    values_to = "q_text"
  ) |>
  dplyr::mutate(
    response_score = dplyr::case_when(
      grepl("followup", q_text) ~ NA_real_,
      TRUE ~ score_response_option
    ),
    response_required = dplyr::case_when(
      grepl("followup", q_text) ~ 0,
      TRUE ~ response_required
    ),
    question_use_for_ranking = dplyr::case_when(
      grepl("followup", q_text) ~ 0,
      TRUE ~ question_use_for_ranking
    )
  ) |>
  tidyr::drop_na(q_text) |>
  # adapt question text to format in responses
  dplyr::mutate(
    q_text_upd = q_text |>
      gsub(pattern = "\\s+|-", replacement = "_", x = _) |>
      tolower())
#
# isolate meta data
q_meta <- q_long_upd_tmp |>
  dplyr::select(
    c("question_use_for_ranking", "response_required", "score_category", "section_title", "section_number", "q_text_upd")
    ) |>
  dplyr::distinct(q_text_upd, .keep_all = TRUE)
q_long_upd <- q_long_upd_tmp |>
  dplyr::select(
    c("q_text_upd", "response_option", "response_score")
  )
#
# modify meta data
q_meta <- q_meta |>
  # fix error in original question data
  dplyr::mutate(
    question_use_for_ranking = dplyr::case_when(
      grepl("welke_bemonsteringsmethoden", q_text_upd) ~ 0,
      TRUE ~ question_use_for_ranking
    )
  ) |>
  # rename
  dplyr::rename(
    score_crit = "score_category",
    section_no = "section_number"
  )

#
#
# --- join questions and responses ---------------
#
res_all <- dplyr::left_join(
  x = res_long,
  y = q_long_upd |> dplyr::select(-dplyr::contains("section")),
  by = c(
    "question_upd" = "q_text_upd",
    "response" = "response_option"
  )
) |>
  dplyr::left_join(
    x = _,
    y = q_meta,
    by = c("question_upd" = "q_text_upd")
  ) |>
  # add indicator whether section was skipped
  dplyr::mutate(
    section_skipped = dplyr::case_when(
      grepl("afw", stadium) & grepl("Versp", section_title) ~ TRUE,
      grepl("spo|wijd", stadium) & grepl("Intro", section_title) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  # rename
  dplyr::rename(
    question_text = "question_upd",
    response_text = "response",
    question_scored = "question_use_for_ranking"
    )
#
# define subsets of response data
res_scored <- res_all |>
  dplyr::filter(question_scored |> as.logical(), !section_skipped)
res_open <- res_all |>
  dplyr::filter(!question_scored |> as.logical(), !section_skipped)
#
#
# --- summarize responses (functions) ---------------
#
# mean function
sum_mean <- function(
    res_closed,
    crit,
    group_by_section = FALSE
){
  res_sum <- res |>
    dplyr::filter(
      grepl(crit, score_crit)
    ) |>
    dplyr::summarise(
      mean = mean(response_score, na.rm = TRUE),
      .by = if(group_by_section) c("species", "section_number") else "species"
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
res_sum_feas_gm <- sum_mean(res_scored, "feas", group_by_section = TRUE)
res_sum_urge_gm <- sum_mean(res_scored, "urge", group_by_section = TRUE)
res_sum_gm <- dplyr::full_join(res_sum_feas_gm, res_sum_urge_gm) |>
  dplyr::mutate(
    mean_feasurge = mean(c(mean_feas, mean_urge)),
    .by = species
  ) |> dplyr::arrange(dplyr::desc(mean_feasurge))
#
#
# --- plot score categories ---------------
#
# convert to plot data
res_plot_tmp <- res_scored |>
  dplyr::mutate(
    # question_no = dplyr::row_number(), .by = c(species, section_no)
    question_text_trunc = stringr::str_trunc(string = question_text, width = 30),
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

