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



