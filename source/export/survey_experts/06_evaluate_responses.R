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
) # sort & select most recent
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
    score_upd = dplyr::case_when(
      grepl("followup", q_text) ~ NA_real_,
      TRUE ~ score_response_option
    )
  ) |>
  tidyr::drop_na(q_text) |>
  # adapt question text to format in responses
  dplyr::mutate(
    q_text_upd = q_text |>
      gsub(pattern = "\\s+|-", replacement = "_", x = _) |>
      tolower()) |>
  # select columns
  dplyr::select(
    c(
      "section_title", "section_number",
      "q_text_upd",
      "response_required", "response_option", "score_upd", "score_category"
    )
  )
#
# isolate meta data
q_meta <- q_long_upd_tmp |>
  dplyr::select(
    c("score_category", "section_title", "section_number", "q_text_upd")
    ) |>
  dplyr::distinct(q_text_upd, .keep_all = TRUE)
q_long_upd <- q_long_upd_tmp |>
  dplyr::select(
    -c("score_category", "section_title", "section_number")
  )
#
#
#
# --- join questions and responses ---------------
#
res_final <- dplyr::left_join(
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
  )




