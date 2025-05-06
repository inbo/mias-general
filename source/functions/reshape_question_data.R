reshape_question_data <- function(
    .q_long,
    .lang
) {
  q_upd_tmp <- .q_long |>
    # filter relevant questions
    dplyr::filter(
      question_include_in_form |> as.logical(),
      !grepl(
        if (lang == "NL") "Vooraf" else if (lang == "EN") "Preface",
        section_title
        )
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
  q_meta <- q_upd_tmp |>
    dplyr::select(
      c(
        "question_id", "question_use_for_ranking", "response_required",
        "score_category", "section_title", "section_number", "q_text_upd"
        )
    ) |>
    dplyr::distinct(q_text_upd, .keep_all = TRUE)
  q_upd <- q_upd_tmp |>
    dplyr::select(
      c("q_text_upd", "response_option", "response_score")
    )
  #
  # modify meta data
  q_meta <- q_meta |>
    # fix error in original question data
    dplyr::mutate(
      question_use_for_ranking = dplyr::case_when(
        grepl(
          if (lang == "NL") {
            "welke_bemonsteringsmethoden"
          } else if (lang == "EN"){
            "which_surveillance_techniques_are_available"
          },
          q_text_upd) ~ 0,
        TRUE ~ question_use_for_ranking
      )
    ) |>
    # rename
    dplyr::rename(
      score_crit = "score_category",
      section_no = "section_number"
    )
  q_list <- setNames(list(q_upd, q_meta), c("q_upd", "q_meta"))
}
