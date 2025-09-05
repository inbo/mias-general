rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- load EN questions & reshape ---------------
#
# definitions
lang <- "EN"
source('source/export/survey_experts/00_definitions.R')
#
# load questions
q_file_EN <- list.files(
  questions_path,
  pattern = "long.rda",
  full.names = TRUE
)
q_long_EN <- get(load(q_file_EN))
#
# reshape questions according to structure of responses
q_upd_list_EN <- reshape_question_data(
  .q_long = q_long_EN,
  .lang = lang
)
#
#
#
# --- load NL questions & reshape
#
# definitions
lang <- "NL"
source('source/export/survey_experts/00_definitions.R')
#
# load questions
q_file <- list.files(
  questions_path,
  pattern = "long.rda",
  full.names = TRUE
)
q_long <- get(load(q_file))
#
# reshape questions according to structure of responses
q_upd_list <- reshape_question_data(
  .q_long = q_long,
  .lang = lang
)
#
#
#
# --- load response data and species list ---------------
#

#
res_file <- list.files(
  response_data_path,
  pattern = "long.rda",
  full.names = TRUE
) |>
  grep(pattern = "2025_03_24", x = _, value = TRUE)
res_long <- get(load(res_file))
#
species_list_file <- list.files(
  "data/processed/",
  pattern = "species_list",
  full.names = TRUE
) |>
  grep(pattern = "2025-04-30", x = _, value = TRUE) |>
  grep(pattern = "upd", x = _, value = TRUE)
species_list <- get(load(species_list_file))$data
#
#
#
# --- join NL questions and responses ---------------
#
res_comb_tmp <- dplyr::left_join(
  x = res_long,
  # HERE
  y = q_upd_list$q_upd |> dplyr::select(-dplyr::contains("section")),
  by = c(
    "question_upd" = "q_text_upd",
    "response" = "response_option"
  )
) |>
  dplyr::left_join(
    x = _,
    y = q_upd_list$q_meta,
    by = c("question_upd" = "q_text_upd")
  ) |>
  # add indicator whether section was skipped
  dplyr::mutate(
    section_skipped = dplyr::case_when(
      grepl("afwezig", stadium) & grepl("Verspreiding", section_title) ~ TRUE,
      grepl("beperkt|wijdverspreid", stadium) & grepl("Introductie", section_title) ~ TRUE,
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
#
#
# --- add species information ---------------
#
# restructure species information
species_info <- species_list |>
  dplyr::mutate(
    vern_name_eng = dplyr::case_when(
      is.na(vern_name_gbif_eng_alt) ~ vern_name_gbif_eng,
      TRUE ~ vern_name_gbif_eng_alt
    ),
    vern_name_nld = dplyr::case_when(
      is.na(vern_name_gbif_nld) & is.na(vern_name_gbif_nld_alt)  ~ vern_name_gsheet_nld,
      !is.na(vern_name_gbif_nld_alt)  ~ vern_name_gbif_nld_alt,
      TRUE ~ vern_name_gbif_nld
    ),
    sci_name = dplyr::case_when(
      grepl("Reynoutria", sci_name_gbif_acc) ~ sci_name_gbif_acc_alt,
      TRUE ~ sci_name_gbif_acc
    )
  ) |>
  dplyr::select(
    tidyselect::any_of(
      c("on_unionlist", "on_unionlist_2025", "kingdom", "taxon", "prius_stadium", "prius_milieu",
        "sci_name", "vern_name_eng", "vern_name_nld")
      )
    ) |>
  dplyr::distinct(sci_name, .keep_all = TRUE)
#
# join results data and species information
res_comb <- dplyr::left_join(
  x = res_comb_tmp ,
  y = species_info,
  by = c("species" = "sci_name")
)
#
# test whether join was successfull
assertthat::noNA(res_comb$vern_name_eng)
assertthat::noNA(res_comb$vern_name_nld)
#
#
#
# --- add english translation ---------------
#
#
#
# join questions EN & NL
colnames(q_upd_list_EN$q_upd) <- paste0(colnames(q_upd_list_EN$q_upd), "_EN")
q_upd <- cbind(
  q_upd_list$q_upd,
  q_upd_list_EN$q_upd
)
q_meta <- dplyr::full_join(
  q_upd_list$q_meta,
  q_upd_list_EN$q_meta |>
    dplyr::rename(
      "section_title_EN" = "section_title",
      "q_text_upd_EN" = "q_text_upd"
    ),
  by = c("question_id", "question_use_for_ranking", "response_required", "score_crit", "section_no")
)

#
# question text
q_text_NL_EN <- q_meta |>
  dplyr::select(tidyselect::contains("q_text")) |>
  dplyr::rename_with(~gsub("q_text_upd", "question_text", .x))
res_comb_tmp <- dplyr::left_join(
  res_comb,
  q_text_NL_EN
) |> dplyr::relocate(
  "question_text_EN",
  .after = "question_text"
)
#
# response text
r_option_NL_EN <- q_upd |>
  dplyr::select(tidyselect::contains("response_option")) |>
  dplyr::distinct() |>
  dplyr::rename_with(~gsub("response_option", "response_text", .x))
res_comb_tmp <- dplyr::left_join(
  res_comb_tmp,
  r_option_NL_EN
) |> dplyr::relocate(
  "response_text_EN",
  .after = "response_text"
)
#
# stadium
stadium_NL_EN <- q_long |>
  dplyr::filter(grepl("_3", question_id)) |>
  dplyr::select(response_option) |>
  cbind(
    x = _,
    y = q_long_EN |>
      dplyr::filter(grepl("_3", question_id)) |>
      dplyr::select(response_option) |>
      dplyr::rename("response_option_EN" = "response_option")
  ) |>
  dplyr::rename_with(~gsub("response_option", "stadium", .x))
res_comb_tmp <- dplyr::left_join(
  res_comb_tmp,
  stadium_NL_EN
) |> dplyr::relocate(
  "stadium_EN",
  .after = "stadium"
)

# section title
s_title_NL_EN <- q_meta |>
  dplyr::select(tidyselect::contains("section_title")) |>
  dplyr::distinct()
res_comb_tmp <- dplyr::left_join(
  res_comb_tmp,
  s_title_NL_EN
) |> dplyr::relocate(
  "section_title_EN",
  .after = "section_title"
)
#
assertthat::are_equal(
  res_comb,
  res_comb_tmp |> dplyr::select(-tidyselect::contains("EN", ignore.case = FALSE))
)
res_comb <- res_comb_tmp
#
#
#
# --- save ---------------
#
#
save(res_comb, file = paste0(response_data_path, "results_combined.rda"))
