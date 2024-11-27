rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
#
#
# --- definitions ---------------
#
# id of sheet with questions to be asked in form
# currently: PRJ_MIUS\_overkoepelend\bevraging_soortenexperts\questions.gsheet
sheet_id <- "1MikuShtt9mFdb5f2Nzts7pFR5MZ6HR7h-6Lx0bcEF7k"
#
# path to save questions
questions_outpath <- "source/export/survey_experts/"
#
#
# --- import data with questions and answers from google sheet ---------------
#
# get sheet tab names
# (authentification needed)
sheet_tab_names <- googlesheets4::sheet_names(ss = sheet_id)
#
# read data from all sheet tabs
questions_list <- lapply(
  sheet_tab_names,
  googlesheets4::read_sheet,
  ss = sheet_id,
  .name_repair = function(x) {
    # rename variables
    gsub(pattern = "\\s+|-", replacement = "_", x) |>
      tolower()
  }
)
#
# combine data
questions <- questions_list |>
  dplyr::bind_rows()
#
# fill missings with last value
questions_long <- questions |>
  tidyr::fill(dplyr::starts_with(c("section", "question_id"))) |>
  dplyr::group_by(question_id) |>
  tidyr::fill(dplyr::everything()) |>
  dplyr::ungroup()
#
# save questions
save(questions_long, file = paste0(questions_outpath, "questions_long.rda"))

