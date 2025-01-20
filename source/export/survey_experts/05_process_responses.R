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
questions_file <- list.files(
  questions_path,
  pattern = "long.rda",
  full.names = TRUE
)
questions_long <- get(load(questions_file))
#
response_file <- list.files(
  response_data_path,
  pattern = "long.rda",
  full.names = TRUE
) # sort & select most recent
responses_long <- get(load(questions_file))

