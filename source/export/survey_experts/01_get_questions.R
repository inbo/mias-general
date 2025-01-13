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
# --- import data with questions and answers from google sheet ---------------
#
# get sheet tab names
# (authentification needed)
sheet_tab_names <- googlesheets4::sheet_names(ss = sheet_id)
#
# read data from all sheet tabs
questions_list <- lapply(
  sheet_tab_names,
  function(i){
    googlesheets4::read_sheet(
      ss = sheet_id,
      sheet = i,
      .name_repair = function(x) {
        # rename variables
        gsub(pattern = "\\s+|-", replacement = "_", x) |>
          tolower()
      }
    ) |>
      # add tab names as variable
      dplyr::mutate(question_id := i, .before = 1)
  }
)
#
# combine data across tabs
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
# reshape response options to wide
questions_wide <- questions_long |>
  dplyr::mutate(
    tmp = paste0(dplyr::row_number()),
    .by = question_id
  ) |>
  tidyr::pivot_wider(
    values_from = c(response_option, score_response_option),
    names_from = tmp
  )
#
# save questions
save(questions_long, file = paste0(questions_path, "questions_long.rda"))
save(questions_wide, file = paste0(questions_path, "questions_wide.rda"))
#
#
#
# --- create questions overview pdf -------------
#
# create individual sections
create_overview_questions(
    data_qa = questions_wide |>
      dplyr::filter(
        question_include_in_form == 1
      ),
    name_qtype = "response_format",
    name_q = "question_text",
    name_qexpl = "question_explanation",
    name_qexplfu = "question_explanation_follow_up",
    basename_aoptions = "response_option",
    name_secno = "section_number",
    name_sectitle = "section_title",
    path_section_templatefile = paste0(questions_path, "overview_section_template.Rmd"),
    path_section_out = paste0(questions_path, "overview_sections/")
  )
#
# render pdf
pdf_name <- paste0("overview_questions_", lang, ".pdf")
rmarkdown::render(
  input = paste0(questions_path, "overview_master.Rmd"),
  output_dir = questions_path,
  output_file = pdf_name
)
#
# upload pdf
googledrive::drive_upload(
  media = paste0(questions_path, pdf_name),
  path = media_folder_url |> googledrive::as_id(),
  overwrite = TRUE
)
#
# make pdf public
googledrive::drive_find(
  pattern = pdf_name,
  type = "pdf",
  shared_drive = "PRJ_MIUS"
) |>
  googledrive::drive_share_anyone()

