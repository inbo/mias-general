list.files(
  path = "source/export/survey_experts/docu_presentation",
  pattern = ".png|.jpg|.ttf|.css|.csl", full.names = TRUE
  ) |>
  file.remove()
