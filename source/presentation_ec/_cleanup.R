list.files(
  path = "source/presentation_ec/",
  pattern = ".png|.jpg|.ttf|.css|.csl", full.names = TRUE
) |>
  file.remove()
