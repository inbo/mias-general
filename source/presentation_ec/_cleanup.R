paths <- c(
  "source/presentation_ec",
  "source/presentation_ec/sections"
  )
sapply(
  paths,
  \(x) {
    list.files(
      path = x,
      pattern = ".png|.jpg|.ttf|.css|.csl", full.names = TRUE
    ) |>
      file.remove()
  }
)

