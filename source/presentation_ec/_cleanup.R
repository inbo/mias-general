paths <- c(
  "source/presentation_ec",
  "source/presentation_ec/sections",
  "source/presentation_ec/sections_nonotes"
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

