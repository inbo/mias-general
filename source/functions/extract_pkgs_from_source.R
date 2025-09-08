extract_pkgs_from_source <- function(source_dir = file.path("source", "export", "survey_experts")) {

  # Get all .R and .qmd files within source_dir
  r_files <- list.files(
    source_dir,
    full.names = TRUE,
    recursive = TRUE
    ) |>
    grep("(\\.qmd$|(?<!\\.Rmd)\\.R$)", x = _, value = TRUE, perl = TRUE) |>
    # remove path to flandersqmd extensions
    grep("_extensions", x = _, invert = TRUE, value = TRUE)

  # Read contents of all .R files
  contents <- lapply(r_files, readLines, warn = FALSE) |>
    unlist(x = _, use.names = FALSE)

  # Remove commented lines (lines that start with "#" after optional whitespace)
  uncommented <- contents[!grepl("^\\s*#", contents)]

  # Regex to capture "pkg::fun" and "pkg:::fun"
  matches <- regmatches(
    uncommented,
    gregexpr("\\b([A-Za-z0-9\\.]+)::+?[A-Za-z0-9_\\.]+", uncommented)
    ) |>
    unlist(x = _)

  # Extract just the package names (before "::")
  pkg <- gsub("::.*", "", matches)

  # Unique, sorted list
  #unique(sort(pkg))
  table(pkg) |> as.data.frame() |> dplyr::arrange(dplyr::desc(Freq))
}
