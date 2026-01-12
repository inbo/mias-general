rm_notes_presentation <- function(
    main_file = "source/presentation_ec/presentation_ec.qmd",
    output_main_file = "source/presentation_ec/presentation_ec_nonotes.qmd",
    sections_dir = "source/presentation_ec/sections",
    output_sections_dir = "source/presentation_ec/sections_nonotes"
    ) {
  # ---- Helper function: remove notes from one .qmd ----
  remove_notes <- function(input_file, output_file) {
    lines <- readLines(input_file, warn = FALSE)
    inside_notes <- FALSE
    output_lines <- c()

    for (line in lines) {
      if (grepl("^:::\\s*\\{\\.notes\\}", line)) {
        inside_notes <- TRUE
        next
      }
      if (inside_notes && grepl("^:::\\s*$", line)) {
        inside_notes <- FALSE
        next
      }
      if (!inside_notes) {
        output_lines <- c(output_lines, line)
      }
    }

    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    writeLines(output_lines, output_file)
  }

  # ---- Step 1: Clean section files ----
  files <- list.files(sections_dir, pattern = "\\.qmd$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No .qmd files found in '", sections_dir, "'.")
  }

  message("🧹 Removing notes from section files...")
  for (f in files) {
    out_f <- file.path(output_sections_dir, basename(f))
    remove_notes(f, out_f)
    message("   ✅ ", basename(f))
  }
  message("🎉 Clean section files written to '", output_sections_dir, "'")

  # ---- Step 2: Create clean version of main file ----
  main_lines <- readLines(main_file, warn = FALSE)

  sections_folder_name <- basename(sections_dir)
  output_sections_folder_name <- basename(output_sections_dir)

  # Replace paths to sections
  main_lines <- gsub(
    paste0("\\{<\\s*include\\s+[^>]*\\b", sections_folder_name, "/"),
    paste0("{< include ", output_sections_folder_name, "/"),
    main_lines
  )
  writeLines(main_lines, output_main_file)
  message("📄 Clean main file written to '", output_main_file, "'")

  message("Note: You need to manually adapt the output-dir in the '_quarto.yml' before rendering the clean main file")

}
