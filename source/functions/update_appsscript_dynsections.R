update_appsscript_dynsections <- function(
    #qtext_filter,
    #aoptions_updated,
    #section_to_move,
    #section_fu
  path_to_appscript
){
  appsscript <- readLines(path_to_appscript)
  #
  # split script
  s1_lines <- grep("section introductievestiging", appsscript)
  s2_lines <- grep("section verspreidingabundantie", appsscript)
  appsscript_begin <- appsscript[1 : (s1_lines[1] - 1)]
  appsscript_s1 <- appsscript[s1_lines[1] : s1_lines[2]]
  appsscript_s2 <- appsscript[s2_lines[1] : s2_lines[2]]
  appsscript_end <- appsscript[(s2_lines[2] +1) : length(appsscript)]
  #
  # duplicate section introductievestiging & update
  appsscript_s1_dupl <- appsscript_s1 |>
    gsub(
      pattern = "introductievestiging",
      replacement = "introductievestigingdupl"
      )
  # duplicate section verspreidingabundantie & update
  #appsscript_s2_dupl <- appsscript_s2 |>
  #  gsub(
  #    pattern = "verspreidingabundantie",
  #    replacement = "verspreidingabundantiedupl"
  #    )
  #
  #
  # update answer options
  appsscript_begin <- appsscript_begin |>
    gsub(
      pattern = "'afwezig'",
      replacement = "'afwezig', introductievestiging"
    ) |>
    gsub(
      pattern = "'sporadisch aanwezig'",
      replacement = "'sporadisch aanwezig', introductievestigingdupl"
    ) |>
    gsub(
      pattern = "'beperkt gevestigd'",
      replacement = "'beperkt gevestigd', verspreidingabundantie"
      ) |>
    gsub(
      pattern = "'wijdverspreid'",
      replacement = "'wijdverspreid', verspreidingabundantie"
    )
  #
  # insert section jumps
  end_line <- grep("end section beheer", appsscript_end)
  appsscript_end <- append(
    appsscript_end,
    c(
      "introductievestigingdupl.setGoToPage(impact);",
      "introductievestiging.setGoToPage(verspreidingabundantie);",
      "verspreidingabundantie.setGoToPage(verspreidingabundantie);"
    ),
    after = end_line
  )
  #
  # recombine script
  appsscript_beginend <- c(appsscript_begin, appsscript_end)
  q_line <- grep("In welk invasiestadium befindt zich de soort in Vlaanderen?", appsscript_beginend)
  appsscript_begin_new <- appsscript_beginend[1:q_line]
  appsscript_end_new <- appsscript_beginend[(q_line+1):length(appsscript_beginend)]
  appsscript_upd <- c(
    appsscript_begin_new,
    appsscript_s1,
    appsscript_s1_dupl,
    appsscript_s2,
    appsscript_end_new
  )
  #
  #
  writeLines(
    appsscript_upd,
    path_to_appscript
    )
}
