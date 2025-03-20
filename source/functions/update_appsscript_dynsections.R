update_appsscript_dynsections <- function(
    #qtext_filter,
    #aoptions_updated,
    #section_to_move,
    #section_fu
  path_to_appscript,
  lang = "NL"
){
  appsscript <- readLines(path_to_appscript)
  #
  # split script
  s1_lines <- grep(
    ifelse(
      lang == "NL",
      "section introductievestiging",
      "section introductionestablishment"
      ),
    appsscript)
  s2_lines <- grep(
    ifelse(
      lang == "NL",
      "section verspreidingabundantie",
      "section distributionabundance"),
    appsscript)
  appsscript_begin <- appsscript[1 : (s1_lines[1] - 1)]
  appsscript_s1 <- appsscript[s1_lines[1] : s1_lines[2]]
  appsscript_s2 <- appsscript[s2_lines[1] : s2_lines[2]]
  appsscript_end <- appsscript[(s2_lines[2] +1) : length(appsscript)]
  #
  # duplicate section introductievestiging & update
  appsscript_s1_dupl <- appsscript_s1 |>
    gsub(
      pattern = ifelse(
          lang == "NL",
          "introductievestiging",
          "introductionestablishment"
          ),
      replacement = ifelse(
          lang == "NL",
          "introductievestigingdupl",
          "introductionestablishmentdupl"
          )
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
      pattern = ifelse(
        lang == "NL",
          "'afwezig'",
          "'absent'"
          ),
      replacement = ifelse(
        lang == "NL",
          "'afwezig', introductievestiging",
          "'absent', introductionestablishment"
          )
    ) |>
    gsub(
      pattern = ifelse(
        lang == "NL",
                       "'sporadisch aanwezig'",
                       "'sporadically present'"
                       ),
      replacement = ifelse(
        lang == "NL",
                           "'sporadisch aanwezig', introductievestigingdupl",
                           "'sporadically present', introductionestablishmentdupl"
                           )
    ) |>
    gsub(
      pattern = ifelse(
        lang == "NL",
        "'beperkt gevestigd'",
        "'established to limited extend'"
        ),
      replacement = ifelse(
        lang == "NL",
        "'beperkt gevestigd', verspreidingabundantie",
        "'established to limited extend', distributionabundance"
        )
      ) |>
    gsub(
      pattern = ifelse(
        lang == "NL",
        "'wijdverspreid'",
        "'widespread'"
        ),
      replacement = ifelse(
        lang == "NL",
        "'wijdverspreid', verspreidingabundantie",
        "'widespread', distributionabundance"
        )
    )
  #
  # insert section jumps
  end_line <- grep(ifelse(
    lang == "NL",
    "end section beheer",
    "end section management"
    ),
    appsscript_end)
  appsscript_end <- append(
    appsscript_end,
    if ( lang == "NL") {
      c(
        "introductievestigingdupl.setGoToPage(impact);",
        "introductievestiging.setGoToPage(verspreidingabundantie);",
        "verspreidingabundantie.setGoToPage(verspreidingabundantie);"
      )
    } else {
      c(
        "introductionestablishmentdupl.setGoToPage(impact);",
        "introductionestablishment.setGoToPage(distributionabundance);",
        "distributionabundance.setGoToPage(distributionabundance);"
      )
    },
    after = end_line
  )
  #
  # recombine script
  appsscript_beginend <- c(appsscript_begin, appsscript_end)
  q_line <- grep(ifelse(
    lang == "NL",
    "In welk invasiestadium befindt zich de soort in Vlaanderen?",
    "Which invasion stage is the species in in Flanders?"
    ),
    appsscript_beginend)
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
