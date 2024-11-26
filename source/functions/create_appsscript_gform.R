create_appsscript_gform <- function(
    data_qa,
    name_qid,
    name_qtype,
    name_q,
    name_qexpl,
    name_qexplfu,
    basename_aoptions,
    name_areq,
    name_secno,
    name_sectitle,
    form_titlebase,
    gdrive_destfolder_id,
    species_qtext,
    species_qtext_map,
    species_info,
    url_qoverview
){
  #
  # define species information
  species_names <- species_info$names
  specias_maps <- species_info$maps

  #
  # define loop over forms
  form_titlelist <- if (FALSE) {
    paste(form_titlebase, seq_along(species_names))
  } else {
    paste(form_titlebase, species_names)
    }
  script_loop_start <- paste(
    sprintf(
      "var formtitlelist = ['%s'];",
      form_titlelist |> paste(collapse = "','")
    ),
    sprintf(
      "var specieslist = ['%s'];",
      species_names |> paste(collapse = "','")
    ),
    sprintf(
      "var mapidlist = ['%s'];",
      specias_maps |> paste(collapse = "','")
    ),
    "for (var i = 0; i < formtitlelist.length; i++) {",
    "var formtitle = formtitlelist[i];",
    "var species = specieslist[i];",
    "var mapid = mapidlist[i];",
    NULL,
    sep = "\n"
  )
  script_loop_end <- "}"
  #
  # define form title & description
  script_title <- paste(
    "var form = FormApp.create(formtitle);",
    sprintf(
      "form.setDescription('%s'.concat('%s', species, '%s', '%s', '%s', '%s'));",
      "Bedankt om aan deze bevraging over de soort ",
      "\"",
      "\"",
      " deel te nemen.",
      " Voor een overzicht over de volledige vragenlijst zie: ",
      url_qoverview
    ),
    NULL,
    sep = "\n"
  )
  #
  # define sections, questions, answers
  script_body <- c()
  for (i in 1:nrow(data_qa)) {
    #
    # get s & q & a information
    s_new <- FALSE
    s_name <- data_qa[i, name_sectitle]
    if (i > 1) {
      if (diff(unlist(data_qa[, name_secno]))[i - 1] > 0) {
        s_new <- TRUE
      }
    }
    q_id <- data_qa[i, name_qid] |>
      unlist()
    q_type <- data_qa[i, name_qtype] |>
      unlist()
    q_text <- data_qa[i, name_q] |> unlist()
    q_expl <- data_qa[i, name_qexpl] |> unlist() |> na.omit()
    a_options <- data_qa[i,] |>
      dplyr::select(dplyr::starts_with(basename_aoptions)) |>
      unlist() |>
      na.omit()
    q_expl_fu <- data_qa[i, name_qexplfu] |> unlist() |> na.omit()
    #
    # define new section & mark beginning of section
    s_varname <- s_name |>
      gsub(pattern = "\\s+|[[:punct:]]", replacement = "") |>
      tolower()
    script_s <- if (s_new) {
      paste(
        paste("// begin section", s_varname),
        paste("var", s_varname, "= form.addPageBreakItem();"),
        sprintf(
          #"%s.setTitle('%s'.concat('\\n[', species, ']'));",
          "%s.setTitle('%s');",
          s_varname,
          s_name
        ),
        NULL,
        sep = "\n")
    } else {
      NULL
    }
    #
    # define questions
    q_itemname <- paste0("item", q_id)
    script_q <- switch(
      q_type,
      "short answer" = paste(
        paste("var",q_itemname,"= form.addTextItem();"),
        sprintf(
          "%s.setTitle('%s');",
          q_itemname,
          q_text
        ),
        NULL,
        sep = "\n"),
      "paragraph" = paste(
        paste("var",q_itemname,"= form.addParagraphTextItem();"),
        sprintf(
          "%s.setTitle('%s');",
          q_itemname,
          q_text
        ),
        NULL,
        sep = "\n"),
      "multiple choice"= paste(
        paste("var",q_itemname,"= form.addMultipleChoiceItem();"),
        sprintf(
          "%s.setTitle('%s');",
          q_itemname,
          q_text
        ),
        NULL,
        sep = "\n"),
      "dropdown" = paste(
        paste("var",q_itemname,"= form.addListItem();"),
        sprintf(
          "%s.setTitle('%s');",
          q_itemname,
          q_text
        ),
        NULL,
        sep = "\n")
    )
    #
    # define question description
    script_qexpl <- if (length(q_expl) > 0) {
      sprintf("%s.setHelpText('\\n%s');", q_itemname, q_expl)
    } else {
      NULL
    }
    #
    # define answer options
    script_a <- if (q_type %in% c("multiple choice", "dropdown")) {
      paste(
        paste0(
          q_itemname,
          ".setChoices(["
        ),
        paste(
          sprintf("%s.createChoice('%s'),", q_itemname, a_options),
          NULL,
          collapse = "\n"
        ),
        "]);",
        NULL,
        sep = "\n"
      )
    } else {
      NULL
    }
    #
    # update answer options for question on species
    if (q_text == species_qtext){
      script_a <- paste(
        sprintf("%s.setChoices([", q_itemname),
        sprintf("%s.createChoice(species)", q_itemname),
        "]);",
        NULL,
        sep = "\n"
      )
    }
    #
    # define whether replies are required
    if (data_qa[i, name_areq] |> unlist() |> as.logical()) {
      script_a <- paste(
        script_a,
        paste0(
          q_itemname,
          ".setRequired(true);"
        ),
        sep = "\n"
      )
    }
    #
    # define question description follow-up
    script_qexplfu <- if (length(q_expl_fu) > 0) {
      paste(
        sprintf("form.addParagraphTextItem().setTitle('').setHelpText('%s');",
                q_expl_fu),
        NULL,
        sep = "\n"
      )
    } else {
      NULL
    }
    #
    # define image item (distribution map)
    script_image <- if (q_text == species_qtext_map) {
      paste(
        "var img = DriveApp.getFileById(mapid);",
        "var blob = img.getBlob();",
        "form.addImageItem().setImage(blob).setTitle('Verspreidingskaart op basis van GBIF gegevens');",
        NULL,
        sep = "\n"
      )
    } else {
      NULL
    }
    #
    # define end of section
    script_sb <- paste("// end section", s_varname)
    if (i < nrow(data_qa)) {
      if (diff(unlist(data_qa[, name_secno]))[i] == 0 | is.na(s_varname)) {
        script_sb <- NULL
      }
    }
    #
    # combine script parts per question
    script_body <- paste(
      script_body,
      script_s,
      script_image,
      script_q,
      script_qexpl,
      script_a,
      script_qexplfu,
      script_sb,
      sep = "\n"
    )
  }
  #
  # define destination folder on google drive
  script_save <- paste(
    sprintf("var folderId = '%s';", gdrive_destfolder_id),
    "var folder = DriveApp.getFolderById(folderId);",
    "var file = DriveApp.getFileById(form.getId());",
    "file.moveTo(folder);",
    NULL,
    sep = "\n"
  )
  #
  # put all components together
  script <- paste(
    "function createFormLoop() {",
    script_loop_start,
    script_title,
    script_body,
    script_save,
    script_loop_end,
    "}",
    sep = "\n"
  )
}
