create_appsscript_gform <- function(
    data_qa,
    name_qtype,
    name_q,
    basename_aoptions,
    name_secno,
    name_sectitle,
    form_title,
    gdrive_destfolder_id
){
  # start writing google apps script
  form_script_begin <- paste(
    "function createForm() {",
    sprintf("  var form = FormApp.create('%s');", form_title),
    NULL,
    sep = "\n"
  )
  #
  #
  # loop through each row of data with questions
  form_script_sqa <- c()
  for (i in 1:nrow(data_qa)) {

    q_type <- data_qa[i, name_qtype] |>
      unlist()
    question <- data_qa[i, name_q] |>
      unlist()
    options <- data_qa[i,] |>
      dplyr::select(contains(basename_aoptions)) |>
      unlist() |>
      na.omit()

    # script part to define new section
    form_script_s <- NULL
    if (i > 1) {
      if (diff(unlist(data_qa[, name_secno]))[i - 1] > 0){
        form_script_s <- sprintf(
            "  form.addPageBreakItem().setTitle('%s');\n",
            data_qa[i, name_sectitle]
          )
      }
    }

    # script part to define questions
    form_script_q <- switch(
      q_type,
      "short answer" = sprintf(
        "  form.addTextItem().setTitle('%s');",
        question
      ),
      "paragraph" = sprintf(
        "  form.addParagraphTextItem().setTitle('%s');",
        question
      ),
      "multiple choice"= paste(
        sprintf(
          "  var item = form.addMultipleChoiceItem();"
        ),
        sprintf(
          "  item.setTitle('%s');",
          question
        ),
        NULL,
        sep = "\n"),
      "dropdown" = paste(
        sprintf(
          "  var item = form.addListItem();"
        ),
        sprintf(
          "  item.setTitle('%s');",
          question
        ),
        NULL,
        sep = "\n")
    )

    # script part to define answer options
    form_script_a <- if(q_type %in% c("multiple choice", "dropdown")){
      paste(
        "  item.setChoices([",
        paste(
          sprintf("    item.createChoice('%s'),", options),
          collapse = "\n"
        ),
        "  ]);",
        NULL,
        sep = "\n"
      )
    } else {
      NULL
    }

    form_script_sqa <- paste(
      form_script_sqa,
      form_script_s,
      form_script_q,
      form_script_a,
      sep = "\n"
      )

  }

  # add destination folder on google drive
  form_script_save <- paste(
    sprintf("  var folderId = '%s';", gdrive_destfolder_id),
    "  var folder = DriveApp.getFolderById(folderId);",
    "  var file = DriveApp.getFileById(form.getId());",
    "  file.moveTo(folder);",
    NULL,
    sep = "\n"
  )

  # finalize google apps scripts
  form_script <- paste(
    form_script_begin,
    form_script_sqa,
    form_script_save,
    "}",
    sep = "\n"
    )
}

