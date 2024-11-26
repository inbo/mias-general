create_overview_gform <- function(
    data_qa,
    name_qtype,
    name_q,
    name_qexpl,
    name_qexplfu,
    basename_aoptions,
    name_secno,
    name_sectitle,
    path_section_template_rmd
){

  # read template rmd
  rmd_template <- readLines(con = path_section_template_rmd)

  for (i in seq_along(data_qa[, name_q] |> unlist())){
    #
    # get s & q & a information
    s_tmp <- data_qa[i, name_sectitle] |> unlist()
    s_title <- if (!is.na(s_tmp)) {paste("#", s_tmp)} else {""}
    if (i > 1) {
      if (diff(unlist(data_qa[, name_secno]))[i - 1] == 0){
        s_title <- ""
      }
    }
    last_line <- "\\\\newpage"
    if (i < nrow(data_qa)) {
    if (diff(unlist(data_qa[, name_secno]))[i] == 0){
      last_line <- ""
    }
    }
    q_type <- data_qa[i, name_qtype] |> unlist()
    q_text <- data_qa[i, name_q] |> unlist()
    q_tmp <- data_qa[i, name_qexpl] |> unlist()
    q_expl <- if (!is.na(q_tmp)) {q_tmp} else {""}
    a_tmp <- data_qa[i,] |>
      dplyr::select(dplyr::starts_with(basename_aoptions)) |>
      unlist() |>
      na.omit()
    a_options <- if (length(a_tmp > 0)){
      paste("-", a_tmp, collapse = "\n")
    } else {
      ""
    }
    q_tmp <- data_qa[i, name_qexplfu] |> unlist()
    q_expl_fu <- if (!is.na(q_tmp)) {q_tmp} else {""}
    #
    # update section rmd
    rmd_upd <- rmd_template |>
      gsub(pattern = "# section title", replacement = s_title) |>
      gsub(pattern = "question text", replacement = q_text) |>
      gsub(pattern = "response options", replacement = a_options) |>
      gsub(pattern = "question explanation follow-up", replacement = q_expl_fu) |>
      gsub(pattern = "last line", replacement = last_line)
    rmd_upd <- rmd_upd |>
      gsub(pattern = "question explanation", replacement = q_expl)
    rmd_upd <- rmd_upd |>
      gsub(pattern = "\\*\\*", replacement = "")
    #
    # write section rmd
    writeLines(
      text = rmd_upd,
      con = paste0(path_section_template_rmd |> dirname(), "/sections/section_", i, ".Rmd")
      )
  }
}

