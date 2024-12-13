create_appsscript_sendemail <- function(
    data_email,
    test = FALSE,
    test_address = NULL
){
  script_email <- c()
  for (i in seq_along(data_email$expert_emailaddress)) {
    script_email <- paste(
      script_email,
      sprintf(
        "MailApp.sendEmail('%s', '%s', '%s');",
        ifelse(test, test_address, data_email$expert_emailaddress[i]),
        data_email$email_subjectline[i],
        paste0(
          ifelse(test,
                 paste0(
                   'test mail to ',
                   data_email$expert_emailaddress[i],
                   '\\n',
                   paste0(rep('-', 20), collapse = "-"),
                   '\\n'
                   ),
                 ""
          ),
          data_email$email_body[i]
        )
      ),
      NULL,
      sep = "\n"
    )
  }
  paste(
    paste0(
      "function ",
      ifelse(test, "sendEmailTest", "sendEmail")
      ,"() {"
      ),
    script_email,
    "}",
    NULL,
    sep = "\n"
  )
}
