create_appsscript_sendemail <- function(
    data_email
){
  script_email <- c()
  for (i in seq_along(data_email$viewurl)) {
    script_email <- paste(
      script_email,
      sprintf(
        "MailApp.sendEmail('%s', '%s', '%s');",
        data_email$expert_emailaddress[i],
        data_email$email_subjectline[i],
        data_email$email_body[i]
      ),
      NULL,
      sep = "\n"
    )
  }
  paste(
    "function sendEmail() {",
    script_email,
    "}",
    NULL,
    sep = "\n"
  )
}
