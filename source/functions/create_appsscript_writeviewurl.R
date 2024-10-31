create_appsscript_writeviewurl <- function(
    form_id,
    gdrive_destfolder_id,
    name_outtxtfile
){
  if (!grepl("\\.txt", name_outtxtfile)){
    name_outtxtfile <- paste0(name_outtxtfile, ".txt")
  }
  form_script <- paste(
    "function writeFormUrlToFile() {",
    sprintf("  var formId = '%s';", form_id),
    sprintf("  var folderId = '%s';", gdrive_destfolder_id),
    sprintf("  var fileName = '%s';", name_outtxtfile),
    "  var form = FormApp.openById(formId);",
    "  var viewUrl = form.getPublishedUrl();",
    "  var content = viewUrl;",
    "  var folder = DriveApp.getFolderById(folderId);",
    "  var file = folder.createFile(fileName, content);",
    "}",
    NULL,
    sep = "\n"
  )
}
