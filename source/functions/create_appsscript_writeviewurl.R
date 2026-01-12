create_appsscript_writeviewurl <- function(
    form_ids,
    gdrive_destfolder_id,
    name_outfile
){
  paste(
    "function writeFormUrlToFile() {",
    sprintf(
    "var formIdList = (['%s']);",
    paste(form_ids, collapse = "','")
    ),
    sprintf("var folderId = '%s';", gdrive_destfolder_id),
    sprintf("var fileName = '%s';", name_outfile),
    "var fileContent = [['formid', 'formtitle', 'viewurl']];",
    "for (var i = 0; i < formIdList.length; i++) {",
    "var formId = formIdList[i];",
    "var form = FormApp.openById(formId);",
    "var formTitle = DriveApp.getFileById(formId).getName();",
    "var viewUrl = form.getPublishedUrl();",
    "fileContent[i+1] = [formId, formTitle, viewUrl];",
    "}",
    "var folder = DriveApp.getFolderById(folderId);",
    "var spreadsheet = SpreadsheetApp.create(fileName);",
    "var file = DriveApp.getFileById(spreadsheet.getId());",
    "file.moveTo(folder);",
    "var sheet = spreadsheet.getSheets()[0];",
    "sheet.getRange(1, 1, fileContent.length, fileContent[0].length).setValues(fileContent);",
    "}",
    NULL,
    sep = "\n"
  )
}
