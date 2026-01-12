create_appsscript_linkformstosheet <- function(
    form_ids,
    gdrive_destfolder_id,
    name_outfile
){
  paste(
    "function linkFormsToSheet() {",
    sprintf(
    "var formIdList = (['%s']);",
    paste(form_ids, collapse = "','")
    ),
    sprintf("var folderId = '%s';", gdrive_destfolder_id),
    sprintf("var fileName = '%s';", name_outfile),
    "var folder = DriveApp.getFolderById(folderId);",
    "var spreadsheet = SpreadsheetApp.create(fileName);",
    "var sheetId = spreadsheet.getId();",
    "var file = DriveApp.getFileById(sheetId);",
    "file.moveTo(folder);",
    "for (var i = 0; i < formIdList.length; i++) {",
    "var formId = formIdList[i];",
    "var form = FormApp.openById(formId);",
    "form.setDestination(FormApp.DestinationType.SPREADSHEET, sheetId);",
    "}",
    "}",
    NULL,
    sep = "\n"
  )
}
