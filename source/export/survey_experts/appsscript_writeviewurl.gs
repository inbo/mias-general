function writeFormUrlToFile() {
var formIdList = (['1DNb-WZqe8V9XtR-Q7bs6td6mZkhbC_gNZd4I6P4EKOM','1JrZ_kUEeVgLEp5OccBzkP7iVsY1UmT3jKlNfF_z0B_c','1QU2NuOvEpKPjiRaa4syt4uTw9NsiEq0zxlo0U--rfzQ','1kC5GK0UjyPSGotm__WFw3gAS56IFL4p1t8a-VU5t19A']);
var folderId = '1p6eClyAGP_DUeSuKPmzeOyY05c8f5OKu';
var fileName = 'bevraging_viewurls';
var fileContent = [['formid', 'formtitle', 'viewurl']];
for (var i = 0; i < formIdList.length; i++) {
var formId = formIdList[i];
var form = FormApp.openById(formId);
var formTitle = DriveApp.getFileById(formId).getName();
var viewUrl = form.getPublishedUrl();
fileContent[i+1] = [formId, formTitle, viewUrl];
}
var folder = DriveApp.getFolderById(folderId);
var spreadsheet = SpreadsheetApp.create(fileName);
var file = DriveApp.getFileById(spreadsheet.getId());
file.moveTo(folder);
var sheet = spreadsheet.getSheets()[0];
sheet.getRange(1, 1, fileContent.length, fileContent[0].length).setValues(fileContent);
}

