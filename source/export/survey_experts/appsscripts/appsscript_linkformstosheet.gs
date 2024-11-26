function linkFormsToSheet() {
var formIdList = (['1DNb-WZqe8V9XtR-Q7bs6td6mZkhbC_gNZd4I6P4EKOM','1JrZ_kUEeVgLEp5OccBzkP7iVsY1UmT3jKlNfF_z0B_c','1QU2NuOvEpKPjiRaa4syt4uTw9NsiEq0zxlo0U--rfzQ','1kC5GK0UjyPSGotm__WFw3gAS56IFL4p1t8a-VU5t19A']);
var folderId = '1Esk5X5J9YKgtZdS4iNQ8g1jpemG6uGcR';
var fileName = 'bevraging_test_responses';
var folder = DriveApp.getFolderById(folderId);
var spreadsheet = SpreadsheetApp.create(fileName);
var sheetId = spreadsheet.getId();
var file = DriveApp.getFileById(sheetId);
file.moveTo(folder);
for (var i = 0; i < formIdList.length; i++) {
var formId = formIdList[i];
var form = FormApp.openById(formId);
form.setDestination(FormApp.DestinationType.SPREADSHEET, sheetId);
}
}

