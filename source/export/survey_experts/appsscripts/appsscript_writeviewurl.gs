function writeFormUrlToFile() {
var formIdList = (['1Kjsq8H3x-_QLGaMSMFJDTdgjNEWgj4bxv7icnbSHd6c','1sprA4v611F1eP23hUjvCcTG8dg_jrM77VJRYrVi6wxc','130zfqtHYygIoLD1qmQWlUD67iMYFj6a8R5AkQgwV670','1XlhGy6YdLQUpd9ojuPBLG8ZpszjaqKaAKYhGD03wEaE','18sk5o2on75yhvRl2KgLiRu2XHHAfklswnQmZe8pi9po','1QEUbkwSJRnsiisLurxioJK0_iFnIKMqu_OOgWdXe_2M','1lCA_3DllvA_vHS-XZ4CwA95WwMOGhN60_GJHmHqTOkU','1xlPqsVpVzIep5pAoAziBkzHrYIarWw4BqoFspzaq0Tw','1G76kbqxXwdCnwv1JyHUAzxW1bdXHsl1Yt1TVSPfoGhg','18Hi2dMz9ieV8oQbqWNRRDKFRJcbmBaGh1KLIO2Rmlek']);
var folderId = '1GPNJGmDoaaT4P2U51cUkWBQKR8cvtRnw';
var fileName = 'survey_viewurls';
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

