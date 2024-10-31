function writeFormUrlToFile() {
  var formId = '1CvgZkAsNZYK1EglepnW8jzLvhAj00IGkYeoi8uOCqlw';
  var folderId = '1INTfoS4vLtt4QmrwWLjhV43UoWjo8aix';
  var fileName = 'questionnaire_test_viewurl.txt';
  var form = FormApp.openById(formId);
  var viewUrl = form.getPublishedUrl();
  var content = viewUrl;
  var folder = DriveApp.getFolderById(folderId);
  var file = folder.createFile(fileName, content);
}

