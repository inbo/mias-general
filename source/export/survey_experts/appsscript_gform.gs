function createForm() {
  var form = FormApp.create('questionnaire_test');


  var item = form.addListItem();
  item.setTitle('Which species are you reporting for?');

  item.setChoices([
    item.createChoice('[IAS list to be updated]'),
  ]);

  var item = form.addMultipleChoiceItem();
  item.setTitle('Is the species currently present or absent in Vlanders?');

  item.setChoices([
    item.createChoice('present'),
    item.createChoice('absent'),
  ]);

  form.addParagraphTextItem().setTitle('Explain briefly.');

  var folderId = '1INTfoS4vLtt4QmrwWLjhV43UoWjo8aix';
  var folder = DriveApp.getFolderById(folderId);
  var file = DriveApp.getFileById(form.getId());
  file.moveTo(folder);

}
