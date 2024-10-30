create_appsscript_dropdownupd <- function(
    form_id,
    question_text,
    aoptions_upd
    ){

  form_script_loop <- paste(
    "function updateDropdown() {",
    sprintf("  var form = FormApp.openById('%s');", form_id),
    "  var items = form.getItems();",
     "  for (var i = 0; i < items.length; i++) {",
    "    var item = items[i];",
    "    if (item.getType() === FormApp.ItemType.LIST) {",
    sprintf("      if (item.getTitle() === '%s') {", question_text),
    "        var itemUpd = item;",
    "      }",
    "    }",
    "  }",
    NULL,
    sep = "\n"
  )

  aoptions_coll <- paste(aoptions_upd, collapse = "','")
  form_script_upd <- paste(
    sprintf("  itemUpd.asListItem().setChoiceValues(['%s']);", aoptions_coll),
    "}",
    NULL,
    sep = "\n"
  )


  form_script <- paste(
    form_script_loop,
    form_script_upd,
    NULL,
    sep = "\n"
  )

  }
