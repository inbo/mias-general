rm(list = ls())
#
# -------------------------------------------------------------------------
#
# files list from local copy of (forked) occ-cube-alien repo
files_source <- list.files(
  "../../github_INBO/occ-cube-alien/src/belgium",
  full.names = TRUE
)
#
# destination files
files_destination <- lapply(
  files_source,
  function(x){
    paste0(
      "source/gbif_occcubes/",
      x |> basename() |> gsub(pattern = ".Rmd", replacement = "", x = _),
      ".R"
    )
  }
)
#
# deal with duplicate chunk names in source rmd files
options(knitr.duplicate.label = 'allow')
#
# save destination files as r scripts
for (i in seq_along(files_source)){
  knitr::purl(
    input = files_source[[i]],
    output = files_destination[[i]],
    documentation = 2
  )
}

