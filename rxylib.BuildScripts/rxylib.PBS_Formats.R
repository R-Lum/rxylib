### ===============================================================================================
### R package rxylib BUILDSCRIPTS
### Formats
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-06-29
### ===============================================================================================

## this script adds all allowed formats to the produced package manual

  ##get format list
  formats <- rxylib:::get_supportedFormats()

  ##create data frame
  header <- "\\tabular{llll}{\\bold{ID} \\tab \\bold{NAME} \\tab \\bold{DESCRIPTION} \\tab \\bold{FILE EXTENSION} \\cr"
  footer <- " \\tab \\tab }"
  main <- vapply(1:length(formats$name), function(x){
    paste0(
      "[",x,",]"," \\tab ",
      formats$name[x],
      " \\tab ",
      formats$desc[x],
      " \\tab ",
      formats$exts[x],
      "\\cr"
    )

  }, vector(mode = "character", length = 1))

  ##table
  table <- c(header, main, footer)

  ##read RD file
  file <- readLines("man/rxylib-package.Rd")
  insert_id <- grep(pattern = "Supported data formats", file, fixed = TRUE)

  ##output
  writeLines(
    text = c(file[1:insert_id], table, file[(insert_id + 1):length(file)]),
    con = "man/rxylib-package.Rd")

