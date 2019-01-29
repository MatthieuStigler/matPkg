#' Convert a table to direct pdf
#'@export
mat_table_to_pdf <- function(x, filename = "input.tex", clean=TRUE, quiet=TRUE,
                         plus= "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}") {

  if(inherits(x, "xtable")) {
    warning("xtable object is actually not chracter, use print")
    x <-  print(x)
  }
  if(stringr::str_detect(filename, "\\.pdf$|\\.png$")) warning("filneame should be raw")
  if(!stringr::str_detect(filename, "\\.tex"))   filename <- paste(filename, ".tex", sep="")

  first <- c("\\documentclass[varwidth=\\maxdimen]{standalone}[2011/12/21]",  "\\begin{document}")
  last <- "\\end{document}"
  text <- paste(first[1], plus, first[2], x, last, sep="\n")

  fileConn <- file(filename)
  writeLines(text, fileConn)
  close(fileConn)

  old_wd <- getwd()
  setwd(dirname(filename))
  if(!file.exists(basename(filename))) warning("Oups")
  try(tools::texi2pdf(basename(filename), quiet=quiet, clean=clean))
  setwd(old_wd)

  if(clean) {
    filename_2  <- stringr::str_remove(filename, "\\..+$")
    all_files <- list.files(path = dirname(filename),
                            pattern=basename(filename_2),
                            full.names = TRUE)
    # all_files <- list.files(pattern=filename_2)
    all_files_rem <- all_files[!stringr::str_detect(all_files, "\\.pdf")]
    file.remove(all_files_rem)
  }
}

#' @export
table_to_pdf <-  function(x) .Deprecated("mat_table_to_pdf")
