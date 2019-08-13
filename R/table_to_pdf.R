#' Convert a table to direct pdf
#'
#' Use tools::texi2pdf
#'@param x the table input
#'@param filename output name
#'@param clean,quiet passed to tools::texi2pdf
#'@param plus additional lattex stuff
#'@examples
#'\dontrun{
#'tab <-  data.frame(variable= c("A", "B"), stat = c("mean"), value = 1:2)
#'tab_xt <- print(xtable::xtable(tab), print.results = FALSE)
#'
#'mat_table_to_pdf(tab_xt, filename = "filename_test")
#'file.remove("filename_test.pdf")
#'}
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
#' @rdname mat_table_to_pdf
table_to_pdf <-  function(x) .Deprecated("mat_table_to_pdf")


# mat_pdf_to_png <-  function(path) {
#   if(!file.exists(path)) stop("File not found")
#   if(!str_detect(path, "\\.pdf")) stop("File not pdf?")
#   file_out <-
#   mat_list_dir(getwd(), pattern = "pdf", recursive = FALSE) %>%
#     mutate(path_out = paste("convert -flatten-density 150", full_path,  "-quality 90",  str_replace(full_path, "pdf", "png")))%$%
#     map(path_out, system)
# }
