#' Convert a table to direct pdf
#'
#' Use tools::texi2pdf
#'@param x the table input
#'@param is_path_x whether x is a file on disk
#'@param filename output name
#'@param quiet passed to tools::texi2pdf
#'@param clean_tex,clean_rest Should remove other files, and tex too?
#'@param plus additional lattex stuff
#'@seealso  \code{\link{mat_pdf_to_png}}
#'@examples
#'\dontrun{
#'tab <-  data.frame(variable= c("A", "B"), stat = c("mean"), value = 1:2)
#'tab_xt <- print(xtable::xtable(tab), print.results = FALSE)
#'
#'mat_table_to_pdf(tab_xt, filename = "filename_test.pdf")
#'file.remove("filename_test.pdf")
#'## case 2: file is already on disk, use is_path_x
#'tmp_file <- tempfile()
#'print(xtable::xtable(tab), file = tmp_file)
#'mat_table_to_pdf(tmp_file, filename = "filename_test2.pdf", is_path_x = TRUE)
#'file.remove("filename_test2.pdf")
#'}
#'@export
mat_table_to_pdf <- function(x, filename = "input.pdf",  quiet=TRUE,
                             is_path_x = FALSE,
                             clean_tex = TRUE, clean_rest = TRUE,
                             plus= "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}") {

  if(inherits(x, "xtable") ) {
    warning("xtable object is actually not character, use print/capture.output")
    x <-  print(x)
  }
  if(!clean_tex | !clean_rest) warning("clean_rest or clean_tex not used/useful anymore")
  if(stringr::str_detect(filename, "\\.tex$"))  warning("Old code!?")
  if(!stringr::str_detect(filename, "\\.pdf$"))  warning("No pdf output?")

  ## read x if is path
  if(is_path_x) {
    x <- readLines(x)
  }

  ## format text
  ## format x if length(x) %>%  1
  if(length(x)>1) x <-  paste(x, collapse = "\n")
  first <- c("\\documentclass[varwidth=\\maxdimen]{standalone}[2011/12/21]",  "\\begin{document}")
  last <- "\\end{document}"
  text <- paste(first[1], plus, first[2], x, last, sep="\n")

  ## write down formatted file in temp file
  tmp_dir <- tempdir()
  tmp_file <- paste(tmp_dir, "file_temp.tex", sep = "/")

  fileConn <- file(tmp_file)
  writeLines(text, fileConn)
  close(fileConn)

  old_wd <- getwd()
  setwd(tmp_dir)
  if(!file.exists(tmp_file)) warning("Oups")
  try(tools::texi2pdf(tmp_file, quiet=quiet, clean=FALSE))
  setwd(old_wd)

  ## copy now
  file.copy(stringr::str_replace(tmp_file, "\\.tex$", ".pdf"),
            stringr::str_replace(filename, "\\.tex$", ".pdf"), overwrite = TRUE)


  if(any(c(clean_tex, clean_rest)) & FALSE) {
    filename_2  <- stringr::str_remove(filename, "\\..+$")
    all_files <- list.files(path = dirname(filename),
                            pattern=basename(filename_2),
                            full.names = TRUE)

    if(clean_rest) file.remove(all_files[!stringr::str_detect(all_files, "\\.pdf$|\\.tex$")])
    if(clean_tex) file.remove(all_files[stringr::str_detect(all_files, "\\.tex$")])
  }
}


mat_table_to_pdf_old <- function(x, filename = "input.tex",  quiet=TRUE,
                             clean_tex = TRUE, clean_rest = TRUE,
                             plus= "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}") {

  if(inherits(x, "xtable")) {
    warning("xtable object is actually not character, use print/capture.output")
    x <-  print(x)
  }
  if(stringr::str_detect(filename, "\\.pdf$|\\.png$")) warning("filneame should be raw")
  if(!stringr::str_detect(filename, "\\.tex"))   filename <- paste(filename, ".tex", sep="")

  ## format x if length(x) %>%  1
  if(length(x)>1) x <-  paste(x, collapse = "\n")

  first <- c("\\documentclass[varwidth=\\maxdimen]{standalone}[2011/12/21]",  "\\begin{document}")
  last <- "\\end{document}"
  text <- paste(first[1], plus, first[2], x, last, sep="\n")

  fileConn <- file(filename)
  writeLines(text, fileConn)
  close(fileConn)

  old_wd <- getwd()
  setwd(dirname(filename))
  if(!file.exists(basename(filename))) warning("Oups")
  try(tools::texi2pdf(basename(filename), quiet=quiet, clean=FALSE))
  setwd(old_wd)

  if(any(c(clean_tex, clean_rest))) {
    filename_2  <- stringr::str_remove(filename, "\\..+$")
    all_files <- list.files(path = dirname(filename),
                            pattern=basename(filename_2),
                            full.names = TRUE)

    if(clean_rest) file.remove(all_files[!stringr::str_detect(all_files, "\\.pdf$|\\.tex$")])
    if(clean_tex) file.remove(all_files[stringr::str_detect(all_files, "\\.tex$")])
  }
}

#' @export
#' @rdname mat_table_to_pdf
table_to_pdf <-  function(x) .Deprecated("mat_table_to_pdf")

#' Convert pdf to png
#'
#' @param path path of file
#' @param density,quality parameters to convert
#' @param echo should show command?
#' @export
#' @examples
#'   pdf("try.pdf")
#'   plot(1, col = "green")
#'   dev.off()
#'   mat_pdf_to_png("try.pdf")
#'   file.remove(c("try.pdf", "try.png"))

mat_pdf_to_png <-  function(path, density = 150, quality = 90, echo=TRUE) {
  if(!file.exists(path)) stop("File not found")
  if(!stringr::str_detect(path, "\\.pdf")) stop("File not pdf?")
  path_out <- stringr::str_replace(path, "\\.pdf", ".png")
  cmd <-  paste("convert -flatten -density",  density, path,  "-quality",  quality,  path_out)
  if(echo) print(cmd)
  system(cmd)

}

if(FALSE) {
  pdf("try.pdf")
  plot(1, col = "green")
  dev.off()
  mat_pdf_to_png("try.pdf")
  file.remove(c("try.pdf", "try.png"))
}
