#' Convert a table to direct pdf
#'
#' Use tools::texi2pdf
#'@param x the table input
#'@param is_path_x whether x is a file on disk
#'@param filename output name
#'@param quiet passed to tools::texi2pdf
#'@param clean_tex,clean_rest Should remove other files, and tex too?
#'@param plus additional LaTeX stuff
#'@param copy.mode Argument passed to \code{\link{file.copy}} to override or not permissions
#'@seealso  \code{\link{mat_pdf_to_png}}, \code{\link{mat_tex_to_png}}
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
mat_table_to_pdf <- function(x, filename = NULL,  quiet=TRUE,
                             is_path_x = FALSE,
                             clean_tex = TRUE, clean_rest = TRUE,
                             copy.mode = TRUE,
                             plus= "\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}") {

  if(inherits(x, "xtable") ) {
    warning("xtable object is actually not character, use print/capture.output")
    x <-  print(x)
  }
  if(!clean_tex | !clean_rest) warning("clean_rest or clean_tex not used/useful anymore")

  ## Filename default
  if(is.null(filename)){
    if(is_path_x) {
      filename <- str_replace(x, "\\.tex$", ".pdf")
    } else {
      filename <- "input.pdf"
    }
  }
  if(stringr::str_detect(filename, "\\.tex$"))  warning("Old code!?")
  if(!stringr::str_detect(filename, "\\.pdf$"))  warning("'filename' does not contain 'pdf' extension?")

  ## read x if is path
  if(is_path_x) {
    x <- readLines(x)
  }

  ## Check for problems
  if(any(str_detect(x, "usepackage"))) {
    detect_pkg <- str_extract(x, "(?<=usepackage\\{).+(?=\\})") %>%
      purrr::discard(is.na)
    warning(paste("Document should not contain calls with 'usepackage', found:", paste(detect_pkg, collapse = ", ")))
  }
  if(any(str_detect(x, "begin\\{+document"))) warning("Document should not contain calls with 'begin{document'")

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
            stringr::str_replace(filename, "\\.tex$", ".pdf"),
            overwrite = TRUE, copy.mode = copy.mode)


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
#' @param correct_grayscale Should correct for \code{RGB color space not permitted on grayscale}
#' @param echo should show command?
#' @export
#' @seealso \code{\link{mat_table_to_pdf}}, \code{\link{mat_tex_to_png}}
#' @examples
#' tmp <- file.path(tempdir(), "try.pdf")
#' pdf(tmp)
#' plot(1, col = "green")
#' dev.off()
#' mat_pdf_to_png(tmp)
#'
#' ## use `correct_grayscale = TRUE` for message "RGB color space not permitted on grayscale"
#' pdf(tmp)
#' plot(1)
#' dev.off()
#' mat_pdf_to_png(tmp, correct_grayscale = TRUE)
mat_pdf_to_png <-  function(path, density = 150, quality = 90, echo=TRUE,
                            correct_grayscale = FALSE) {
  if(!file.exists(path)) stop("File not found")
  if(!stringr::str_detect(path, "\\.pdf")) stop("File not pdf?")
  path_out <- stringr::str_replace(path, "\\.pdf", ".png")
  colorspace <- if(correct_grayscale) "-colorspace RGB" else NULL
  cmd <-  paste("convert -flatten -density",  density, colorspace, path,  "-quality",  quality,  path_out)
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


################################
#'## tex to pdf
################################


#' Convert tex to png
#'
#' This is a wrapper for mat_table_to_pdf and mat_pdf_to_png
#'
#' @param path Path for the .tex on disk
#' @param path_out Where to save .png
#' @param correct_grayscale,... Arguments passed to \code{\link{mat_pdf_to_png}}
#'
#'@seealso  \code{\link{mat_table_to_pdf}}, \code{\link{mat_pdf_to_png}}
#'@examples
#'\dontrun{
#'tab <-  data.frame(variable= c("A", "B"), stat = c("mean"), value = 1:2)
#'tab_xt <- print(xtable::xtable(tab), print.results = FALSE)
#'
#'##
#'tmp_file <- tempfile()
#'print(xtable::xtable(tab), file = tmp_file)
#'mat_tex_to_png(tmp_file, "table_as_image.png")
#'file.remove("table_as_image")
#'}
#'@export
mat_tex_to_png <- function(path, path_out = gsub("\\.tex$", ".png", path),
                           correct_grayscale = TRUE, ...){

  ## tex to pdf, pdf in temp
  tmp_file_pdf <- paste0(tempfile(), ".pdf")
  tmp_file_png <- gsub("\\.pdf$", ".png", tmp_file_pdf)

  mat_table_to_pdf(x=path, is_path_x = TRUE, filename=tmp_file_pdf)

  ## now pdf to png
  mat_pdf_to_png(tmp_file_pdf, correct_grayscale=correct_grayscale)
  file.copy(tmp_file_png, path_out)

  ## remove files
  silent <- file.remove(c(tmp_file_png, tmp_file_pdf))
}
