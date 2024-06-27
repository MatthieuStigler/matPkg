library(matPkg)
path_rscripts <- system.file("r_scripts_fake", package = "matPkg")

dir_dat <- mat_99_list_Rfiles(path_rscripts) |>
  dplyr::select(-folder)

dir_dat |>
  dplyr::count(has_error_parse, error_parse)


## run with internal
out_int <- mat_99_run_Rfiles(dir_dat) |>
  dplyr::select(filename, has_error, error, error_parse)

## run with external
out_ext <- mat_99_run_Rfiles(dir_dat, run_function = "external") |>
  dplyr::select(filename, has_error, error, error_parse)

## run with external and vanilla
out_ext <- mat_99_run_Rfiles(scripts_file = dir_dat, run_function = "external", run_cmd_vanilla = TRUE) |>
  dplyr::select(filename, has_error, error, error_parse)


## run with external and temp dir
dir_temp <- tempdir()
out_ext_dir <- mat_99_run_Rfiles(scripts_file = dir_dat, run_function = "external", tmp_dir = dir_temp) |>
  dplyr::select(filename, has_error, error, error_parse)
list.files(dir_temp)
unlink(dir_temp)

## check internals
out1 <- matPkg:::source_throw(dir_dat$full_path[[1]])
out2 <- matPkg:::source_rcmd_batch(dir_dat$full_path[[1]])

## compare outputs
out_int
out_ext

# test_that("999 output snapshot", {
#   expect_snapshot(dir_dat)
#   expect_snapshot(out)
# })

# testthat::snapshot_review(path="tests/testthat/_snaps")
# testthat::snapshot_review("tests/testthat/_snaps/999_source.new.md")
# testthat:::snapshot_meta(path="tests/testthat")

if(FALSE) {
  # testthat::snapshot_review()
  # devtools::test()
  # devtools::check()
}
