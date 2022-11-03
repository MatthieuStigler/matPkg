library(matPkg)
path_rscripts <- system.file("r_scripts_fake", package = "matPkg")

dir_dat <- mat_99_list_Rfiles(path_rscripts) |>
  dplyr::select(-folder)

## run witn internal
out_int <- mat_99_run_Rfiles(dir_dat) |>
  dplyr::select(filename, has_error, error, error_parse)

## run with external
out_ext <- mat_99_run_Rfiles(dir_dat, run_function = "external") |>
  dplyr::select(filename, has_error, error, error_parse)

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
