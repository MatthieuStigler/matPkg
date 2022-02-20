library(matPkg)
path_rscripts <- system.file("r_scripts_fake", package = "matPkg")

dir_dat <- mat_99_list_Rfiles(path_rscripts) %>%
  dplyr::select(-folder)
out <- mat_99_run_Rfiles(dir_dat) |>
  dplyr::select(filename, has_error, error, error_parse)


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
