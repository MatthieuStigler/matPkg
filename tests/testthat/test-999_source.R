library(matPkg)
path_rscripts <- system.file("r_scripts_fake", package = "matPkg")

dir_dat <- mat_99_list_Rfiles(path_rscripts)
out <- mat_99_run_Rfiles(dir_dat) |>
  dplyr::select(filename, has_error, error, error_parse)


test_that("999 output snapshot", {
  expect_snapshot(dir_dat)
  expect_snapshot(out)
})

