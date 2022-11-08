library(matPkg)

data(iris_tb)

### Standard
way_1 <- iris_tb |>
  tidyr::nest(data=-Species) |>
  mat_lm_means_tidy(Petal.Width)

## Check weights
way_2 <- iris_tb |>
  tidyr::nest(data = everything()) |>
  mat_lm_means_tidy(Petal.Width, weight = rep(c(1, 0), c(50, 100)))


test_that("Weights passed to lm work", {
  expect_equal(way_1[1,] |> select(-n, -Species), way_2 |> select(-n))
})

