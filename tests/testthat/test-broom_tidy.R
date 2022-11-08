library(matPkg)

data(iris_tb)

environment(mat_lm_means_tidy) <- environment(mat_check_0row)

### Standard
way_1 <- iris_tb |>
  tidyr::nest(data=-Species) |>
  mat_lm_means_tidy(Petal.Width)

## Check weights
way_2 <- iris_tb |>
  tidyr::nest(data = everything()) |>
  mat_lm_means_tidy(Petal.Width, weight = rep(c(1, 0), c(50, 100)))


test_that("Weights passed to lm work", {
  expect_equal(way_1[1,] |> dplyr::select(-n, -Species), way_2 |> dplyr::select(-n))
})

## variable is inside: does not work!
way_3 <- iris_tb |>
  mutate(weight = rep(c(1, 0), c(50, 100))) %>%
  tidyr::nest(data = everything()) |>
  mat_lm_means_tidy(Petal.Width, weight = weight)

test_that("Fine if in data", {
  expect_equal(way_3, way_2)
})

test_that("But doesnt work if not called weight...", {
  expect_error(iris_tb |>
                 mutate(weight_var = rep(c(1, 0), c(50, 100))) %>%
                 tidyr::nest(data = everything()) |>
                 mat_lm_means_tidy(Petal.Width, weight = weight_var)
  )
})

