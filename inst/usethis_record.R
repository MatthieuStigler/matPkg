

## from: https://github.com/r-lib/usethis

library(usethis)
path <- "/home/matifou/gitReps/my_github/matPkg"
# create_package(path)


use_package("tidyr", "Imports")
use_package("broom", "Imports")
use_package("dplyr", "Imports")
use_package("ggplot2", "Imports")
use_package("magrittr", "Imports")
use_package("tibble", "Imports")
use_package("tools", "Imports")
use_package("purrr", "Imports")
use_package("rlang", "Imports")
use_package("tidyselect", "Imports")
use_package("stringr", "Imports")
use_package("nlme", "Imports")
use_package("raster", "Imports")
use_package("sf", "Imports")
use_package("rmarkdown", "Imports")
use_package("knitr", "Imports")
use_package("data.table", "Imports")
use_package("methods", "Imports")

## for the example
# use_package("tidyverse", "Suggests")

use_roxygen_md()

use_readme_md()


use_git()

use_mit_license("matPkg")

use_build_ignore("data_raw", escape = TRUE)
