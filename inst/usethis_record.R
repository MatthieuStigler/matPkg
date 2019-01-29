

## from: https://github.com/r-lib/usethis

library(usethis)
path <- "/home/matifou/gitReps/my_github/matPkg"
# create_package(path)


use_package("tidyr", "Imports")
use_package("broom", "Imports")
use_package("dplyr", "Imports")
use_package("ggplot2", "Imports")
use_package("magrittr", "Imports")

use_roxygen_md()

use_readme_md()
