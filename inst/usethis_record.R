library(usethis)

## Incremental version update:
usethis::use_version(which = "dev")

## news file
usethis::use_news_md(open = rlang::is_interactive())

## from: https://github.com/r-lib/usethis

path <- "/home/matifou/gitReps/my_github/matPkg"
# create_package(path)


use_package("tidyr", "Imports", min_version = "0.8.31")
use_package("broom", "Imports")
use_package("dplyr", "Imports")
use_package("ggplot2", "Imports")
use_package("magrittr", "Imports")
use_package("tibble", "Imports")
use_package("tools", "Imports")
use_package("purrr", "Imports")
use_package("rlang", "Imports", min_version = "1.0.0") # for prefix argument of rlang::cnd_message
use_package("tidyselect", "Imports")
use_package("stringr", "Imports")
use_package("nlme", "Imports")
use_package("raster", "Imports")
use_package("sf", "Imports")
use_package("rmarkdown", "Imports")
use_package("knitr", "Imports")
use_package("data.table", "Imports")
use_package("methods", "Imports")
use_package("lmtest", "Imports")
use_package("pryr", "Imports")
use_package("lmtest", "Imports")
# use_package("utils", "Imports")
use_package("readr", "Imports")
use_package("pkgload", "Imports")

## for the example
use_package("sandwich", "Suggests")



## Initial commands
# use_roxygen_md()
# use_readme_md()
# use_git()
# use_mit_license("matPkg")
# use_build_ignore("data_raw", escape = TRUE)
# use_testthat()
