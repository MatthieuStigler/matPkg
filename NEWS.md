# matPkg 0.2.7

on 2026-10-16

* Replace package pryr (archived) with lobstr, functions are almost the same
* Update roxygen

# matPkg 0.2.62

on 2026-02-08

* mat_join_compare(): allow now comparisons across character/mixed variables, correct small bug


# matPkg 0.2.61

on 2026-02-03

* add new argument `check_numbers` in `mat_99_list_Rfiles()` to handle numbers such as 5_10 and warn if same as 5_1
* Fix bad time info with 999

# matPkg 0.2.60

on 2024-07-04

* add new argument `tools` in mat_pdf_to_png to allow to use pdftoppm


# matPkg 0.2.50

* add new argument `cmd_vanilla` in `source_rcmd_batch` to run with vanilla
* update doc with roxy 7.3.1

# matPkg 0.2.46

* fix issue with YAML header parsing

# matPkg 0.2.3

- New mat_print_tib

# matPkg 0.2.0.9008

* Important change: use `new.env(parent = .GlobalEnv)` instead of `new.env()`. Hopefully this will solve problems such as *simpleError in as.double(y): cannot coerce type 'S4' to vector of type 'double'*
* Added a `NEWS.md` file to track changes to the package.
