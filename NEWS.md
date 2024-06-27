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
