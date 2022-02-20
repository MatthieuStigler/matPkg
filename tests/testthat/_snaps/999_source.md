# 999 output snapshot

    Code
      dir_dat
    Output
      # A tibble: 9 x 13
        filename    ext   folder subfolder number_char number yaml         error_parse
        <chr>       <chr> <chr>  <chr>     <chr>        <dbl> <list>       <chr>      
      1 1_1_light_~ R     /home~ r_script~ 1_1            1.1 <named list> <NA>       
      2 1_2_heavy_~ R     /home~ r_script~ 1_2            1.2 <named list> <NA>       
      3 1_3_light_~ R     /home~ r_script~ 1_3            1.3 <named list> <NA>       
      4 1_4_sleep_~ R     /home~ r_script~ 1_4            1.4 <named list> <NA>       
      5 1_5_SUPER_~ R     /home~ r_script~ 1_5            1.5 <named list> <NA>       
      6 1_6_load_p~ R     /home~ r_script~ 1_6            1.6 <named list> <NA>       
      7 1_7_messag~ R     /home~ r_script~ 1_7            1.7 <named list> <NA>       
      8 1_8_parse_~ R     /home~ r_script~ 1_8            1.8 <NULL>       <text>:13:~
      9 1_9_no_yam~ R     /home~ r_script~ 1_9            1.9 <list [0]>   <NA>       
      # ... with 5 more variables: has_error_parse <lgl>, has_yaml <lgl>,
      #   has_runMat <lgl>, runMat_val <lgl>, full_path <chr>

---

    Code
      out
    Output
      # A tibble: 5 x 4
        filename               has_error error                             error_parse
        <chr>                  <lgl>     <chr>                             <chr>      
      1 1_1_light_one_rLo.R    FALSE     <NA>                              <NA>       
      2 1_2_heavy_one_rLoH.R   FALSE     <NA>                              <NA>       
      3 1_3_light_error_rLoE.R TRUE      Can't subset columns that don't ~ <NA>       
      4 1_4_sleep_3_rLoS.R     FALSE     <NA>                              <NA>       
      5 1_6_load_package.R     FALSE     <NA>                              <NA>       

