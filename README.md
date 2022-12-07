# PL Visualizer

## Introduction
`PL Visualizer` is a Shiny app aimed to visualize advanced team statistics from the Premier League. This app scrapes squad and individual player data from [Fbref](https://fbref.com/en/comps/9/Premier-League-Stats) using the R package `rvest`. The Shiny app can be accessed here: https://brianjmpark.shinyapps.io/pl_visualizer/

## Explanation
`PL Visualizer` currently supports 2 main types of visualizations: **Team stats** and **Player stats**. 

For **team stats**, there are currently 5 types of visualizations:
* Goals scored vs. expected goals
* Non-penalty expected goals vs. goals conceded
* Goals conceded vs. shots on target against
* Goals conceded vs. post-shot expected goals conceded
* Non-penalty expected goals per shot vs. shots per 90

Selecting *Team Selection* from the drop-down menu highlights the squad in the scatterplot.

Changing user choices from the drop-down menus automatically refreshes the data table and the visualization.

Ticking the checkbox for 'Hide non-selected squad labels' de-clutters the plot by only highlighting the user-selected squad.

For **player stats**, the program draws 12 different individual statistics for the selected player and compares them to the league median for that player's position. These 12 statistics are: 

*Shots, Non-penalty expected goals, Assists, Expected assists, Progressive distance, Total passes attempted, Pass completion %, Shot creating actions, Tackles won, Tackles won %, Interceptions*, and *Clearances*. 

The program currently does not support goalkeepers' stats.

Caveat: the program currently does not support players who have played for multiple clubs (e.g., Daniel James have played for both Leeds and Fulham in the current season). 

## R dependencies
* `shiny`
* `rvest`
* `xml2`
* `dplyr`
* `stringr`
* `DT`
* `shinythemes`
* `RColorBrewer`
* `ggrepel`

## R sessionInfo()

R version 4.0.5 (2021-03-31)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.2.3

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:

[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:

[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:

 [1] DT_0.26           ggrepel_0.9.1     rvest_1.0.3       forcats_0.5.1     stringr_1.4.0     dplyr_1.0.5       purrr_0.3.4      
 [8] readr_1.4.0       tidyr_1.1.3       tibble_3.1.1      ggplot2_3.3.3     tidyverse_1.3.1   shinythemes_1.2.0 rsconnect_0.8.25 
[15] shiny_1.6.0      

loaded via a namespace (and not attached):

 [1] Rcpp_1.0.6          lubridate_1.7.10    packrat_0.7.0       assertthat_0.2.1    digest_0.6.27       utf8_1.2.1         
 [7] mime_0.10           R6_2.5.0            cellranger_1.1.0    backports_1.2.1     reprex_2.0.0        httr_1.4.2         
[13] pillar_1.6.0        rlang_1.0.6         curl_4.3            readxl_1.3.1        rstudioapi_0.13     jquerylib_0.1.4    
[19] labeling_0.4.2      selectr_0.4-2       htmlwidgets_1.5.3   munsell_0.5.0       broom_0.7.12        compiler_4.0.5     
[25] httpuv_1.6.0        modelr_0.1.8        askpass_1.1         pkgconfig_2.0.3     htmltools_0.5.3     openssl_1.4.3      
[31] tidyselect_1.1.0    fansi_0.4.2         crayon_1.4.1        dbplyr_2.1.1        withr_2.4.2         later_1.2.0        
[37] grid_4.0.5          jsonlite_1.7.2      xtable_1.8-4        gtable_0.3.0        lifecycle_1.0.0     DBI_1.1.1          
[43] magrittr_2.0.1      scales_1.1.1        stringi_1.5.3       cli_3.1.0           cachem_1.0.4        farver_2.1.0       
[49] fs_1.5.0            promises_1.2.0.1    xml2_1.3.2          bslib_0.4.0         ellipsis_0.3.1      generics_0.1.0     
[55] vctrs_0.3.7         tools_4.0.5         glue_1.4.2          hms_1.0.0           crosstalk_1.1.1     fastmap_1.1.0      
[61] yaml_2.2.1          colorspace_2.0-0    BiocManager_1.30.12 memoise_2.0.0       haven_2.4.1         sass_0.4.2 
