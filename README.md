# PL Visualizer

## Introduction
`PL Visualizer` is a Shiny app aimed to visualize advanced team statistics from the Premier League. This app scrapes squad and individual player data from [Fbref](https://fbref.com/en/comps/9/Premier-League-Stats) using the R package `rvest`. The Shiny app can be accessed here: https://brianjmpark.shinyapps.io/pl_visualizer/

The web app has now been updated according to the new Premier League campaign! 

## Explanation
`PL Visualizer` currently supports 3 main types of visualizations: **Team stats**, **Outfield player stats**, and **Goalkeeper stats**. 

For **team stats**, there are currently 5 types of visualizations:
* Goals scored vs. expected goals
* Non-penalty expected goals vs. goals conceded
* Goals conceded vs. shots on target against
* Goals conceded vs. post-shot expected goals conceded
* Non-penalty expected goals per shot vs. shots per 90

Selecting *Team Selection* from the drop-down menu highlights the squad in the scatterplot.

Changing user choices from the drop-down menus automatically refreshes the data table and the visualization.

Ticking the checkbox for 'Hide non-selected squad labels' de-clutters the plot by only highlighting the user-selected squad.

For **Outfield player stats**, the program draws 17 different individual statistics for the selected player and calculates the percentile against other players in the same position. These 17 statistics are: 

*Shots, Non-penalty expected goals, Assists, Expected assists, Progressive distance, Total distance, Total passes attempted, Pass completion %, Progressive passes, Passes into the penalty area, Goal creating actions, Shot creating actions, Tackles won, Tackles won %, Interceptions*, and *Clearances*. 

Note this tab does not support or include goalkeepers.

Caveat: the program currently does not support players who have played for multiple clubs (e.g., Daniel James have played for both Leeds and Fulham in the '21-'22 season). 

The **Goalkeeper stats** tab currently shows two visualizations:
* Post-shot expected goals differential (PsXG - GA) versus save percentage
* Post-shot expected goals per shots on target (PsXG/SoT) versus save percentage

## R dependencies
* `shiny`
* `rvest`
* `xml2`
* `dplyr`, `purrr`, `stringr`
* `DT`
* `bslib`
* `RColorBrewer`
* `ggplot`
* `ggrepel`
* `shinycssloaders`

## R sessionInfo()
``` r
R version 4.3.1 (2023-06-16)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Big Sur 11.2.3

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/C/en_US.UTF-8

time zone: America/Toronto
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] RColorBrewer_1.1-3    xml2_1.3.4            DT_0.28               ggrepel_0.9.3        
 [5] rvest_1.0.3           lubridate_1.9.2       forcats_1.0.0         stringr_1.5.0        
 [9] dplyr_1.1.2           purrr_1.0.1           readr_2.1.4           tidyr_1.3.0          
[13] tibble_3.2.1          ggplot2_3.4.2         tidyverse_2.0.0       magrittr_2.0.3       
[17] shinycssloaders_1.0.0 bslib_0.5.0           rsconnect_1.0.1       shiny_1.7.4          

loaded via a namespace (and not attached):
 [1] gtable_0.3.3      htmlwidgets_1.6.2 tzdb_0.4.0        vctrs_0.6.3      
 [5] tools_4.3.1       crosstalk_1.2.0   generics_0.1.3    curl_5.0.1       
 [9] fansi_1.0.4       pkgconfig_2.0.3   lifecycle_1.0.3   compiler_4.3.1   
[13] farver_2.1.1      textshaping_0.3.6 munsell_0.5.0     httpuv_1.6.11    
[17] htmltools_0.5.5   sass_0.4.6        yaml_2.3.7        later_1.3.1      
[21] pillar_1.9.0      crayon_1.5.2      jquerylib_0.1.4   ellipsis_0.3.2   
[25] openssl_2.0.6     cachem_1.0.8      mime_0.12         tidyselect_1.2.0 
[29] digest_0.6.31     stringi_1.7.12    labeling_0.4.2    fastmap_1.1.1    
[33] grid_4.3.1        colorspace_2.1-0  cli_3.6.1         utf8_1.2.3       
[37] withr_2.5.0       scales_1.2.1      promises_1.2.0.1  timechange_0.2.0 
[41] httr_1.4.6        askpass_1.1       ragg_1.2.5        hms_1.1.3        
[45] memoise_2.0.1     rlang_1.1.1       Rcpp_1.0.10       xtable_1.8-4     
[49] glue_1.6.2        renv_1.0.2        selectr_0.4-2     rstudioapi_0.14  
[53] jsonlite_1.8.5    R6_2.5.1          systemfonts_1.0.4 fs_1.6.2 
```
