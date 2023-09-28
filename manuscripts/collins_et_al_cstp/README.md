Analysis for Collins et al. Citizen Science Theory and Practice
manuscript
================
Keith Jennings
2023-09-28

Import the tidyverse package and the study data.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   1.0.1
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.2
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.4     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
df <- read.csv("../../data/processed/mros_obs_gpm_processed_20220503.csv")
```

Filter to just the study domain and obs that pass QC.

``` r
studyDomain = data.frame(ecoregions = c("High Plains", 
                                        "Southern Rockies", 
                                        "Colorado Plateaus",   # Colorado
                                        "Central Basin and Range", 
                                        "Sierra Nevada",               # Sierra
                                        "Willamette Valley", 
                                        "Cascades",                          # Oregon
                                        "Eastern Great Lakes Lowlands", 
                                        "Northeastern Highlands"), # Northeast
                         regions = c(rep("Colorado", 3),
                                   rep("Sierra", 2),
                                   rep("Oregon", 2),
                                   rep("Northeast", 2)))

obsStudy <- filter(df, 
                   eco_l3 %in% studyDomain$ecoregion)
obsPass <- filter(obsStudy, 
                  tair_flag == "Pass" &
                  ppt_flag == "Pass" & 
                  rh_flag == "Pass" &
                  dist_flag == "Pass" & 
                  dupe_flag == "Pass") %>% 
  left_join(., studyDomain,
            by = c("eco_l3" = "ecoregions"))
```

Some summary info:

- There were 13017 observations during the study period from 2021-10-07
  to 2022-05-03
- Of those observations, 11906 were in the targeted study regions
- 11517 observations (96.7%) in the study regions passed our quality
  control checks

Observations by Level 3 Ecoregion

| eco_l3                       |    n |
|:-----------------------------|-----:|
| Northeastern Highlands       | 4159 |
| Sierra Nevada                | 1779 |
| Southern Rockies             | 1746 |
| Central Basin and Range      | 1509 |
| Eastern Great Lakes Lowlands | 1046 |
| High Plains                  |  607 |
| Colorado Plateaus            |  585 |
| Willamette Valley            |   55 |
| Cascades                     |   31 |

Observations by study region

| regions   |    n |
|:----------|-----:|
| Northeast | 5205 |
| Sierra    | 3288 |
| Colorado  | 2938 |
| Oregon    |   86 |
