Mountain Rain or Snow 2020-2021 Analysis
================
Keith Jennings
7/26/2021

# Introduction

The `Analysis` directory includes the processing scripts used to
evaluate precipitation phase data submitted by citizen scientists taking
part in *Mountain Rain or Snow*. This includes pre-processing the
observations, downloading ancillary meteorological data, estimating air
temperature at each observation point, and quality controlling the
precipitation phase reports.

The analysis below includes observations submitted near Lake Tahoe
during the 2020 and 2021 campaigns.

# Step 1: Data Pre-Processing

The `mros_cit_sci_obs_preprocess.R` script in `analysis/pre_processing`
associates the following geospatial data with each observation:

-   Level II, III, and IV Ecoregions
-   US State
-   Elevation (m)

The script also formats the time data and removes all observations
submitted without geolocation info.

# Step 2: Meteorological Data Download and Pre-Processing

Few of the citizen science observations are submitted near existing
meteorological measurement stations, meaning we need to model the air
temperature for each rain, snow, mixed data point. We do this with air
temperature data from four networks:

| Network Name | URL                                                      |
|--------------|----------------------------------------------------------|
| HADS         | <https://mesonet.agron.iastate.edu/request/dcp/fe.phtml> |
| SNOTEL       | <https://wcc.sc.egov.usda.gov/reportGenerator/>          |
| RAWS         | <https://raws.dri.edu/>                                  |
| NCDC LCD     | <https://gis.ncdc.noaa.gov/maps/ncei/lcd>                |

The two intial datasets can be accessed programatically using
`analysis/download/data_download_hads.R` and
`analysis/download/data_download_snotel.R` while the latter two need to
be downloaded manually from the links. After downloading the data, we
prepare them for the temperature model using
`analysis/pre_processing/mros_met_data_preprocess.R`.

# Step 3: Air Temperature Modeling

We then model the air temperature at each location using four methods in
the `analysis/met_modeling/mros_air_temperature_model.R` script:

-   Inverse distance weighting (IDW), plus a constant lapse rate of
    -0.005°C/m
-   IDW, plus a variable lapse rate computed from air temperature
    observations
-   Nearest met station, plus a constant lapse rate of -0.005°C/m
-   Nearest met station, plus a variable lapse rate computed from air
    temperature observations

As a result of its higher performance in terms of mean bias and
r<sup>2</sup>, we use the IDW plus variable lapse rate method when
assigning air temperature to each observation point.

Future versions of the code will model additional meteorological
variables, such as relative humidity, dew point temperature, and wet
bulb temperature.

# Step 4: Quality Control of Citizen Science Observations

Next, we flag observations based on five criteria:

1.  Whether precipitation was recorded that day at the nearby NCDC LCD
    met stations.
2.  The estimated air temperature relative to realistic values for rain
    and snow.
3.  The relative humidity at the observation point
4.  Average distance from the met stations.
5.  If the timestamp was a duplicate of an observation from the same
    observer.

We then output the processed, quality controlled observations to a
shareable data file: `data/processed/mros_obs_processed_2020_2021.RDS`

# Looking at the data

Now we can examine the data more in depth. First, we’ll start an R
session and import the file:

``` r
# Load the tidyverse
library(tidyverse)

# Use cowplot for plot formatting and import plot styles
library(cowplot); theme_set(theme_cowplot())
source("functions/mros_plot_formats.R")

# Lubridate for date handling tools
library(lubridate)

# Import data
obs <- readRDS("../data/processed/mros_obs_processed_2020_2021.RDS")

# Add factor level to phase to force Rain > Mixed > Snow order
obs <- obs %>% mutate(phase = factor(phase, levels = c("Rain", "Mixed", "Snow")))
```

Within the `obs` file we have a total of 2495 observations submitted
between 2020-01-08 and 2021-05-23 in the Lake Tahoe area. Of these 2495
observations, 2248 passed all of the quality control checks (this is
90.1% of the database).

For the rest of document, we’ll evaluate only data that passed the QC
checks. We’ll also add elevation bins for additional analyses

``` r
# Filter to passing obs
obsPass <- filter(obs, tair_flag == "Pass" & 
                        ppt_flag == "Pass" & 
                        rh_flag == "Pass" &
                        dist_flag == "Pass" & 
                        dupe_flag == "Pass")

# Add elevation info
obsPass <- obsPass %>% 
   mutate(elev_bin = cut_width(elev, width = 500))

# Summarize by day
obsDaily <- obsPass %>% group_by(date) %>% summarize(n = n(), snow_pct = sum(phase == "Snow")/n() * 100, rain_pct = sum(phase == "Rain")/n() * 100, mixed_pct = sum(phase == "Mixed")/n() * 100 )
```

## Observations by elevation

We received precipitation reports in the study area between 771 m and
2680 m, with mean and median report elevations of 1742 m and 1844 m,
respectively. In total, there were 1438, 472, 338, snow, rain, and mixed
observations.

The elevational breakdown of precipitation phase looks like this:

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Similarly, we can look at the number of reports per precipitation phase
by elevation:

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Or grouped together:

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Observations by state and ecoregion

Across the study period, we had the following geographic breakdown of
observations.

By state:

| state      |    n |
|:-----------|-----:|
| California | 1069 |
| Nevada     | 1179 |

By US EPA Level III Ecoregion:

| eco_l3                  |    n |
|:------------------------|-----:|
| Central Basin and Range |  827 |
| Sierra Nevada           | 1421 |

And by US EPA Level IV Ecoregion:

| eco_l4                                             |    n |
|:---------------------------------------------------|-----:|
| Central Sierra Lower Montane Forests               |   41 |
| Central Sierra Mid-Montane Forests                 |   10 |
| Lahontan Salt Shrub Basin                          |   10 |
| Northeastern Sierra Mixed Conifer-Pine Forests     | 1094 |
| Northern Sierra Mid-Montane Forests                |   15 |
| Northern Sierra Subalpine Forests                  |    9 |
| Northern Sierra Upper Montane Forests              |  252 |
| Sierra Nevada-Influenced Ranges                    |    6 |
| Sierra Nevada-Influenced Semiarid Hills and Basins |  811 |

## Observations by year

Our *Tahoe Rain or Snow* study period comprised two water years: 2020
and 2021.

``` r
# Add water year info
obsPass <- obsPass %>% 
   mutate(wy = ifelse(month(date) >= 10,
                      year(date) + 1,
                      year(date)),
          dowy = ifelse(month(date) >= 10,
                        yday(date) - 273,
                        yday(date) + 92))

# Compute cumulative observations per phase and water year
obsPass <- obsPass %>% 
  arrange(date) %>% 
  group_by(wy, phase) %>% 
  mutate(n_cumulative = row_number())
```

We received 953 observations in water year 2020 and 1295 in 2021. Snow
was the most frequent phase in both water years, but both rain and mixed
precipitation increased in relative proportion in 2021.

|   wy | phase |   n |  pct |
|-----:|:------|----:|-----:|
| 2020 | Rain  | 151 | 15.8 |
| 2020 | Mixed | 134 | 14.1 |
| 2020 | Snow  | 668 | 70.1 |
| 2021 | Rain  | 321 | 24.8 |
| 2021 | Mixed | 204 | 15.8 |
| 2021 | Snow  | 770 | 59.5 |

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Observations by air temperature

According to modeled air temperature data, volunteers submitted
precipitation phase reports from a minimum air temperature value of
-10.2°C to a maximum of 25.7°C with a median value of 1.1°C. The vast
majority of reports came from a relatively narrow air temperature range.
Our data show 95% of observations corresponded to air temperatures
between -5.1°C and 9.6°C. 45.6% of reported precipitation fell between
0°C and 4°C, the air temperature range identified by previous research
to have the greatest rain-snow partitioning uncertainty.

We also found marked overlap in the phase distributions by temperature,
underscoring the difficulty in using near-surface meteorological data to
partition rain, snow, and mixed precipitation (FIG XXXX). The
distributions for rain and snow, for example, overlapped by 52.8%,
43.8%, and 45.7% for air, wet bulb, and dew point temperature,
respectively. This suggests wet bulb temperature is a more sensitive
predictor of rain versus snow relative to the other temperature types,
while air temperature is the least sensitive. Air temperature also had
the highest percent overlap between snow and mixed (72.8%) and rain and
mixed phases (75.3%). The minimum overlap between mixed precipitation
and another phase was 67.5%, indicating the near-surface meteorological
conditions favoring mixed precipitation are often indistinguishable from
rain and snow.

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Rain-snow partitioning

Using the observations and modeled meteorological data, we can compute
snowfall probability curves and 50% snowfall probability temperatures,
the latter of which are used as thresholds in models to split solid and
liquid precipitation. We provide a methods overview here and the full
code can be found in `mros_obs_analysis.R`. In short, we bin air
temperature in 1°C increments from -10°C to 20°C and wet bulb
temperature in 1°C increments from -12°C to 16°C. We then compute the
probability of snowfall occurring in each air and wet bulb temperature
bin. Those data are next fit with a hyperbolic tangent as in Dai (2008)
and Jennings et al. (2018) to create snowfall probability curves. The
50% snowfall probability air and wet bulb temperature thresholds are
where the fitted curves pass the 50% mark.

Let’s import the data.

``` r
# Import data
rs_p <- readRDS("../data/processed/mros_obs_rs_partitioning_2020_2021.RDS")
```

We can then look at the air temperature snowfall probability plot:

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The wet bulb temperature snowfall probability plot:

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The dew point temperature snowfall probability plot:

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

And the three of them combined:

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

When evaluating snowfall probability from the crowdsourced data by air,
wet bulb, and dew point temperature, we see the expected reverse sigmoid
curve (Fig. XXXX). In all cases probability is near 100% for the coldest
temperatures before decreasing as temperature warms. There are two
noticeable features of the air temperature curves: 1) the observed curve
has a greater amount of noise than the curves for wet bulb and dew point
temperature and 2) the fitted probability curve has a shallower gradient
near 50%, confirming the metric’s reduced sensitivity to rain-snow
partitioning. The 50% snowfall probability thresholds derived from
hyperbolic tangent curves fit to the citizen science data are 3.9°C,
0.9°C, and -0.5°C for air, wet bulb, and dew point temperature,
respectively. We expect threshold values to follow this trend as
T<sub>dew</sub> \< T<sub>wet</sub> \< T<sub>air</sub> when relative
humidity is less than 100%.

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

| phase |  mean_rh | temp     |
|:------|---------:|:---------|
| Rain  | 79.73312 | air      |
| Mixed | 72.74908 | air      |
| Snow  | 65.55655 | air      |
| Rain  | 82.74519 | wet bulb |
| Mixed | 77.43236 | wet bulb |
| Snow  | 69.99865 | wet bulb |

## 

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- --> \##
Rain-snow lines from the citizen science data

``` r
# Import processed rain-snow line data
rain_snow_line <- readRDS("../data/processed/mros_obs_rain_snow_line_2020-2021.RDS")

# Import the probabilities by elevation bin
rain_snow_prob_elev <- readRDS("../data/processed/mros_obs_rain_snow_prob_elev_2020-2021.RDS")

# Get valid dates from data
valid_dates = filter(rain_snow_line, note == "valid" &
                       n >= 10 &
                       rs_line <= max(obsPass$elev) &
                       rs_line >= min(obsPass$elev))$date

# Plot
ggplot(filter(rain_snow_prob_elev, date %in% valid_dates), 
       aes(snow_prob, elev)) + 
  geom_point() + 
  geom_hline(data = filter(rain_snow_line, date %in% valid_dates), aes(yintercept = rs_line)) + 
  facet_wrap(~date) 
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- --> During the
study period, we had 32 days when we could estimate a valid rain-snow
line elevation from the citizen science data. We did not consider values
above the maximum or below the minimum observation elevation to be
valid. That means of the 69 days with at least 10 observations, we could
compute daily rain-snow lines on 46.4% of them. There were 25 days when
the maximum snowfall probability was less than 50% and 2 days when the
minimum was above 50%. In these cases we would estimate the rain-line
elevation to be above or below the study domain, respectively,
indicating rain or snow dominance. On days with valid values, daily
rain-snow line elevations ranged from a minimum of 1174 m to a maximum
of 2405 m, with a median value of 1707 m.

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

Instances of valid daily rain-snow line estimates occurring in
succession where relatively rare during our study period.

## Precipitation phase partitioning method comparison

Import the summary data produced in
`analysis/rain_snow/mros_method_comparison.R`

``` r
# import the summary and summary by tair data
method_summary <- readRDS("../data/processed/mros_obs_sim_summary.RDS") %>% 
  filter(scenario != "thresh_tair_0" & scenario != "thresh_tair_0.5")
method_summary_byTair <- readRDS("../data/processed/mros_obs_sim_summary_byTair.RDS") %>% 
  filter(scenario != "thresh_tair_0" & scenario != "thresh_tair_0.5")
```

The different precipitation phase partitioning methods applied to the
modeled meteorological data produced marked variability in the predicted
snow, rain, and mixed proportions relative to the observations. Over our
study period, crowdsourced precipitation phase reports were comprised of
64% snow, 21% rain, and 15% mixed. In comparison, the precipitation
phase partitioning methods tended to underpredict snow and mixed
precipitation, while overpredicting rain. Snowfall frequency estimates
from the different methods ranged from a minimum of 23.5% to a maximum
of 84.7%, with an average of 60.7% and standard deviation of 18%.
Rainfall frequency estimates were similarly varied, ranging from a
minimum of 15.3% to a maximum of 57.3%, with an average of 34.9% and
standard deviation of 12.3%. Only the two air temperature ranges
predicted mixed precipitation, with frequency estimates of 14.4% and
48.1%. The other methods do not assign mixed precipitation, bringing
down the average to 4.5%.

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

Besides the variability in the estimated frequency of the different
phases, we also quantified the performance of the methods. Although
refining their output was not the goal of this research, we do note some
relevant results here. Method success rate, where 100% equals all
precipitation phase observations correctly predicted, ranged from 41.5%
for R<sub>a1.0</sub> to 71.4% for T<sub>d0.0</sub>. The top six methods
compared to the observations all incorporated humidity in some form and,
notably, none could predict mixed precipitation as they were the wet
bulb and dew point thresholds along with the binary logistic regression
model. This meant, in practice, the best they could perform was 85%.
Even though the range methods could predict mixed precipitation, they
performed poorly overall. The best-performing air temperature method was
the optimized T<sub>a4.2</sub> from our previous study. Additionally,
there were no methods that had both snow and rain relative biases less
than 10%. In general, the methods that had low snow biases had higher
rain biases and vice versa.

For comparison sake we also reassigned mixed precipitation to be rain as
in the IMERG PLP product, which only considers precipitation to be in
the liquid or solid phase. Applying this assumption widened the range of
method success rates, which now stretched from a minimum of 38.5% for
R<sub>a1.0</sub> to 75.6% for T<sub>d0.0</sub>. Again, the top six
methods were all of the ones that incorporated humidity. In this
analysis, there were two methods, Bin<sub>log</sub> and
T<sub>w0.5</sub>, that had rain and snow frequency relative bias
magnitudes \<= 10%.

``` r
method_summary_4table <-
  left_join(filter(method_summary, eval_type == "all"),
            filter(method_summary, eval_type == "mixRain"),
                   by = "scenario") %>% 
  mutate(scenario = factor(scenario, 
                           levels=c("thresh_tair_1",
                                    "thresh_tair_1.5",
                                    "thresh_tair_1.8",
                                    "thresh_tair_1.9",
                                    "thresh_tair_2.7",
                                    "thresh_tair_4.2",
                                    "thresh_twet_0",
                                    "thresh_twet_0.5",
                                    "thresh_twet_1", 
                                    "thresh_tdew_0",
                                    "thresh_tdew_0.5",
                                    "range_tair_-0.5_0.5",
                                    "range_tair_-1_3",
                                    "binlog"))) %>% 
  arrange(scenario) %>% 
  mutate_if(is.numeric, round, digits = 1) %>% 
  as.data.frame()
rownames(method_summary_4table) <- c("T~a1.0~",
                               "T~a1.5~",
                               "T~a1.8~",
                               "T~a1.9~",
                               "T~a2.7~",
                               "T~a4.2~",
                               "T~w0.0~",
                               "T~w0.5~",
                               "T~w1.0~",
                               "T~d0.0~",
                               "T~d0.5~",
                               "R~a0.0~",
                               "R~a1.0~",
                               "Bin~log~")
method_summary_4table %>% 
  #filter(eval_type == "all" | eval_type == "mixRain") %>% 
  select(`Success Rate A` = perf_pct.x, `Snow Bias A` = snow_bias_pct.x, 
         `Rain Bias A` = rain_bias_pct.x, `Mixed Bias A` = mixed_bias_pct.x,
         `Success Rate MR` = perf_pct.y, `Snow Bias MR` = snow_bias_pct.y, 
         `Rain Bias MR` = rain_bias_pct.y, `Mixed Bias MR` = mixed_bias_pct.y) %>% 
  knitr::kable()
```

|                   | Success Rate A | Snow Bias A | Rain Bias A | Mixed Bias A | Success Rate MR | Snow Bias MR | Rain Bias MR | Mixed Bias MR |
|:------------------|---------------:|------------:|------------:|-------------:|----------------:|-------------:|-------------:|--------------:|
| T<sub>a1.0</sub>  |           58.8 |       -24.7 |       146.8 |       -100.0 |            68.3 |        -24.7 |         43.8 |           NaN |
| T<sub>a1.5</sub>  |           60.7 |       -14.3 |       115.0 |       -100.0 |            68.8 |        -14.3 |         25.3 |           NaN |
| T<sub>a1.8</sub>  |           61.0 |        -7.9 |        95.6 |       -100.0 |            68.3 |         -7.9 |         14.0 |           NaN |
| T<sub>a1.9</sub>  |           61.4 |        -6.7 |        92.2 |       -100.0 |            68.5 |         -6.7 |         12.0 |           NaN |
| T<sub>a2.7</sub>  |           63.6 |         6.2 |        52.8 |       -100.0 |            69.4 |          6.2 |        -11.0 |           NaN |
| T<sub>a4.2</sub>  |           65.7 |        28.2 |       -14.4 |       -100.0 |            68.8 |         28.2 |        -50.1 |           NaN |
| T<sub>w0.0</sub>  |           66.5 |        -6.6 |        91.7 |       -100.0 |            74.9 |         -6.6 |         11.7 |           NaN |
| T<sub>w0.5</sub>  |           68.4 |         4.3 |        58.5 |       -100.0 |            75.1 |          4.3 |         -7.7 |           NaN |
| T<sub>w1.0</sub>  |           69.2 |        16.0 |        22.9 |       -100.0 |            74.3 |         16.0 |        -28.4 |           NaN |
| T<sub>d0.0</sub>  |           71.4 |        22.3 |         3.6 |       -100.0 |            75.6 |         22.3 |        -39.6 |           NaN |
| T<sub>d0.5</sub>  |           70.9 |        32.3 |       -26.9 |       -100.0 |            73.8 |         32.3 |        -57.4 |           NaN |
| R<sub>a0.0</sub>  |           46.8 |       -55.7 |       172.9 |         -4.4 |            55.1 |        -55.7 |         59.0 |           Inf |
| R<sub>a1.0</sub>  |           41.5 |       -63.2 |        35.2 |        219.8 |            38.5 |        -63.2 |        -21.2 |           Inf |
| Bin<sub>log</sub> |           66.6 |        -2.7 |        79.9 |       -100.0 |            74.1 |         -2.7 |          4.8 |           NaN |

As expected, we saw method performance vary by air temperature with the
lowest success rates between 0°C and 10°C. All methods could reliably
predict snowfall at air temperatures below freezing, while the air
temperature thresholds and ranges struggled to provide accurate snowfall
predictions above 0°C. None of the methods could accurately predict
freezing rain. The six methods incorporating humidity provided more
consistent performance across the range of modelled air temperatures
with T<sub>d0.0</sub> and T<sub>d0.5</sub> most capable of correctly
predicting the snow falling at warm temperatures. However, these methods
did this at the expense of underpredicting rain above 7.5°C.
T<sub>d0.0</sub>, the best-performing method in both analysis cases, had
a minimum success rate of 47.2% at 8.5°C. Although concerning, it is
still quantifiably better than the sub 20% minimum success rates of the
air temperature range methods.

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## GPM IMERG comparison

``` r
# Import data
gpm <- readRDS("../data/processed/mros_gpm_processed_2020_2021.RDS")

# Join the data
obsGPM <- left_join(obsPass,
                    gpm,
                    by = "id")

###############################################################################
# Summarize correct observations by temp bin

# Make wider bins for analysis
tair_max = 14
tair_min = -7
tair_bin_width = 1
obsGPM$tair_bin2 <- cut(obsGPM$tair, 
                     breaks = seq(tair_min, 
                                  tair_max, 
                                  by = tair_bin_width))


# Add numeric version of each bin
tair_cuts_to_number2 <- data.frame("tair_bin2" = levels(obsGPM$tair_bin2),
                                   "tair_bin_num2" = seq(tair_min + (0.5 * tair_bin_width),
                                                         tair_max - (0.5 * tair_bin_width), 
                                                         by = tair_bin_width))

# Join
obsGPM <- left_join(obsGPM, tair_cuts_to_number2, by = "tair_bin2")


# Add GPM probability thresholds for rain, snow, mixed
prob_thresh_upper_rain = 100
prob_thresh_lower_rain = 50
prob_thresh_upper_snow = 50
prob_thresh_lower_snow = 0
prob_thresh_upper_mixed = prob_thresh_lower_rain
prob_thresh_lower_mixed = prob_thresh_upper_snow

# Denote whether phase designation was correct or not
obsGPM$phase2 <- as.character(obsGPM$phase)
obsGPM <- obsGPM %>% 
  mutate(phase_noMIXED = case_when(phase2 == "Mixed" ~ "Rain",
                                   TRUE ~ phase2),
         gpm_phase = case_when(gpm_prob <= prob_thresh_upper_snow &
                                 gpm_prob >= prob_thresh_lower_snow ~ "Snow",
                               gpm_prob <= prob_thresh_upper_rain &
                                 gpm_prob >= prob_thresh_lower_rain ~ "Rain",
                               gpm_prob < prob_thresh_upper_mixed &
                                 gpm_prob > prob_thresh_lower_mixed~ "Mixed"),
         gpm_score = case_when(gpm_phase == phase2 ~ 1,
                               gpm_phase != phase2 ~ 0),
         gpm_score_noMIXED = case_when(gpm_phase == phase_noMIXED ~ 1,
                                       gpm_phase != phase_noMIXED ~ 0))

# Summarize by tair bin
gpm_summary_noMIXED <- obsGPM %>% 
  filter(!is.na(gpm_score_noMIXED)) %>% 
  group_by(tair_bin_num2) %>% 
  summarize(n_obs = length(gpm_score_noMIXED), 
            gpm_perf_pct = (sum(gpm_score_noMIXED) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_gpm = (sum(gpm_phase == "Snow") / n_obs) * 100)
```

Then plot the analyzed results:

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

## Rain-snow line comparison

Import the data

``` r
library(lubridate) # for datetime handling

# Import data
gpm <- readRDS("../data/processed/gpm_rain_snow_line_2020_2021.RDS") %>% 
  mutate(datetime = with_tz(datetime, "Etc/Gmt+8")) # convert to PST
flr <- read.csv("../data/processed/CFF_2019_2021.csv", na.strings = " NaN") %>% 
  mutate(datetime = with_tz(as.POSIXct(paste0(year, "-", month, "-", day, " ", hour),
                               format = "%Y-%m-%d %H",
                               tz = "GMT"),
                            "Etc/Gmt+8"), # convert to PST
         rain_snow_line = BBH * 1000) # convert from km to m
obs <- readRDS("../data/processed/mros_obs_rain_snow_line_2020-2021.RDS") %>% 
  mutate(datetime_min = as.POSIXct(paste0(date, " 08:00"), 
                                   tz = "Etc/Gmt+8"),
         datetime_max = as.POSIXct(paste0(date, " 20:00"), 
                                   tz = "Etc/Gmt+8"))

# Join data
all <- left_join(gpm, flr, by = "datetime")
```

Plot an event:

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->
