# Mountain rain or snow rain-snow line creation from citsci data

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(foreach) # for parallel processing
library(doMC); registerDoMC(cores = 4)


########## User input

# Input files
citsci.input = "data/processed/mros_obs_processed_20220503.RDS"

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Compute the state and ecoregion number of valid obs
eco3 <- obs %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)
eco4 <- obs %>% group_by(eco_l4) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh) %>% 
  filter(eco_l4 != "Sedimentary Subalpine Forests")
state <- obs %>% group_by(state) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)

# Import obs
obs <- readRDS(citsci.input) %>% 
  filter(tair_flag == "Pass" & 
           ppt_flag == "Pass" & 
           rh_flag == "Pass" &
           dist_flag == "Pass" & 
           closest_flag == "Pass" &
           nstation_flag == "Pass" &
           eco_l3 %in% eco3$eco_l3)

# Create elevation bins and names
obs <- obs %>%
  mutate(elev_bin = cut_width(elev, width = 100))

obs_by_elev_date <- obs %>% 
  group_by(date, elev_bin, eco_l3) %>% 
  summarise(snow_prob = sum(phase == "Snow")/length(phase) * 100) %>% 
  mutate(elev = str_extract(string = elev_bin, pattern = "(?<=()).*(?=,)"),
         elev = as.numeric(str_sub(elev, 2, -1)) + 50)

# Get unique dates and locations
dates_locs <- unique(obs_by_elev_date[c("date", "eco_l3")])

# Compute 50% rain-snow elevation per scene
rain_snow_line.l <-
  foreach(i = 1:length(dates_locs$date), .errorhandling = "pass") %dopar% {
    
    # Filter GPM to datetime
    tmp.df <- filter(obs_by_elev_date, date == pull(dates_locs[i, "date"]) &
                       eco_l3 == pull(dates_locs[i, "eco_l3"]))
    
    # Check that there are probabilities above and below 50%
    if(min(tmp.df$snow_prob) >= 50){
      tmp.rs_line = NA
      tmp.note = "above"
    } else if(max(tmp.df$snow_prob) < 50){
      tmp.rs_line = NA
      tmp.note = "below"
    } else {
      # Build linear model predicting prob by elev
      tmp.lm <- lm(snow_prob ~ elev, tmp.df)
      tmp.slope = tmp.lm$coefficients[2] %>% as.numeric()
      tmp.intercept = tmp.lm$coefficients[1] %>% as.numeric()
      tmp.rs_line = (50 - tmp.intercept) / tmp.slope
      #tmp.r2 = summary(tmp.lm)$r.squared
      #tmp.pval = summary(tmp.lm)$coefficients[2,4] 
      tmp.note = "valid"
    }
    
    # Output into dataframe
    tmp.rain_snow_line <- data.frame(date =  pull(dates_locs[i, "date"]),
                                     eco_l3 =  pull(dates_locs[i, "eco_l3"]),
                                     rs_line = tmp.rs_line,
                                     note = tmp.note)
    
    # Print to list
    tmp.rain_snow_line
    
  }

# Bind list into dataframe
rain_snow_line <- plyr::ldply(rain_snow_line.l, bind_rows)


# Export data
saveRDS(object = rain_snow_line,
        file = "data/processed/mros_obs_rain_snow_line_2020-2021.RDS")


# Plot all probs by elev bin
ggplot(obs_by_elev_date, aes(elev_bin, snow_prob)) + 
  geom_point() + 
  facet_wrap(~date)
