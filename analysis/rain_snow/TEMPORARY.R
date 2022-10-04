library(tidyverse)
#library(cowplot); theme_set(theme_bw())
library(cowplot); theme_set(theme_bw(base_size = 14)) # for pres plots

allData <- readRDS("~/Documents/projects/mountain_rain_or_snow/analysis/MountainRainOrSnow/data/processed/mros_obs_rs_partitioning_20220503.RDS")

# Input files
citsci.input = "data/processed/mros_obs_gpm_processed_20220503.csv"

# RS line
rain_snow_line <- readRDS("data/processed/mros_obs_rain_snow_line_20220503.RDS")

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Threshold for obs per temp bin (fewer obs = greater noise)
obs_per_bin_thresh = 15

# Import the data 
# And filter to just those passing the QC checks
obs <- read.csv(citsci.input)

# Compute the state and ecoregion number of valid obs
eco3 <- obs %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)
eco4 <- obs %>% group_by(eco_l4) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh) %>% 
  filter(eco_l4 != "Sedimentary Subalpine Forests")
state <- obs %>% group_by(state) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)

# Get the observed prob and predicted prob data
obsProb <- allData[["snow_prob"]] %>% 
  filter(name %in% c(eco3$eco_l3, eco4$eco_l4, state$state)) %>% 
  filter(name != "Northern Piedmont" & group != "eco_3")
predProb <-allData[["snow_pred"]] %>% 
  filter(name %in% c(eco3$eco_l3, eco4$eco_l4, state$state))
t50 <- allData[["temp50"]] %>% 
  filter(name %in% c(eco3$eco_l3, eco4$eco_l4, state$state))

groupType = "state"
tempType = "tair"
qcType = FALSE
curveType = "hyp"
ggplot() +
  geom_point(data = filter(obsProb, group == groupType, temp_type == tempType, qc == qcType), 
             aes(temp_val, snow_prob)) +
  geom_line(data = filter(predProb, loc_group == groupType, temp_type == tempType, qc == "yes"), 
             aes(temp_val, snow_prob_pred, color = method)) +
  facet_wrap(~name)

groupType = "eco_l3"
tempType = "tair"
qcType = FALSE
methodType = "log"

# all obs and fit
ggplot() +
  geom_point(data = filter(obsProb, group == groupType, temp_type == tempType, 
                           qc == qcType, n_obs > 15), 
             aes(temp_val, snow_prob*100), fill = "#ADEFD1FF", shape = 21) +
  geom_line(data = filter(predProb, loc_group == groupType, temp_type == tempType, 
                          qc == "yes", method == methodType), 
            aes(temp_val, snow_prob_pred*100),
            lwd = 1, 
            color = "#00203FFF") +
  facet_wrap(~name, nrow = 2) +
  labs(x = "Air Temperature (°C)", y = "Snowfall Probability (%)")

# Select fit lines
filter(predProb, loc_group == groupType, temp_type == tempType, 
       qc == "yes", method == methodType,
       name %in% c("Northeastern Highlands", "Sierra Nevada", "Southern Rockies")) %>% 
  ggplot() +
  geom_line(aes(temp_val, snow_prob_pred*100, color = name),
            lwd = 1) +
  labs(x = "Air Temperature (°C)", y = "Snowfall Probability (%)") +
  scale_color_brewer(palette = "Dark2") +
  ggrepel::geom_label_repel(data = filter(predProb, loc_group == groupType, temp_type == tempType, 
                                          qc == "yes", method == methodType,
                                          name %in% c("Northeastern Highlands", "Sierra Nevada", "Southern Rockies"),
                                          temp_val == 2.5),
                            aes(label = name, x = temp_val, y = snow_prob_pred*100,
                                color = name), min.segment.length = 0.1,
                            nudge_x = -1) +
  geom_hline(yintercept = 50, color = "gray", lty = "dashed", lwd = 1) +
  theme(legend.position = "none")

# Obs in the ecoregions
obs %>% filter(eco_l3 %in% eco3$eco_l3) %>% 
  summarize(n = n())
obs %>% filter(eco_l3 %in% eco3$eco_l3 &
                 tair_flag == "Pass" &
                 rh_flag == "Pass" &
                 ppt_flag == "Pass" &
                 dist_flag == "Pass" &
                 closest_flag == "Pass" &
                 nstation_flag == "Pass") %>% 
  group_by(phase) %>%
  summarize(n = n())


# Plot mixed precip
obsProb %>% 
  filter( temp_type == "tair" & group == "eco_l3" &
           name %in% eco3$eco_l3 & n_obs > 15) %>% 
  ggplot(., aes(temp_val, name, fill = mixed_prob)) +
  geom_raster() +
  scale_fill_viridis_c()

# Plot GPM
ggplot(filter(obs, phase == "Mix"), aes(gpm_prob)) + 
  geom_histogram(fill = "purple", color = "black") +
  labs(x = "IMERG PLP (%)", y = "Count")

# Import rs-line

# Plot for april events in NE
ne_spring_rs_line_plot <-
  ggplot(filter(rain_snow_line,!is.na(rs_line) & eco_l3 == "Northeastern Highlands" & date > ymd("2022-03-01")), 
         aes(date, rs_line)) + 
  geom_point() +
  ylim(0,1200) + 
  geom_line(lwd = 1, color = "purple") +
  labs(x = "Date", y = "Daily Rain-Snow\nLine Elev. (m)")
ne_apr_snowprobs_plot <-
  ggplot(filter(obs_by_elev_date, eco_l3 == "Northeastern Highlands" & date %in% c(ymd("2022-04-16"), ymd("2022-04-17"))), aes(elev, snow_prob)) + 
  geom_point() + 
  facet_wrap(~date) +
  labs(x = "Elevation (m)", y = "Snowfall\nProbability (%)")
plot_grid(
  ne_spring_rs_line_plot,
  ne_apr_snowprobs_plot, 
  ncol = 1, labels = "auto"
)


# GPM analysis
gpm <- read.csv("data/processed/mros_obs_gpm_processed_20220503.csv")
