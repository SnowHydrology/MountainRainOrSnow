# Load packages
library(tidyverse)

########## User input

# Input files
citsci.input = "data/processed/mros_obs_processed_20220503.RDS"
gpm.input = "data/processed/mros_gpm_processed_20220503.RDS"

# Output files
citsci.output = "data/processed/"

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Add GPM probability thresholds for rain, snow, mix
prob_thresh_upper_rain = 100
prob_thresh_lower_rain = 50
prob_thresh_upper_snow = 50
prob_thresh_lower_snow = 0
prob_mix_offset = 10 # used for defining mix probability range
#prob_thresh_upper_mix = prob_thresh_lower_rain
#prob_thresh_lower_mix = prob_thresh_upper_snow

########## Data import

# Import obs and GPM
obs_all <- readRDS(citsci.input) 
gpm <- readRDS(gpm.input)

# Put the GPM data into the obs dataset
obs_all <- left_join(obs_all, 
                 gpm, 
                 by = "id")

# Export the data 
write.csv(select(obs_all, -time_diff),
          file = "data/processed/mros_obs_gpm_processed_20220503.csv")

########## Data prep

# Compute the state and ecoregion number of valid obs
eco3 <- obs_all %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)
eco4 <- obs_all %>% group_by(eco_l4) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh) %>% 
  filter(eco_l4 != "Sedimentary Subalpine Forests")
state <- obs_all %>% group_by(state) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)
all_locs <- c(eco3$eco_l3, eco4$eco_l4, state$state)

# Make the data longer for summary
# And filter to just our states/ecoregions of interests
obs <- bind_rows(
  obs_all %>% 
    select(id, phase, gpm_prob, eco_l3, eco_l4, state, tair_bin_num) %>% 
    pivot_longer(cols = c(eco_l3, eco_l4, state), 
                 values_to = "name",
                 names_to = "loc_group") %>% 
    mutate(qc = "no"),
  obs_all %>% 
    filter(., tair_flag == "Pass" & 
             ppt_flag == "Pass" & 
             rh_flag == "Pass" &
             dist_flag == "Pass" & 
             dupe_flag == "Pass") %>% 
    select(id, phase, gpm_prob, eco_l3, eco_l4, state, tair_bin_num) %>% 
    pivot_longer(cols = c(eco_l3, eco_l4, state), 
                 values_to = "name",
                 names_to = "loc_group") %>% 
    mutate(qc = "yes")
) %>% 
  filter(name %in% all_locs) %>% 
  mutate(mix = "yes")

# Bind the data again, but remove mix obs
obs <- bind_rows(
  obs,
  obs %>% 
    filter(., phase != "Mix") %>% 
    mutate(mix = "no")
)

########## Data analysis

# Denote whether phase designation was correct or not
obs <- obs %>% 
  mutate(gpm_phase = case_when(mix == "yes" ~
                                 case_when(gpm_prob <= prob_thresh_upper_snow &
                                             gpm_prob >= prob_thresh_lower_snow ~ "Snow",
                                           gpm_prob <= prob_thresh_upper_rain &
                                             gpm_prob >= prob_thresh_lower_rain ~ "Rain"),
                               mix == "no" ~
                                 case_when(gpm_prob <= (prob_thresh_upper_snow - prob_mix_offset) &
                                             gpm_prob >= prob_thresh_lower_snow ~ "Snow",
                                           gpm_prob <= prob_thresh_upper_rain  &
                                             gpm_prob >= (prob_thresh_lower_rain + prob_mix_offset) ~ "Rain",
                                           gpm_prob < (prob_mix_offset + prob_thresh_lower_rain) &
                                             gpm_prob > (prob_thresh_upper_snow - prob_mix_offset) ~ "Mix")),
         gpm_score = case_when(gpm_phase == phase ~ 1,
                               gpm_phase != phase ~ 0))


# Summarize by tair bin and other groupings
gpm_summary <- obs %>% 
  group_by(tair_bin_num, name, loc_group, qc, mix) %>% 
  summarize(n_obs = n(), 
            gpm_perf_pct = (sum(gpm_score) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_gpm = (sum(gpm_phase == "Snow") / n_obs) * 100)


# Export


# First source the script for plot formats and fills
source("analysis/functions/mros_plot_formats.R")

# Add factor level to phase to force Rain > mix > Snow order
obs <- obs %>% mutate(phase = factor(phase, levels = c("Rain", "Mix", "Snow")))

# Plot histograms of GPM prob by phase
ggplot(obs, aes(gpm_prob, fill = phase)) + 
  geom_histogram(color = "black") +
  phase_fill_scale +
  facet_wrap(~phase, scales = "free") +
  labs(x = "GPM Probablity of Liquid Precipitation (%)",
       y = "Number of Observations per Phase") +
  theme(legend.position = "none")

# Plot
ggplot(gpm_summary, aes(tair_bin_num, gpm_perf_pct)) + 
  geom_line() +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Success Rate (%)")

# Plot with snow %
ggplot(gpm_summary, aes(tair_bin_num, gpm_perf_pct)) + 
  geom_line()+
  geom_line(data = gpm_summary, aes(tair_bin_num, snow_pct_obs), color = "purple") +
  geom_line(data = gpm_summary, aes(tair_bin_num, snow_pct_gpm), color = "blue") +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Success Rate (black)\nObs Snow (purple)\nGPM Snow (blue) (all %)")

filter(gpm_summary, loc_group == "state") %>% 
  ggplot(., aes(tair_bin_num, gpm_perf_pct, lty = mix)) + 
  geom_line()+
  geom_line(aes(tair_bin_num, snow_pct_obs, lty = mix), color = "purple") +
  geom_line(aes(tair_bin_num, snow_pct_gpm, lty = mix), color = "blue") +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Success Rate (black)\nObs Snow (purple)\nGPM Snow (blue) (all %)") + 
  facet_wrap(~name + qc)

reshape2::melt(select(gpm_summary_noMIXED, -n_obs), id.vars = "tair_bin_num2", 
               variable.name = "metric", value.name = "pct") %>% 
  ggplot(., aes(tair_bin_num2, pct, color = metric)) +
  geom_line(lwd = 1) +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Performance and Snow Probability (%)") + 
  scale_color_manual(values = c("black", "purple", "blue"),
                     labels = c("GPM Success Rate", "Obs. Snow Prob.", 
                                "GPM Snow Prob."),
                     name = "") +
  theme(legend.position = c(0.1, 0.2))

# Compare GPM to obs snow pct
ggplot(gpm_summary_noMIXED, aes(snow_pct_obs, snow_pct_gpm)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm")

# Summarize
summary(lm(snow_pct_obs ~ snow_pct_gpm, gpm_summary_noMIXED))
