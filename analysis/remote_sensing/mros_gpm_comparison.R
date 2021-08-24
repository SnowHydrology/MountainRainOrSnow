
# Put the GPM data into the obs dataset
obs <- left_join(obs, 
                 gpm_obs, 
                 by = "id")

# Plot histograms of GPM prob by phase
ggplot(obs, aes(gpm_prob)) + geom_histogram() + facet_wrap(~phase, scales = "free")

# Plot GPM prob by tair
ggplot(obs, aes(tair, gpm_prob)) + geom_point()

################################################################################
# Summarize correct observations by temp bin

# Add GPM probability thresholds for rain, snow, mixed
prob_thresh_upper_rain = 100
prob_thresh_lower_rain = 50
prob_thresh_upper_snow = 50
prob_thresh_lower_snow = 0
prob_thresh_upper_mixed = prob_thresh_lower_rain
prob_thresh_lower_mixed = prob_thresh_upper_snow

# Make wider bins for analysis
tair_max = 14
tair_min = -7
tair_bin_width = 1
obs$tair_bin2 <- cut(obs$tair, 
                     breaks = seq(tair_min, 
                                  tair_max, 
                                  by = tair_bin_width))


# Add numeric version of each bin
tair_cuts_to_number2 <- data.frame("tair_bin2" = levels(obs$tair_bin2),
                                   "tair_bin_num2" = seq(tair_min + (0.5 * tair_bin_width),
                                                         tair_max - (0.5 * tair_bin_width), 
                                                         by = tair_bin_width))

# Join
obs <- left_join(obs, tair_cuts_to_number2, by = "tair_bin2")


# Denote whether phase designation was correct or not
obs <- obs %>% 
  mutate(phase_noMIXED = case_when(phase == "Mixed" ~ "Rain",
                                   TRUE ~ phase),
         gpm_phase = case_when(gpm_prob <= prob_thresh_upper_snow &
                                 gpm_prob >= prob_thresh_lower_snow ~ "Snow",
                               gpm_prob <= prob_thresh_upper_rain &
                                 gpm_prob >= prob_thresh_lower_rain ~ "Rain",
                               gpm_prob < prob_thresh_upper_mixed &
                                 gpm_prob > prob_thresh_lower_mixed~ "Mixed"),
         gpm_score = case_when(gpm_phase == phase ~ 1,
                               gpm_phase != phase ~ 0),
         gpm_score_noMIXED = case_when(gpm_phase == phase_noMIXED ~ 1,
                                       gpm_phase != phase_noMIXED ~ 0))


# Summarize by tair bin
gpm_summary_noMIXED <- obs %>% 
  filter(!is.na(gpm_score_noMIXED)) %>% 
  group_by(tair_bin_num2) %>% 
  summarize(n_obs = length(gpm_score_noMIXED), 
            gpm_perf_pct = (sum(gpm_score_noMIXED) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_gpm = (sum(gpm_phase == "Snow") / n_obs) * 100)


gpm_summary <- obs %>% 
  filter(!is.na(gpm_score)) %>% 
  group_by(tair_bin_num2) %>% 
  summarize(n_obs = length(gpm_score), 
            gpm_perf_pct = (sum(gpm_score) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_gpm = (sum(gpm_phase == "Snow") / n_obs) * 100)



# First source the script for plot formats and fills
source("analysis/functions/mros_plot_formats.R")

# Add factor level to phase to force Rain > Mixed > Snow order
obs <- obs %>% mutate(phase = factor(phase, levels = c("Rain", "Mixed", "Snow")))

# Plot histograms of GPM prob by phase
ggplot(obs, aes(gpm_prob, fill = phase)) + 
  geom_histogram(color = "black") +
  phase_fill_scale +
  facet_wrap(~phase, scales = "free") +
  labs(x = "GPM Probablity of Liquid Precipitation (%)",
       y = "Number of Observations per Phase") +
  theme(legend.position = "none")

# Plot
ggplot(gpm_summary, aes(tair_bin_num2, gpm_perf_pct)) + 
  geom_line() +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Success Rate (%)")

# Plot with snow %
ggplot(gpm_summary, aes(tair_bin_num2, gpm_perf_pct)) + 
  geom_line()+
  geom_line(data = gpm_summary, aes(tair_bin_num2, snow_pct_obs), color = "purple") +
  geom_line(data = gpm_summary, aes(tair_bin_num2, snow_pct_gpm), color = "blue") +
  labs(x = expression("Air Temperature Bin ("*degree*C*")"),
       y = "GPM Success Rate (black)\nObs Snow (purple)\nGPM Snow (blue) (all %)")

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
