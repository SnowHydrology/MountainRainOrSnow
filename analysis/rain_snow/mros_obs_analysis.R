# Script for rain-snow analysis of MRoS precipitation phase observations

# Keith Jennings
# kjennings@lynker.com
# 2021-08-02

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(minpack.lm)
################################################################################
########################  Data Import and Preparation  #########################
################################################################################

# Import the data 
# And ilter to just those passing the QC checks
obs <- readRDS("data/processed/mros_obs_processed_2020_2021.RDS") %>% 
  filter(tair_flag == "Pass" & 
           ppt_flag == "Pass" & 
           rh_flag == "Pass" &
           dist_flag == "Pass" & 
           dupe_flag == "Pass")

# Create elevation bins and names
# And add water year to evaluate different project years
obs <- obs %>% 
  mutate(elev_bin = cut_width(elev, width = 500),
         wyear = if_else(as.numeric(format(datetime, "%m")) >= 10,
                         as.numeric(format(datetime, "%Y")) + 1,
                         as.numeric(format(datetime, "%Y"))))

# Add cumulative observations
obs <- obs %>% 
  arrange(datetime) %>% 
  mutate(obs_cumsum_all = 1:length(phase)) %>% 
  group_by(phase) %>% 
  mutate(obs_cumsum_phase = 1:length(phase)) %>% 
  group_by(elev_bin) %>% 
  mutate(obs_cumsum_elev = 1:length(phase)) %>% 
  group_by(eco_l3) %>% 
  mutate(obs_cumsum_ecol3 = 1:length(phase))


################################################################################
#                         50% Snowfall Probability                             #
################################################################################

# Make tair bins for analysis
tair_max = 20
tair_min = -10
tair_bin_width = 1
obs$tair_bin <- cut(obs$tair, 
                     breaks = seq(tair_min, 
                                  tair_max, 
                                  by = tair_bin_width))
# Make twet bins for analysis
twet_max = 16
twet_min = -12
twet_bin_width = 1
obs$twet_bin <- cut(obs$twet, 
                    breaks = seq(twet_min, 
                                 twet_max, 
                                 by = twet_bin_width))

# Make tdew bins for analysis
tdew_max = 12
tdew_min = -12
tdew_bin_width = 1
obs$tdew_bin <- cut(obs$tdew, 
                    breaks = seq(tdew_min, 
                                 tdew_max, 
                                 by = tdew_bin_width))



# Add numeric version of each bin
tair_cuts_to_number <- data.frame("tair_bin" = levels(obs$tair_bin),
                                   "tair_bin_num" = seq(tair_min + (0.5 * tair_bin_width),
                                                         tair_max - (0.5 * tair_bin_width), 
                                                         by = tair_bin_width))
twet_cuts_to_number <- data.frame("twet_bin" = levels(obs$twet_bin),
                                  "twet_bin_num" = seq(twet_min + (0.5 * twet_bin_width),
                                                       twet_max - (0.5 * twet_bin_width), 
                                                       by = twet_bin_width))
tdew_cuts_to_number <- data.frame("tdew_bin" = levels(obs$tdew_bin),
                                  "tdew_bin_num" = seq(tdew_min + (0.5 * tdew_bin_width),
                                                       tdew_max - (0.5 * tdew_bin_width), 
                                                       by = tdew_bin_width))

# Join
obs <- left_join(obs, tair_cuts_to_number, by = "tair_bin")
obs <- left_join(obs, twet_cuts_to_number, by = "twet_bin")
obs <- left_join(obs, tdew_cuts_to_number, by = "tdew_bin")


# Summarize by tair bin
obs_tair_summary <- obs %>% 
  group_by(tair_bin_num) %>% 
  summarize(n_obs = n(), 
            snow_prob = (sum(phase == "Snow") / n_obs))
obs_twet_summary <- obs %>% 
  group_by(twet_bin_num) %>% 
  summarize(n_obs = n(), 
            snow_prob = (sum(phase == "Snow") / n_obs))
obs_tdew_summary <- obs %>% 
  group_by(tdew_bin_num) %>% 
  summarize(n_obs = n(), 
            snow_prob = (sum(phase == "Snow") / n_obs))

# Fit the probability data to a hyperbolic tangent
prob_tair_fit <- nlsLM(snow_prob ~ #predicts snow frequency
                    a * (tanh( b* (tair_bin_num - c)) - d), #as a function of air temperature and 4 fitting parameters
                  #tanh is the hyperbolic tangent (gives curve shape)
                  data = obs_tair_summary, 
                  start = list(a = -45, b = 0.7, c = 1.2, d = 1))
prob_twet_fit <- nlsLM(snow_prob ~ #predicts snow frequency
                         a * (tanh( b* (twet_bin_num - c)) - d), #as a function of air temperature and 4 fitting parameters
                       #tanh is the hyperbolic tangent (gives curve shape)
                       data = obs_twet_summary, 
                       start = list(a = -45, b = 0.7, c = 1.2, d = 1))
prob_tdew_fit <- nlsLM(snow_prob ~ #predicts snow frequency
                         a * (tanh( b* (tdew_bin_num - c)) - d), #as a function of air temperature and 4 fitting parameters
                       #tanh is the hyperbolic tangent (gives curve shape)
                       data = obs_tdew_summary, 
                       start = list(a = -45, b = 0.7, c = 1.2, d = 1))

# Compute the 50% probability temperature
temp50_tair = (0.5 * log((1 + (0.5/as.numeric(coef(prob_tair_fit)[1]) + as.numeric(coef(prob_tair_fit)[4]))) / 
                      (1 - (0.5/as.numeric(coef(prob_tair_fit)[1]) + as.numeric(coef(prob_tair_fit)[4])))))/
  as.numeric(coef(prob_tair_fit)[2]) + 
  as.numeric(coef(prob_tair_fit)[3])
temp50_twet = (0.5 * log((1 + (0.5/as.numeric(coef(prob_twet_fit)[1]) + as.numeric(coef(prob_twet_fit)[4]))) / 
                           (1 - (0.5/as.numeric(coef(prob_twet_fit)[1]) + as.numeric(coef(prob_twet_fit)[4])))))/
  as.numeric(coef(prob_twet_fit)[2]) + 
  as.numeric(coef(prob_twet_fit)[3])
temp50_tdew = (0.5 * log((1 + (0.5/as.numeric(coef(prob_tdew_fit)[1]) + as.numeric(coef(prob_tdew_fit)[4]))) / 
                           (1 - (0.5/as.numeric(coef(prob_tdew_fit)[1]) + as.numeric(coef(prob_tdew_fit)[4])))))/
  as.numeric(coef(prob_tdew_fit)[2]) + 
  as.numeric(coef(prob_tdew_fit)[3])

# Make a predicted dataset using prob_fit
prob_tair_predict <- data.frame(tair_bin_num = seq(tair_min, tair_max, by = 0.5)) %>% 
  mutate(snow_prob_pred = predict(prob_tair_fit, newdata = .),
         snow_prob_pred = case_when(snow_prob_pred > 1 ~ 1,
                                    snow_prob_pred < 0 ~ 0,
                                    TRUE ~ snow_prob_pred))
prob_twet_predict <- data.frame(twet_bin_num = seq(twet_min, twet_max, by = 0.5)) %>% 
  mutate(snow_prob_pred = predict(prob_twet_fit, newdata = .),
         snow_prob_pred = case_when(snow_prob_pred > 1 ~ 1,
                                    snow_prob_pred < 0 ~ 0,
                                    TRUE ~ snow_prob_pred))
prob_tdew_predict <- data.frame(tdew_bin_num = seq(tdew_min, tdew_max, by = 0.5)) %>% 
  mutate(snow_prob_pred = predict(prob_tdew_fit, newdata = .),
         snow_prob_pred = case_when(snow_prob_pred > 1 ~ 1,
                                    snow_prob_pred < 0 ~ 0,
                                    TRUE ~ snow_prob_pred))

# Plot
ggplot(prob_tair_predict, aes(tair_bin_num, snow_prob_pred * 100)) + 
  geom_line(lwd = 1) + 
  geom_hline(yintercept = 50, color = "gray", lty = "dashed", lwd = 1) +
  geom_point(data = obs_tair_summary, aes(tair_bin_num, snow_prob * 100), 
             color = "purple") +
  labs(x = expression("Air Temperature ("*degree*C*")"),
       y = "Snowfall Probability (%)")
ggplot(prob_twet_predict, aes(twet_bin_num, snow_prob_pred * 100)) + 
  geom_line(lwd = 1) + 
  geom_hline(yintercept = 50, color = "gray", lty = "dashed", lwd = 1) +
  geom_point(data = obs_twet_summary, aes(twet_bin_num, snow_prob * 100), 
             color = "purple") +
  labs(x = expression("Wet Bulb Temperature ("*degree*C*")"),
       y = "Snowfall Probability (%)")
ggplot(prob_tdew_predict, aes(tdew_bin_num, snow_prob_pred * 100)) + 
  geom_line(lwd = 1) + 
  geom_hline(yintercept = 50, color = "gray", lty = "dashed", lwd = 1) +
  geom_point(data = obs_tdew_summary, aes(tdew_bin_num, snow_prob * 100), 
             color = "purple") +
  labs(x = expression("Dew Point Temperature ("*degree*C*")"),
       y = "Snowfall Probability (%)")

################################################################################
#################################  Export  #####################################
################################################################################
rs_partition <- list(temp50_tair = temp50_tair,
                     temp50_twet = temp50_twet,
                     temp50_tdew = temp50_tdew,
                     snow_prob_tair = obs_tair_summary,
                     snow_prob_twet = obs_twet_summary,
                     snow_prob_tdew = obs_tdew_summary,
                     snow_pred_tair = prob_tair_predict,
                     snow_pred_twet = prob_twet_predict,
                     snow_pred_tdew = prob_tdew_predict)
saveRDS(object = rs_partition, 
        file = "data/processed/mros_obs_rs_partitioning_2020_2021.RDS")

################################################################################
##################################  Plots  #####################################
################################################################################

# First source the script for plot formats and fills
source("analysis/functions/mros_plot_formats.R")

# Add factor level to phase to force Rain > Mixed > Snow order
obs <- obs %>% mutate(phase = factor(phase, levels = c("Rain", "Mixed", "Snow")))

################################################################################
# Plot rain, snow, mix over time
# All plotted together
phase_cumulative_all_plot <-
  ggplot(obs, aes(datetime, obs_cumsum_all, fill = phase, shape = phase)) +
  geom_point(size = 3, alpha = 0.5) +
  phase_fill_scale +
  phase_shape_scale +
  labs(x = "Date", y = "Total Observations")

# Export
save_plot(plot = phase_cumulative_all_plot,
          filename = paste0(plot_path,
                            "trs_phase_cumulative_all.jpg"),
          base_width = 10, base_height = 6, dpi=300)

################################################################################
# Plot phase distributions by elevation (feet for report-back)
phase_obs_by_elev_plot_ft <-
  ggplot(obs, aes(elev * 3.28, fill = phase)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~phase) + 
  coord_flip() + 
  phase_fill_scale +
  labs(y = "Total Observations",
       x = "Elevation (ft.)") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black"))

# Export
save_plot(plot = phase_obs_by_elev_plot_ft,
          filename = paste0(plot_path,
                            "trs_phase_obs_by_elev_ft.jpg"),
          base_width = 10, base_height = 6, dpi=300)

# Plot phase distributions by elevation (meters)
phase_obs_by_elev_plot_m <-
  ggplot(obs, aes(elev, fill = phase)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~phase) + 
  coord_flip() + 
  phase_fill_scale +
  labs(y = "Total Observations",
       x = "Elevation (m)") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black"))

# Export
save_plot(plot = phase_obs_by_elev_plot_m,
          filename = paste0(plot_path,
                            "trs_phase_obs_by_elev_m.jpg"),
          base_width = 10, base_height = 6, dpi=300)

################################################################################
# Plot phase proportions by elevation bin (in feet for report-back)
phase_pct_by_elev_bin_plot_ft <-
  ggplot(obs, aes(elev_bin, fill = phase)) +
  geom_bar(position = "fill", color = "black") +
  phase_fill_scale +
  labs(x = "Elevation Bin (ft.)",
       y = "Type Proportion (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_discrete(labels = c("< 4100",
                              "4100 - 5740",
                              "5740 - 7380",
                              "> 7380"))

# Export
save_plot(plot = phase_pct_by_elev_bin_plot_ft,
          filename = paste0(plot_path,
                            "trs_phase_pct_by_elev_bin_ft.jpg"),
          base_width = 10, base_height = 6, dpi=300)

# Plot phase proportions by elevation bin (in meters for manuscript)
phase_pct_by_elev_bin_plot_m <-
  ggplot(obs, aes(elev_bin, fill = phase)) +
  geom_bar(position = "fill", color = "black") +
  phase_fill_scale +
  labs(x = "Elevation Bin (m)",
       y = "Phase Proportion (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_discrete(labels = c("< 1250",
                              "1250 - 1750",
                              "1750 - 2250",
                              "> 2250")) +
  theme(legend.title = element_blank())

# Export
save_plot(plot = phase_pct_by_elev_bin_plot_m,
          filename = paste0(plot_path,
                            "trs_phase_pct_by_elev_bin_m.jpg"),
          base_width = 10, base_height = 6, dpi=300)
save_plot(plot = phase_pct_by_elev_bin_plot_m,
          filename = paste0(plot_path,
                            "trs_phase_pct_by_elev_bin_m_NARROW.jpg"),
          base_width = 5.75, base_height = 5, dpi=300)


################################################################################
# Plot obs counts by time of day and day of week
phase_count_by_time_and_day_plot <- 
  plot_grid(
    ggplot(obs, aes(hour)) + 
      geom_histogram(bins = 24, fill = "slateblue1", color = "black") +
      labs(x = "Hour",
           y = "Total Observations"),
    ggplot(obs, aes(day_of_week)) + 
      geom_bar(stat = "count", fill = "slateblue1", color = "black") +
      xlim("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") +
      labs(x = "Day of Week",
           y = "Total Observations") +
      theme(axis.text.x = element_text(angle = -20, hjust = 0),
            plot.margin = margin(10, 15, 10, 10)),
    align = "vh"
  )


# Export
save_plot(plot = phase_count_by_time_and_day_plot,
          filename = paste0(plot_path,
                            "trs_phase_count_by_time_and_day.jpg"),
          base_width = 10, base_height = 6, dpi=300)

################################################################################
# Plot obs counts by day of week



# Plot the number of observations by day of week
ggplot(obs, aes(day_of_week)) + 
  geom_bar(stat = "count") + 
  xlim("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Plot the number of observations by time of day
ggplot(obs, aes(hour)) + 
  geom_histogram(bins = 24)

# Plot phase counts by elevation, split by phase type
plotly::ggplotly(ggplot(obs, aes(elev, fill = phase)) + 
                   geom_histogram(color = "black") + 
                   facet_wrap(~phase) + 
                   coord_flip() + 
                   scale_fill_manual(values= c("purple", "blue", "white")))




obs %>% group_by(elev_bin) %>% summarise(n = n(), snow_prob = sum(phase == "Snow")/n(), rain_prob= sum(phase == "Rain")/n(), mixed_prob = sum(phase == "Mixed")/n())
# A tibble: 4 x 5
elev_bin                n snow_prob rain_prob mixed_prob
<fct>               <int>     <dbl>     <dbl>      <dbl>
  1 [750,1.25e+03]         42     0.429    0.262      0.310 
2 (1.25e+03,1.75e+03]   423     0.638    0.217      0.144 
3 (1.75e+03,2.25e+03]   497     0.763    0.119      0.119 
4 (2.25e+03,2.75e+03]    41     0.927    0.0244     0.0488

