# Figures for WMO presentation

# Keith Jennings
# 2025-04-22

# Load R packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(scales)

# Import data
data_pre <- "../../data/"
summary_bytemp <- readRDS(paste0(data_pre, "performance_summary_bytemp.RDS")) %>% 
  mutate(source_lab = ifelse(source == "cs", "Crowdsourced", "Synoptic"))
summary_all <- readRDS(paste0(data_pre, "performance_summary_all.RDS"))%>% 
  mutate(source_lab = ifelse(source == "cs", "Crowdsourced", "Synoptic"))

# Rank the methods 
ranks <-
  summary_all %>% 
  group_by(source, scenario) %>% 
  mutate(accuracy_rank = row_number(-accuracy_pct), 
         rain_bias_rank = row_number(abs(rain_bias_pct)),
         snow_bias_rank = row_number(abs(snow_bias_pct))) %>% 
  select(source, scenario, ppm, accuracy_rank:snow_bias_rank)

# Join to the summary data
summary_bytemp <- left_join(summary_bytemp, ranks,
                             by = c("source", "scenario", "ppm"))

# Make a vertical benchmark accuracy plot
# Make PPM labels
ppm_labels <- 
  data.frame(
    ppm = c(
      "binlog",
      "nn",
      "rf",
      "thresh_tair_1",
      "thresh_tair_1.5",
      "thresh_tdew_0",
      "thresh_tdew_0.5",
      "thresh_twet_0",
      "thresh_twet_0.5",
      "thresh_twet_1",
      "xg"
    ),
    ppm_labs = c(
      "Bin[log]",
      "ANN",
      "RF",
      "T[a1.0]",
      "T[a1.5]",
      "T[d0.0]",
      "T[d0.5]",
      "T[w0.0]",
      "T[w0.5]",
      "T[w1.0]",
      "XG"
    )
  )

# Filter data to plot and join labels
plot_data <- summary_bytemp %>% 
  filter(scenario == "nomix_imbal" & source == "cs" &
           tair_bin >= -5 & tair_bin <= 10) %>% 
  left_join(ppm_labels, by = "ppm")
plot_ranks <- filter(ranks, source == "cs" & scenario == "nomix_imbal") %>% 
  left_join(., ppm_labels, by = "ppm")

# Make plot
plot_grid(
  ggplot(plot_data, aes(tair_bin, reorder(ppm_labs, -accuracy_rank), fill = accuracy_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_y_discrete("PPM", labels = parse(text = arrange(plot_ranks, -accuracy_rank)$ppm_labs)) +
    labs(x = "Air temperature (°C)") +
    scale_fill_gradientn(colors = hcl.colors(3, "Heat2"), name = "Accuracy\n(%)") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, reorder(ppm_labs, -snow_bias_rank), fill = snow_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Snow Bias\n(%)")  +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = parse(text = arrange(plot_ranks, -snow_bias_rank)$ppm_labs)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, reorder(ppm_labs, -rain_bias_rank), fill = rain_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Rain Bias\n(%)") +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = parse(text = arrange(plot_ranks, -rain_bias_rank)$ppm_labs)),
  nrow = 3, labels = "auto", rel_heights = c(0.8, 0.8, 1)
)
