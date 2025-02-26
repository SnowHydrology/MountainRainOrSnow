# Plots for IPWG presentation on 2025-02-19
# Based on data and analyses from the Nature Communications manuscript

# Load R packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(scales)

# Import data
data_pre <- "../../data/"
summary_bytemp <- readRDS(paste0(data_pre, "performance_summary_bytemp.RDS")) %>% 
  mutate(source_lab = ifelse(source == "cs", "Crowdsourced", "Synoptic"))

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
  filter(!ppm %in% c("xg", "rf", "nn")) %>% 
  left_join(ppm_labels, by = "ppm")

# Make plot
plot_grid(
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = accuracy_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev) +
    labs(x = "Air temperature (°C)") +
    scale_fill_gradientn(colors = hcl.colors(3, "Heat2"), name = "Accuracy\n(%)") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = snow_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Snow Bias\n(%)")  +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = rain_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Rain Bias\n(%)") +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev),
  nrow = 3, labels = "auto", rel_heights = c(0.8, 0.8, 1)
)


# Now plot ML results

# Filter data to plot and join labels
plot_data <- summary_bytemp %>% 
  filter(scenario == "nomix_imbal" & source == "cs" &
           tair_bin >= -5 & tair_bin <= 10) %>% 
  filter(ppm %in% c("xg", "rf", "nn")) %>% 
  left_join(ppm_labels, by = "ppm")

# Plot the ML results
plot_grid(
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = accuracy_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev) +
    labs(x = "Air temperature (°C)") +
    scale_fill_gradientn(colors = hcl.colors(3, "Heat2"), name = "Accuracy\n(%)",
                         limits = c(27.38386,100)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = snow_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Snow Bias\n(%)")  +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()),
  ggplot(plot_data, aes(tair_bin, ppm_labs, fill = rain_bias_pct)) + 
    geom_raster() + 
    geom_vline(xintercept = 0, lty = "dashed") +
    scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                         colors = hcl.colors(3, "Blue-Red2", rev = T), name = "Rain Bias\n(%)") +
    labs(x = "Air temperature (°C)") +
    scale_y_discrete("PPM", labels = rev(parse(text = unique(plot_data$ppm_labs))),
                     limits = rev),
  nrow = 3, labels = "auto", rel_heights = c(0.8, 0.8, 1)
)


# Make the comparison plot

# Summarize for each dataset
ml_benchmark_diff_bytemp <- bind_rows(
  summary_bytemp %>% 
    filter(source == "cs" & scenario == "nomix_imbal") %>% 
    pivot_wider(names_from = ppm, values_from = accuracy_pct, id_cols = tair_bin) %>% 
    mutate(rf_best = (rf-thresh_twet_1)/thresh_twet_1*100,
           xg_best = (xg-thresh_twet_1)/thresh_twet_1*100,
           nn_best = (nn-thresh_twet_1)/thresh_twet_1*100) %>% 
    select(tair_bin, rf_best:nn_best) %>% 
    pivot_longer(cols = rf_best:nn_best,
                 names_to = c("ppm", "benchmark"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "accuracy_diff_rel_pct") %>% 
    mutate(source_lab = "Crowdsourced", source = "cs")
)


ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line(data = ml_benchmark_diff_bytemp, lwd = 1, 
            aes(tair_bin, accuracy_diff_rel_pct, color = ppm)) +
  scale_color_manual(values =  c("#4a6fe3", "gray26", "#d33f6a"),
                     labels = c("ANN","RF", "XG"),
                     name = "PPM")+
  labs(x = "Air temperature (°C)", 
       y = "Relative accuracy difference to benchmark (%)") +
  xlim(c(-5,10)) +
  theme(legend.position = c(0.03,0.7))

ml_benchmark_rel_diff_plot
save_plot(filename = "figures/fig02_ml_benchmark_rel_diff.png", 
          plot = ml_benchmark_rel_diff_plot, 
          base_height = 5, 
          base_width = 9)
