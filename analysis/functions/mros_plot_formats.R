# Script for plot colors, etc. for Tahoe Rain or Snow

# This script will get sourced before other plots to make sure
# all have the same reproducible format

# Keith Jennings
# ksj.hydro@gmail.com
# 2020-06-08

# Identify the path for saving plots
plot_path = "figures/"

#  Make fill and color parameters for rain, mixed, snow
phase_fill_scale <- scale_fill_manual(name = "Type", 
                                      values = c("blue", "purple", "white"))

phase_color_scale <- scale_color_manual(name = "Type", 
                                      values = c("blue", "purple", "white"))

# Make shape scale for rain, mixed, snow
phase_shape_scale = scale_shape_manual(name = "Type",
                                       values = c(23, 25, 21))
