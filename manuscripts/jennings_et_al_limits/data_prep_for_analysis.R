# Pare down data for analysis

library(tidyverse)

# Data prefix
data_pre <- "../../data/"

# Import the 'predict' objects that have predictions, met data, model objects, etc
rf_predict_cs <- readRDS(paste0(data_pre, "rf_predict_imbal_CS.RDS"))
rf_predict_nh <- readRDS(paste0(data_pre, "rf_predict_imbal_NH.RDS"))
xg_predict_cs <- readRDS(paste0(data_pre, "xg_predict_imbal_CS.RDS"))
xg_predict_nh <- readRDS(paste0(data_pre, "xg_predict_imbal_NH.RDS"))

# Export the model objects to use as variable importance plots
export_list <- list(
  xg_cs <- xg_predict_cs$nomix_imbal$xg_final_fit,
  rf_cs <- rf_predict_cs$nomix_imbal$rf_final_fit,
  xg_nh <- xg_predict_nh$nomix_imbal$rf_final_fit,
  rf_nh <- rf_predict_nh$nomix_imbal$rf_final_fit
)
saveRDS(export_list, paste0(data_pre, "ml_models_for_vip.RDS"))

# Export the met data to use in analysis
met <- bind_rows(
  xg_predict_cs$nomix_imbal$xg_preds %>% 
    select(phase, tair, twet, scenario, source),
  xg_predict_cs$allphase_imbal$xg_preds %>% 
    select(phase, tair, twet, scenario, source),
  xg_predict_nh$nomix_imbal$rf_preds %>% 
    select(phase, tair = Air_Temp, twet, source) %>% 
    mutate(scenario = "nomix_imbal")
)
saveRDS(met, paste0(data_pre, "cs_nh_met_data.RDS"))
