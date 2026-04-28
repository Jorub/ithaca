source('source/evap_trend.R')

# Trend estimates by product for each grid ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend_sel <- subset(evap_trend, select = c("dataset", "lon", "lat", "p", "slope", "lower", "upper"))
write.csv(evap_trend_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_grid_trends_all_datasets.csv"))
