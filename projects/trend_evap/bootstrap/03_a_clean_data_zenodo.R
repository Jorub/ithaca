source('source/evap_trend.R')

# Trend estimates by product for each grid ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend_sel <- subset(evap_trend, select = c("dataset", "lon", "lat", "p", "slope", "lower", "upper"))
write.csv(evap_trend_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_grid_trends_all_datasets.csv"))

# mask
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_masks <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_masks_sel <- subset(evap_masks, select = c("lon", "lat", "elev_class", "KG_beck_v2",
                                                "IPCC_ref_region",
                                                "evap_quant"))
saveRDS(evap_masks_sel, paste0(PATH_SAVE_EVAP_TREND, "evap_mask.rds"))
write.csv(evap_masks_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "evap_mask.csv"))
