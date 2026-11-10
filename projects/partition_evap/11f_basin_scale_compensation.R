source('source/partition_evap.R')
source('source/geo_functions.R')

agreement_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

evap_merge <- merge(evap_mean_datasets, agreement_grid[,.(lon, lat, area, ma_basin)], by = c("lon", "lat", "area"))

evap_merge[]
                                                                      