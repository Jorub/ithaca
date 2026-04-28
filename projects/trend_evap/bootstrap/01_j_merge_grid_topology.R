# Merge grid-wise topologies ----
source('source/evap_trend.R')

# Signal boosters ----
## Positive signal booster, negative signal booster and signal dampener
evap_trend_summary <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_trends_summary_p_val_lon_lat_dataproducts.rds"))

## p-value 0.05 ----
grid_topology <- evap_trend_summary[ ,.(lat, lon, area, dataset, N_pos_0_05, N_neg_0_05, N_none_0_05)]
setnames(grid_topology, old = c("N_pos_0_05", "N_neg_0_05", "N_none_0_05"), 
         new = c("positive_signal", "negative_signal", "dampener"))

# Opposition contributor ----
evap_opposing <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_contributing_opposition.rds"))
## p-value 0.05 ----
evap_opposing_sel <- evap_opposing[variable == "p <= 0.05"]
setnames(evap_opposing_sel, old = c("dataset_leftout"), 
         new = c("dataset"))

grid_topology <- merge(grid_topology, 
                       evap_opposing_sel[,.(lat, lon, area, dataset, contributing_opposition)], 
                       by = c("lon", "lat", "area", "dataset"), all = T)

# DCI opposor ----
evap_opposing_DCI <-  readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_dataset_opposing_DCI.rds"))
evap_opposing_DCI_sel <- evap_opposing_DCI[,.(lon, lat, dataset, opposing_0_05)]

## Simplify values 1 == opposing, all other not opposing ----
evap_opposing_DCI_sel[opposing_0_05 !=1 , opposing_0_05 := 0]
setnames(evap_opposing_DCI_sel, old = c("opposing_0_05"), 
         new = c("opposing_majority_trend"))

grid_topology <- merge(grid_topology, evap_opposing_DCI_sel, by = c("lon", "lat", "dataset"), all = T)

# Significance opposor ----
evap_opposing_significance <-  readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_dataset_opposing_significance.rds"))

evap_opposing_significance_sel <-  evap_opposing_significance[,.(lon, lat, dataset, opposing_0_05)]
setnames(evap_opposing_significance_sel, old = c("opposing_0_05"), 
         new = c("opposing_significance"))

grid_topology <- merge(grid_topology, evap_opposing_significance_sel, by = c("lon", "lat", "dataset"), all = T)

saveRDS(grid_topology, paste0(PATH_SAVE_EVAP_TREND, "grid_dataset_trend_topology.rds"))
write.table(grid_topology, paste0(PATH_SAVE_EVAP_TREND_TABLES, "grid_dataset_trend_topology.csv"))
