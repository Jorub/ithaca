# Grid-cell dataset roles ----
source('source/evap_trend.R')

## Data ----
### Input data generated in trend_evap/bootstrap/01_c and trend_evap/bootstrap/01_d
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))  
evap_trend_summary <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

evap_trend[, dataset := toupper(dataset)]
evap_trend[dataset == "ETMONITOR", dataset := "ETMonitor"]
evap_trend[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
evap_trend[dataset == "ERA5-LAND", dataset := "ERA5-land"]
evap_trend[dataset == "MERRA2", dataset := "MERRA-2"]
evap_trend[dataset == "JRA55", dataset := "JRA-55"]
evap_trend[dataset == "TERRACLIMATE", dataset := "TerraClimate"]


### Positive trend booster ----
evap_trend[, pos_signal := 0]
evap_trend[slope > 0 & p <= 0.2, pos_signal := 1]

### Negative trend booster ----
evap_trend[, neg_signal := 0]
evap_trend[slope < 0 & p <= 0.2, neg_signal := 1]

### Dampener ----
evap_trend[, signal_dampener := 0]
evap_trend[p > 0.2, signal_dampener:= 1]

### merge ensemble stats ---
evap_trend_merge <- merge(evap_trend, 
                          evap_trend_summary[,.(lon, lat, DCI_0_2, N_sig_0_2, N_none_0_2, 
                                                N_pos_0_2, N_neg_0_2, trend_0_2)], 
                          by = c("lon", "lat"))

evap_trend_merge[, opposing_majority_trend := 0]
evap_trend_merge[DCI_0_2/slope < 0 & DCI_0_2 != 0, opposing_majority_trend := 1]
evap_trend_merge[, opposing_significance := 0]
evap_trend_merge[(N_sig_0_2 > N_none_0_2) & p > 0.2, opposing_significance := 1]
evap_trend_merge[(N_none_0_2 > N_sig_0_2) & p <= 0.2, opposing_significance := 1]

evap_trend_merge[, opposition_contributor := 0]
evap_trend_merge[trend_0_2 == "opposing" & 
                   ((N_pos_0_2 == 1 & slope > 0) | (N_neg_0_2 == 1 & slope < 1)) 
                 & p < 0.2, opposition_contributor := 1]

saveRDS(evap_trend_merge[,.(lon, lat, dataset, pos_signal, neg_signal, signal_dampener,
                            opposing_majority_trend, opposing_significance,
                            opposition_contributor)], 
        paste0(PATH_SAVE_EVAP_TREND, "global_grid_cell_topology_0_2.rds"))
