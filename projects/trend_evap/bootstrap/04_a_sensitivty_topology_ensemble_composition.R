# Topology sensitivity test of ensemble composition ----
source('source/evap_trend.R')
source('source/geo_functions.R')

evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))
evap_index <- evap_index[,.(lat, lon, dataset_leftout, DCI_0_05, trend_0_05, N_pos_0_05, N_neg_0_05, N_none_0_05)]
evap_index[, dataset_leftout := toupper(dataset_leftout)]
evap_index[dataset_leftout == "ETMONITOR", dataset_leftout := "ETMonitor"]
evap_index[dataset_leftout == "SYNTHESIZEDET", dataset_leftout := "SynthesizedET"]
evap_index[dataset_leftout == "ERA5-LAND", dataset_leftout := "ERA5-land"]
evap_index[dataset_leftout == "MERRA2", dataset_leftout := "MERRA-2"]
evap_index[dataset_leftout == "JRA55", dataset_leftout := "JRA-55"]
evap_index[dataset_leftout == "TERRACLIMATE", dataset_leftout := "TerraClimate"]

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))

evap_trend <- evap_trend[, .(lat, lon, dataset, slope, p)]
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]
evap_trend[, dataset := toupper(dataset)]
evap_trend[dataset == "ETMONITOR", dataset := "ETMonitor"]
evap_trend[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
evap_trend[dataset == "ERA5-LAND", dataset := "ERA5-land"]
evap_trend[dataset == "MERRA2", dataset := "MERRA-2"]
evap_trend[dataset == "JRA55", dataset := "JRA-55"]
evap_trend[dataset == "TERRACLIMATE", dataset := "TerraClimate"]

## trend opposer ----
evap_merge <- merge(evap_trend, evap_index, by = c("lon", "lat"),
                    allow.cartesian=TRUE)
evap_merge <- evap_merge[dataset != dataset_leftout]

evap_merge[, opposing_0_05 := 0]
evap_merge[DCI_0_05/slope < 0 & DCI_0_05 != 0, opposing_0_05 := 1]
area_opposing <- evap_merge[opposing_0_05 == 1, .(area_opposing = sum(area)), .(dataset, dataset_leftout)]
area_opposing[, trend_opposer := rank(-area_opposing), .(dataset_leftout)]

mean_rank <- area_opposing[, .(mean = mean(trend_opposer)), .(dataset)]
dataset_order <- mean_rank$dataset[order(mean_rank$mean)]

area_opposing[, dataset := factor(dataset, levels = dataset_order)]


## sensitivity opposer ----

evap_merge[N_none_0_05 > (N_pos_0_05+N_neg_0_05), majority_0_05 := "none"]
evap_merge[(N_pos_0_05+N_neg_0_05) >  N_none_0_05, majority_0_05 := "significant"]

evap_merge[, sig_opposing_0_05 := 0]
evap_merge[(majority_0_05 == "none" & p <= 0.05) | (majority_0_05 == "significant" & p > 0.05 ), sig_opposing_0_05 := 1]
area_sig_opposing <- evap_merge[sig_opposing_0_05 == 1, .(area_sig_opposing = sum(area)), .(dataset, dataset_leftout)]

area_sig_opposing[, sig_opposer := rank(-area_sig_opposing), .(dataset_leftout)]

mean_rank <- area_sig_opposing[, .(mean = mean(sig_opposer)), .(dataset)]
dataset_order <- mean_rank$dataset[order(mean_rank$mean)]

area_sig_opposing[, dataset := factor(dataset, levels = dataset_order)]


## Opposer contributor ----
evap_merge[trend_0_05 == "opposing" & 
             ((N_pos_0_05 == 1 & slope > 0) | (N_neg_0_05 == 1 & slope < 1)) 
           & p < 0.05, opposition_contributor := 1]

area_opp_contributor <- evap_merge[opposition_contributor == 1, .(area_oc_opposing = sum(area)), .(dataset, dataset_leftout)]
area_opp_contributor[, opp_contributor := rank(-area_oc_opposing), .(dataset_leftout)]


mean_rank <- area_opp_contributor[, .(mean = mean(opp_contributor)), .(dataset)]
dataset_order <- mean_rank$dataset[order(mean_rank$mean)]

area_opp_contributor[, dataset := factor(dataset, levels = dataset_order)]


sensitivity_test <- merge(area_opposing, area_sig_opposing, 
                          by = c("dataset", "dataset_leftout"))

sensitivity_test <- merge(sensitivity_test, area_opp_contributor, 
                          by = c("dataset", "dataset_leftout"))

saveRDS(sensitivity_test, paste0(PATH_SAVE_EVAP_TREND, "global_oppositional_topology_sensitivity_analysis_ensemble_composition_a.rds"))
