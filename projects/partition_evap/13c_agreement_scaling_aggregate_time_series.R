# Does grid-scale agreement transfer across scales? ----
# aggregating cleaned data ----
source('source/partition_evap.R')

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))

evap_datasets <- evap_datasets[,deg_1_lon := -180 + floor(lon+180) + 0.5, ]
evap_datasets <- evap_datasets[,deg_1_lat := -90 + floor(lat + 90) + 0.5, ]
evap_datasets <- evap_datasets[,deg_2_lon := -180 + floor((lon + 180) / 2)*2 + 1, ]
evap_datasets <- evap_datasets[,deg_2_lat := -90 + floor((lat +  90) / 2)*2 + 1, ]
evap_datasets <- evap_datasets[,deg_4_lon := -180 + floor((lon + 180) / 4)*4 + 2, ]
evap_datasets <- evap_datasets[,deg_4_lat := -90 + floor((lat +  90) / 4)*4 + 2, ]
evap_datasets <- evap_datasets[,deg_8_lon := -180 + floor((lon + 180) / 8)*8 + 4, ]
evap_datasets <- evap_datasets[,deg_8_lat := -90 + floor((lat +  90) / 8)*8 + 4, ]

evap_datasets[, evap_1deg := sum(evap*area)/sum(area), .(deg_1_lon, deg_1_lat, year, dataset)]
evap_datasets[, evap_2deg := sum(evap*area)/sum(area), .(deg_2_lon, deg_2_lat, year, dataset)]
evap_datasets[, evap_4deg := sum(evap*area)/sum(area), .(deg_4_lon, deg_4_lat, year, dataset)]
evap_datasets[, evap_8deg := sum(evap*area)/sum(area), .(deg_8_lon, deg_8_lat, year, dataset)]

evap_datasets[, N_1deg := .N, .(deg_1_lon, deg_1_lat, year, dataset)]
evap_datasets[, N_2deg := .N, .(deg_2_lon, deg_2_lat, year, dataset)]
evap_datasets[, N_4deg := .N, .(deg_4_lon, deg_4_lat, year, dataset)]
evap_datasets[, N_8deg := .N, .(deg_8_lon, deg_8_lat, year, dataset)]

evap1_deg <- unique(evap_datasets[, .(deg_1_lon, deg_1_lat, evap_1deg, year, N_1deg, dataset)])
evap2_deg <- unique(evap_datasets[, .(deg_2_lon, deg_2_lat, evap_2deg, year, N_2deg, dataset)])
evap4_deg <- unique(evap_datasets[, .(deg_4_lon, deg_4_lat, evap_4deg, year, N_4deg, dataset)])
evap8_deg <- unique(evap_datasets[, .(deg_8_lon, deg_8_lat, evap_8deg, year, N_8deg, dataset)])

saveRDS(evap1_deg, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_1deg.rds"))
saveRDS(evap2_deg, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_2deg.rds"))
saveRDS(evap4_deg, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_4deg.rds"))
saveRDS(evap8_deg, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_8deg.rds"))

