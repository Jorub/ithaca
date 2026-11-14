# Does grid-scale agreement transfer across scales?
source('source/partition_evap.R')


agreement_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

stopifnot(
  !anyDuplicated(agreement_grid[, .(lon, lat)]),
  !anyDuplicated(evap_mean_datasets[, .(lon, lat, dataset)])
)

agreement_grid <- agreement_grid[,deg_1_lon := -180 + floor(lon+180) + 0.5, ]
agreement_grid <- agreement_grid[,deg_1_lat := -90 + floor(lat + 90) + 0.5, ]
agreement_grid <- agreement_grid[,deg_2_lon := -180 + floor((lon + 180) / 2)*2 + 1, ]
agreement_grid <- agreement_grid[,deg_2_lat := -90 + floor((lat +  90) / 2)*2 + 1, ]
agreement_grid <- agreement_grid[,deg_4_lon := -180 + floor((lon + 180) / 4)*4 + 2, ]
agreement_grid <- agreement_grid[,deg_4_lat := -90 + floor((lat +  90) / 4)*4 + 2, ]
agreement_grid <- agreement_grid[,deg_8_lon := -180 + floor((lon + 180) / 8)*8 + 4, ]
agreement_grid <- agreement_grid[,deg_8_lat := -90 + floor((lat +  90) / 8)*8 + 4, ]

evap_mean_datasets_deg <- merge(evap_mean_datasets, agreement_grid[,.(lon, lat, deg_1_lon, deg_1_lat,
                                                                    deg_2_lon, deg_2_lat,
                                                                    deg_4_lon, deg_4_lat,
                                                                    deg_8_lon, deg_8_lat)], 
                                by = c("lon", "lat"))
evap_mean_datasets_deg[, mean_evap_1deg := sum(evap_mean*area)/sum(area), .(deg_1_lon, deg_1_lat, dataset)]
evap_mean_datasets_deg[, mean_evap_2deg := sum(evap_mean*area)/sum(area), .(deg_2_lon, deg_2_lat, dataset)]
evap_mean_datasets_deg[, mean_evap_4deg := sum(evap_mean*area)/sum(area), .(deg_4_lon, deg_4_lat, dataset)]
evap_mean_datasets_deg[, mean_evap_8deg := sum(evap_mean*area)/sum(area), .(deg_8_lon, deg_8_lat, dataset)]

# 1 degrees: one coarse mean per product before ensemble statistics ----
evap_mean_1deg_unique <- unique(evap_mean_datasets_deg[, .(
  deg_1_lon, deg_1_lat, dataset, mean_evap_1deg
)])
stopifnot(!anyDuplicated(evap_mean_1deg_unique[
  , .(deg_1_lon, deg_1_lat, dataset)
]))

evap_stats_1deg <- evap_mean_1deg_unique[, .(
  q25_1deg = quantile(mean_evap_1deg, 0.25),
  q75_1deg = quantile(mean_evap_1deg, 0.75),
  mean_1deg = mean(mean_evap_1deg),
  median_1deg = median(mean_evap_1deg)
), .(deg_1_lon, deg_1_lat)]
evap_stats_1deg[, IQR_1deg := q75_1deg - q25_1deg]
evap_stats_1deg[, sIQR_1deg := IQR_1deg / median_1deg]

# 2 degrees: one coarse mean per product before ensemble statistics ----
evap_mean_2deg_unique <- unique(evap_mean_datasets_deg[, .(
  deg_2_lon, deg_2_lat, dataset, mean_evap_2deg
)])
stopifnot(!anyDuplicated(evap_mean_2deg_unique[
  , .(deg_2_lon, deg_2_lat, dataset)
]))

evap_stats_2deg <- evap_mean_2deg_unique[, .(
  q25_2deg = quantile(mean_evap_2deg, 0.25),
  q75_2deg = quantile(mean_evap_2deg, 0.75),
  mean_2deg = mean(mean_evap_2deg),
  median_2deg = median(mean_evap_2deg)
), .(deg_2_lon, deg_2_lat)]
evap_stats_2deg[, IQR_2deg := q75_2deg - q25_2deg]
evap_stats_2deg[, sIQR_2deg := IQR_2deg / median_2deg]

# 4 degrees: one coarse mean per product before ensemble statistics ----
evap_mean_4deg_unique <- unique(evap_mean_datasets_deg[, .(
  deg_4_lon, deg_4_lat, dataset, mean_evap_4deg
)])
stopifnot(!anyDuplicated(evap_mean_4deg_unique[
  , .(deg_4_lon, deg_4_lat, dataset)
]))

evap_stats_4deg <- evap_mean_4deg_unique[, .(
  q25_4deg = quantile(mean_evap_4deg, 0.25),
  q75_4deg = quantile(mean_evap_4deg, 0.75),
  mean_4deg = mean(mean_evap_4deg),
  median_4deg = median(mean_evap_4deg)
), .(deg_4_lon, deg_4_lat)]
evap_stats_4deg[, IQR_4deg := q75_4deg - q25_4deg]
evap_stats_4deg[, sIQR_4deg := IQR_4deg / median_4deg]

# 8 degrees: one coarse mean per product before ensemble statistics ----
evap_mean_8deg_unique <- unique(evap_mean_datasets_deg[, .(
  deg_8_lon, deg_8_lat, dataset, mean_evap_8deg
)])
stopifnot(!anyDuplicated(evap_mean_8deg_unique[
  , .(deg_8_lon, deg_8_lat, dataset)
]))

evap_stats_8deg <- evap_mean_8deg_unique[, .(
  q25_8deg = quantile(mean_evap_8deg, 0.25),
  q75_8deg = quantile(mean_evap_8deg, 0.75),
  mean_8deg = mean(mean_evap_8deg),
  median_8deg = median(mean_evap_8deg)
), .(deg_8_lon, deg_8_lat)]
evap_stats_8deg[, IQR_8deg := q75_8deg - q25_8deg]
evap_stats_8deg[, sIQR_8deg := IQR_8deg / median_8deg]

# Rejoin coarse statistics to the original fine-grid cells ----
agreement_stats_scale <- copy(agreement_grid)
agreement_stats_scale <- merge(
  agreement_stats_scale,
  evap_stats_1deg[, .(deg_1_lon, deg_1_lat, IQR_1deg, sIQR_1deg)],
  by = c("deg_1_lon", "deg_1_lat"), all.x = TRUE, sort = FALSE
)
agreement_stats_scale <- merge(
  agreement_stats_scale,
  evap_stats_2deg[, .(deg_2_lon, deg_2_lat, IQR_2deg, sIQR_2deg)],
  by = c("deg_2_lon", "deg_2_lat"), all.x = TRUE, sort = FALSE
)
agreement_stats_scale <- merge(
  agreement_stats_scale,
  evap_stats_4deg[, .(deg_4_lon, deg_4_lat, IQR_4deg, sIQR_4deg)],
  by = c("deg_4_lon", "deg_4_lat"), all.x = TRUE, sort = FALSE
)
agreement_stats_scale <- merge(
  agreement_stats_scale,
  evap_stats_8deg[, .(deg_8_lon, deg_8_lat, IQR_8deg, sIQR_8deg)],
  by = c("deg_8_lon", "deg_8_lat"), all.x = TRUE, sort = FALSE
)
stopifnot(
  nrow(agreement_stats_scale) == nrow(agreement_grid),
  !anyDuplicated(agreement_stats_scale[, .(lon, lat)])
)

agreement_stats_scale[, reduction_1deg := std_quant_range - sIQR_1deg]
agreement_stats_scale[, reduction_2deg := std_quant_range - sIQR_2deg]
agreement_stats_scale[, reduction_4deg := std_quant_range - sIQR_4deg]
agreement_stats_scale[, reduction_8deg := std_quant_range - sIQR_8deg]

saveRDS(agreement_stats_scale, paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))

