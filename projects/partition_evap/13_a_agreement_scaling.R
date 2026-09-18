# Does grid-scale agreement transfer across scales?
source('source/partition_evap.R')

library(terra)

agreement_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

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

evap_mean_datasets_deg[, q25_1deg := quantile(mean_evap_1deg, 0.25), .(deg_1_lon, deg_1_lat)]
evap_mean_datasets_deg[, q75_1deg := quantile(mean_evap_1deg, 0.75), .(deg_1_lon, deg_1_lat)]
evap_mean_datasets_deg[, mean_1deg := mean(mean_evap_1deg), .(deg_1_lon, deg_1_lat)]
evap_mean_datasets_deg[, median_1deg := quantile(mean_evap_1deg, 0.5), .(deg_1_lon, deg_1_lat)]

evap_mean_datasets_deg[, q25_2deg := quantile(mean_evap_2deg, 0.25), .(deg_2_lon, deg_2_lat)]
evap_mean_datasets_deg[, q75_2deg := quantile(mean_evap_2deg, 0.75), .(deg_2_lon, deg_2_lat)]
evap_mean_datasets_deg[, mean_2deg := mean(mean_evap_2deg), .(deg_2_lon, deg_2_lat)]
evap_mean_datasets_deg[, median_2deg := quantile(mean_evap_2deg, 0.5), .(deg_2_lon, deg_2_lat)]

evap_mean_datasets_deg[, q25_4deg := quantile(mean_evap_4deg, 0.25), .(deg_4_lon, deg_4_lat)]
evap_mean_datasets_deg[, q75_4deg := quantile(mean_evap_4deg, 0.75), .(deg_4_lon, deg_4_lat)]
evap_mean_datasets_deg[, mean_4deg := mean(mean_evap_4deg), .(deg_4_lon, deg_4_lat)]
evap_mean_datasets_deg[, median_4deg := quantile(mean_evap_4deg, 0.5), .(deg_4_lon, deg_4_lat)]


evap_mean_datasets_deg[, q25_8deg := quantile(mean_evap_8deg, 0.25), .(deg_8_lon, deg_8_lat)]
evap_mean_datasets_deg[, q75_8deg := quantile(mean_evap_8deg, 0.75), .(deg_8_lon, deg_8_lat)]
evap_mean_datasets_deg[, mean_8deg := mean(mean_evap_8deg), .(deg_8_lon, deg_8_lat)]
evap_mean_datasets_deg[, median_8deg := quantile(mean_evap_8deg, 0.5), .(deg_8_lon, deg_8_lat)]


evap_mean_datasets_scale_stats <- unique(evap_mean_datasets_deg[,.(lon, lat, deg_1_lon, deg_1_lat,
                                deg_2_lon, deg_2_lat,
                                deg_4_lon, deg_4_lat,
                                deg_8_lon, deg_8_lat,
                                q25_1deg, q75_1deg, mean_1deg, median_1deg,
                                q25_2deg, q75_2deg, mean_2deg, median_2deg,
                                q25_4deg, q75_4deg, mean_4deg, median_4deg,
                                q25_8deg, q75_8deg, mean_8deg, median_8deg
                                )])

evap_mean_datasets_scale_stats[, IQR_1deg := q75_1deg - q25_1deg]
evap_mean_datasets_scale_stats[, IQR_2deg := q75_2deg - q25_2deg]
evap_mean_datasets_scale_stats[, IQR_4deg := q75_4deg - q25_4deg]
evap_mean_datasets_scale_stats[, IQR_8deg := q75_8deg - q25_8deg]

evap_mean_datasets_scale_stats[, sIQR_1deg := (q75_1deg - q25_1deg)/mean_1deg]
evap_mean_datasets_scale_stats[, sIQR_2deg := (q75_2deg - q25_2deg)/mean_2deg]
evap_mean_datasets_scale_stats[, sIQR_4deg := (q75_4deg - q25_4deg)/mean_4deg]
evap_mean_datasets_scale_stats[, sIQR_8deg := (q75_8deg - q25_8deg)/mean_8deg]


agreement_stats_scale <- merge(agreement_grid, evap_mean_datasets_scale_stats[
  ,.(lon, lat, deg_1_lon, deg_1_lat,
     deg_2_lon, deg_2_lat,
     deg_4_lon, deg_4_lat,
     deg_8_lon, deg_8_lat,
     IQR_1deg, IQR_2deg, IQR_4deg, IQR_8deg,
     sIQR_1deg, sIQR_2deg, sIQR_4deg, sIQR_8deg)], by = c("lon", "lat",
                                                          "deg_1_lon", "deg_1_lat",
                                                          "deg_2_lon", "deg_2_lat",
                                                          "deg_4_lon", "deg_4_lat",
                                                          "deg_8_lon", "deg_8_lat"))


agreement_stats_scale[, median_siqr_0_25_to_8 := median(std_quant_range), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_siqr_1_to_8 := median(sIQR_1deg), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_siqr_2_to_8 := median(sIQR_2deg), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_siqr_4_to_8 := median(sIQR_4deg), .(deg_8_lon, deg_8_lat)]

agreement_stats_scale_deg_8 <- unique(agreement_stats_scale[,.(median_siqr_0_25_to_8,
                                                                     median_siqr_1_to_8,
                                                                     median_siqr_2_to_8,
                                                                     median_siqr_4_to_8,
                                                                     sIQR_8deg)])

ggplot(agreement_stats_scale_deg_8)+
  geom_point(aes(x = median_siqr_0_25_to_8, y = sIQR_8deg, col = "0.25"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_1_to_8, y = sIQR_8deg, col = "1"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_2_to_8, y = sIQR_8deg, col = "2"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_4_to_8, y = sIQR_8deg, col = "4"), alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0)+
  theme_bw()


agreement_stats_scale[, median_siqr_0_25_to_8 := median(std_quant_range), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_siqr_0_25_to_4 := median(std_quant_range), .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, median_siqr_0_25_to_2 := median(std_quant_range), .(deg_2_lon, deg_2_lat)]
agreement_stats_scale[, median_siqr_0_25_to_1 := median(std_quant_range), .(deg_1_lon, deg_1_lat)]

ggplot(agreement_stats_scale)+
  geom_point(aes(x = median_siqr_0_25_to_8, y = sIQR_8deg, col = "8"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_0_25_to_4, y = sIQR_4deg, col = "4"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_0_25_to_2, y = sIQR_2deg, col = "2"), alpha = 0.5)+
  geom_point(aes(x = median_siqr_0_25_to_1, y = sIQR_1deg, col = "1"), alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0)+
  theme_bw()

