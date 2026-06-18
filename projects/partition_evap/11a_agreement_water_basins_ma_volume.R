# Agreement over water basins defined in Ma et al. 2023
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid[, count := .N, .(lat,lon)]
evap_grid <- evap_grid[count == 14]
evap_grid[, count := NULL]
evap_mean <- evap_grid[, .(evap_volume = mean(evap_volume)), .(lat, lon)]

## Variables
basins <- merge(evap_mask[, .(lat, lon, ma_basin)], 
                    evap_grid[, .(lon, lat, area, evap_volume, dataset)], by = c("lon", "lat"), all.y = TRUE)
basins <- basins[complete.cases(basins)]
basins_stats <- basins[, .(environment_volume = round(sum(evap_volume, na.rm = T),)), .(ma_basin, dataset)]
partition_basins_datasets <- dcast(basins_stats, dataset ~ ma_basin, fun = mean, na.rm = TRUE)
global <- basins_stats[, .(Global = sum(environment_volume)), .(dataset)]
basins_merged <- merge(partition_basins_datasets, global, by = c("dataset"))

saveRDS(basins_merged, paste0(PATH_SAVE_PARTITION_EVAP, "partition_basins_datasets_global.rds"))

basin_agreement <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, ma_basin)], evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
basin_agreement_sum <- basin_agreement[, .(evap_volume_sum = sum(evap_volume)),.(rel_dataset_agreement,ma_basin)]
basin_agreement_sum[, evap_volume_fraction := evap_volume_sum/sum(evap_volume_sum),.(ma_basin)]
basin_agreement_sum <- basin_agreement_sum[complete.cases(basin_agreement_sum)]

basin_agreement_sum[rel_dataset_agreement == 'high' , rel_dataset_agreement := 'High']
basin_agreement_sum[rel_dataset_agreement == 'above average' , rel_dataset_agreement := 'Above average']
basin_agreement_sum[rel_dataset_agreement == 'average' , rel_dataset_agreement := 'Average']
basin_agreement_sum[rel_dataset_agreement == 'below average' , rel_dataset_agreement := 'Below average']
basin_agreement_sum[rel_dataset_agreement == 'low' , rel_dataset_agreement := 'Low']

saveRDS(basin_agreement_sum, paste0(PATH_SAVE_PARTITION_EVAP, "quartile_agreement_ma_basins.rds"))



# distribution agreement ----
## Data ----
evap_grid <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_gridwise.rds")))
evap_grid <- evap_grid[!is.na(agreement_index),]
evap_grid[, summary(agreement_index)]

evap_vol_mean <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_mean <- evap_vol_mean[, .(evap_volume = mean(evap_volume)), .(lat, lon)]

## Analysis ----
### Relative Distribution\nagreement at quantile 0.1, 0.3, 0.7. 0.9 ----
quant_thr_0_1 <- quantile(evap_grid$agreement_index, c(0.1))
quant_thr_0_3 <- quantile(evap_grid$agreement_index, c(0.3))
quant_thr_0_7 <- quantile(evap_grid$agreement_index, c(0.7))
quant_thr_0_9 <- quantile(evap_grid$agreement_index, c(0.9))

evap_grid[agreement_index > quant_thr_0_9, agreement_fac := ordered(1, labels = "High")]
evap_grid[agreement_index > quant_thr_0_7 & agreement_index <= quant_thr_0_9, agreement_fac := ordered(2, labels = "Above average")]
evap_grid[agreement_index > quant_thr_0_3 & agreement_index <= quant_thr_0_7, agreement_fac := ordered(3, labels = "Average")]
evap_grid[agreement_index > quant_thr_0_1 & agreement_index <= quant_thr_0_3, agreement_fac := ordered(4, labels = "Below average")]
evap_grid[agreement_index <= quant_thr_0_1, agreement_fac := ordered(5, labels = "Low")] 


evap_grid <- evap_mean[evap_grid, on = .(lon, lat)]

evap_grid <- evap_grid[complete.cases(evap_grid),]

ma_basins_distribution <- merge(evap_mask[, .(lat, lon, ma_basin)], evap_grid[, .(lon, lat, evap_volume, agreement_index, agreement_fac)], by = c("lon", "lat"))
ma_basins_distribution_agreement <- ma_basins_distribution[, .(evap_sum = sum(evap_volume)), .(agreement_fac, ma_basin)]
ma_basins_distribution_agreement <- ma_basins_distribution_agreement[complete.cases(ma_basins_distribution_agreement)]
ma_basins_distribution_agreement <- ma_basins_distribution_agreement[order(agreement_fac, ma_basin), ]
ma_basins_distribution_agreement[, evap_volume_sum := sum(evap_sum), ma_basin]
ma_basins_distribution_agreement[, evap_volume_fraction := evap_sum / evap_volume_sum]

saveRDS(ma_basins_distribution_agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_ma_basins.rds"))
