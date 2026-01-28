# Addition of categorical classes to each grid cell 
source('source/partition_evap.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))
evap_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))

## Masks
### Evaporation
evap_stats[, evap_quant := ordered(quantcut(ens_mean_mean, 10), 
                                   labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',     #Caution: This is biased due to different grid size
                                              '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]

### Relative dataset agreement at quantile 0.1, 0.3, 0.7. 0.9
quant_thr_0_1 <- quantile(evap_stats$std_quant_range, c(0.1))
quant_thr_0_3 <- quantile(evap_stats$std_quant_range, c(0.3))
quant_thr_0_7 <- quantile(evap_stats$std_quant_range, c(0.7))
quant_thr_0_9 <- quantile(evap_stats$std_quant_range, c(0.9))

thresholds_sIQR <- c(min(evap_stats$std_quant_range),quant_thr_0_1, quant_thr_0_3, quant_thr_0_7, quant_thr_0_9, max(evap_stats$std_quant_range))
saveRDS(thresholds_sIQR, paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_sIQR.RDS'))

evap_stats[std_quant_range <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "high")] 
evap_stats[std_quant_range > quant_thr_0_1 & std_quant_range <= quant_thr_0_3, rel_dataset_agreement := ordered(3, labels = "above average")]
evap_stats[std_quant_range > quant_thr_0_3 & std_quant_range <= quant_thr_0_7, rel_dataset_agreement := ordered(4, labels = "average")]
evap_stats[std_quant_range > quant_thr_0_7 & std_quant_range <= quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "below average")]
evap_stats[std_quant_range > quant_thr_0_9, rel_dataset_agreement := ordered(7, labels = "low")]


evap_masks <- merge(masks_global, evap_stats[, .(lon, lat, evap_quant, rel_dataset_agreement, #Merges only complete cases
                               std_quant_range, ens_mean_q25, ens_mean_mean, ens_mean_q75)], by = c("lon", "lat"))


evap_masks[, grid_count_ipcc := .N, IPCC_ref_region]
evap_masks[grid_count_ipcc < 10, IPCC_ref_region := NA]
evap_masks[grepl("O", as.character(IPCC_ref_region)) == TRUE, ocean := "yes"]
evap_masks[IPCC_ref_region %in% c("BOB", "ARS"), ocean := "yes"]
evap_masks[ocean == "yes", IPCC_ref_region:= NA]

evap_masks[,ocean:= NULL]
evap_masks[,grid_count_ipcc:= NULL]


## Save data
saveRDS(evap_masks, paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

write.csv(evap_masks, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "evap_masks.csv"))
