# Agreement over water basins defined in Ma et al. 2023
source('source/partition_evap.R')

dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
ma_grid_data <- dataset_agreement_grid_wise[!is.na(ma_basin)]

saveRDS(ma_grid_data, paste0(PATH_SAVE_PARTITION_EVAP, "ma_basin_agreement_environments_grid_wise.rds"))

agreement_ma <- ma_grid_data[, .(area_agreement = sum(area)), .(ma_basin, rel_dataset_agreement, dist_dataset_agreement)]
agreement_ma[, area_basin := sum(area_agreement), .(ma_basin)]
agreement_ma[, area_fraction := area_agreement/area_basin, ]

higher <- c("High", "Above average")
agreement_rel_da_high <- agreement_ma[rel_dataset_agreement %in% "High", .(rel_da_high = sum(area_fraction)), .(ma_basin)]
agreement_rel_da_higher <- agreement_ma[rel_dataset_agreement %in% higher, .(rel_da_higher = sum(area_fraction)), .(ma_basin)]
agreement_dist_da_high <- agreement_ma[dist_dataset_agreement %in% "High", .(dist_da_high = sum(area_fraction)), .(ma_basin)]
agreement_dist_da_higher <- agreement_ma[dist_dataset_agreement %in% higher, .(dist_da_higher = sum(area_fraction)), .(ma_basin)]
agreement_da_joint <- agreement_ma[dist_dataset_agreement %in% higher & rel_dataset_agreement %in% higher, .(joint_da_higher = sum(area_fraction)), .(ma_basin)]

agreement_summary <- merge(agreement_rel_da_high, agreement_rel_da_higher, 
                           by = "ma_basin", all = T)

agreement_summary <- merge(agreement_summary , agreement_dist_da_high, 
                           by = "ma_basin", all = T)

agreement_summary <- merge(agreement_summary , agreement_dist_da_higher, 
                           by = "ma_basin", all = T)

agreement_summary <- merge(agreement_summary , agreement_da_joint, 
                           by = "ma_basin", all = T)

agreement_summary[is.na(agreement_summary)] <- 0

saveRDS(agreement_summary, paste0(PATH_SAVE_PARTITION_EVAP, "high_agreement_ma_basins_area.rds"))
