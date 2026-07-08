# Agreement over water basins defined in Ma et al. 2023
source('source/partition_evap.R')

dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))


ma_grid_data <- dataset_agreement_grid_wise[!is.na(ma_basin)]
ma_grid_data[, .(
  cor_area_volume = cor(area, evap_volume, use = "complete.obs"),
  mean_abs_diff_fraction = mean(abs(
    area / sum(area, na.rm = TRUE) -
      evap_volume / sum(evap_volume, na.rm = TRUE)
  ), na.rm = TRUE)
), by = ma_basin][order(-cor_area_volume)]

ma_grid_data[, volume_per_area := evap_volume / area]

rel_class_diag <- ma_grid_data[
  !is.na(ma_basin) & !is.na(rel_dataset_agreement),
  .(
    area = sum(area, na.rm = TRUE),
    volume = sum(evap_volume, na.rm = TRUE)
  ),
  by = .(ma_basin, rel_dataset_agreement)
]

rel_class_diag[, `:=`(
  area_share = area / sum(area, na.rm = TRUE),
  volume_share = volume / sum(volume, na.rm = TRUE),
  evap_intensity_class = volume / area,
  evap_intensity_basin = sum(volume, na.rm = TRUE) / sum(area, na.rm = TRUE),
  evap_ratio_class_to_basin = (volume / area) /
    (sum(volume, na.rm = TRUE) / sum(area, na.rm = TRUE))
), by = ma_basin]

rel_class_diag[,   difference := volume_share - area_share,]
rel_class_diag[order(ma_basin, rel_dataset_agreement)]

saveRDS(ma_grid_data, paste0(PATH_SAVE_PARTITION_EVAP, "ma_basin_agreement_environments_grid_wise.rds"))

agreement_ma <- ma_grid_data[, .(area_agreement = sum(area), volume_agreement = sum(evap_volume)), .(ma_basin, rel_dataset_agreement, dist_dataset_agreement)]

agreement_ma[, `:=` (area_basin = sum(area_agreement),
                     volume_basin = sum(volume_agreement)), .(ma_basin)]

agreement_ma[, `:=` (area_fraction = area_agreement/area_basin,
                     volume_fraction = volume_agreement/volume_basin), ]

higher <- c("High", "Above average")

agreement_da_all <- agreement_ma[, {
  rel_high    <- rel_dataset_agreement == "High"
  rel_higher  <- rel_dataset_agreement %in% higher
  dist_high   <- dist_dataset_agreement == "High"
  dist_higher <- dist_dataset_agreement %in% higher
  joint_higher <- rel_higher & dist_higher
  
  .(
    rel_da_high        = sum(area_fraction[rel_high], na.rm = TRUE),
    rel_da_higher      = sum(area_fraction[rel_higher], na.rm = TRUE),
    dist_da_high       = sum(area_fraction[dist_high], na.rm = TRUE),
    dist_da_higher     = sum(area_fraction[dist_higher], na.rm = TRUE),
    joint_da_higher    = sum(area_fraction[joint_higher], na.rm = TRUE),
    
    rel_da_high_vol     = sum(volume_fraction[rel_high], na.rm = TRUE),
    rel_da_higher_vol   = sum(volume_fraction[rel_higher], na.rm = TRUE),
    dist_da_high_vol    = sum(volume_fraction[dist_high], na.rm = TRUE),
    dist_da_higher_vol  = sum(volume_fraction[dist_higher], na.rm = TRUE),
    joint_da_higher_vol = sum(volume_fraction[joint_higher], na.rm = TRUE)
  )
}, by = ma_basin]


saveRDS(agreement_summary, paste0(PATH_SAVE_PARTITION_EVAP, "high_agreement_ma_basins_area.rds"))
