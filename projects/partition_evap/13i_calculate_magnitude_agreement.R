source('source/partition_evap.R')

agreement_stats_scale <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))

agreement_8deg <- 
  unique(agreement_stats_scale[,.(sIQR_8deg, deg_8_lon, deg_8_lat),])

quant_thr_0_1 <- quantile(agreement_8deg$sIQR_8deg, c(0.1))
quant_thr_0_3 <- quantile(agreement_8deg$sIQR_8deg, c(0.3))
quant_thr_0_7 <- quantile(agreement_8deg$sIQR_8deg, c(0.7))
quant_thr_0_9 <- quantile(agreement_8deg$sIQR_8deg, c(0.9))

agreement_8deg[sIQR_8deg > quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "Low")]
agreement_8deg[sIQR_8deg > quant_thr_0_7 & sIQR_8deg <= quant_thr_0_9, rel_dataset_agreement := ordered(4, labels = "Below average")]
agreement_8deg[sIQR_8deg > quant_thr_0_3 & sIQR_8deg <= quant_thr_0_7, rel_dataset_agreement := ordered(3, labels = "Average")]
agreement_8deg[sIQR_8deg > quant_thr_0_1 & sIQR_8deg <= quant_thr_0_3, rel_dataset_agreement := ordered(2, labels = "Above average")]
agreement_8deg[sIQR_8deg <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "High")] 

saveRDS(agreement_8deg, paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_8deg.rds"))

agreement_4deg <- 
  unique(agreement_stats_scale[,.(sIQR_4deg, deg_4_lon, deg_4_lat),])

quant_thr_0_1 <- quantile(agreement_4deg$sIQR_4deg, c(0.1))
quant_thr_0_3 <- quantile(agreement_4deg$sIQR_4deg, c(0.3))
quant_thr_0_7 <- quantile(agreement_4deg$sIQR_4deg, c(0.7))
quant_thr_0_9 <- quantile(agreement_4deg$sIQR_4deg, c(0.9))

agreement_4deg[sIQR_4deg > quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "Low")]
agreement_4deg[sIQR_4deg > quant_thr_0_7 & sIQR_4deg <= quant_thr_0_9, rel_dataset_agreement := ordered(4, labels = "Below average")]
agreement_4deg[sIQR_4deg > quant_thr_0_3 & sIQR_4deg <= quant_thr_0_7, rel_dataset_agreement := ordered(3, labels = "Average")]
agreement_4deg[sIQR_4deg > quant_thr_0_1 & sIQR_4deg <= quant_thr_0_3, rel_dataset_agreement := ordered(2, labels = "Above average")]
agreement_4deg[sIQR_4deg <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "High")] 

saveRDS(agreement_4deg, paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_4deg.rds"))

agreement_2deg <- 
  unique(agreement_stats_scale[,.(sIQR_2deg, deg_2_lon, deg_2_lat),])

quant_thr_0_1 <- quantile(agreement_2deg$sIQR_2deg, c(0.1))
quant_thr_0_3 <- quantile(agreement_2deg$sIQR_2deg, c(0.3))
quant_thr_0_7 <- quantile(agreement_2deg$sIQR_2deg, c(0.7))
quant_thr_0_9 <- quantile(agreement_2deg$sIQR_2deg, c(0.9))

agreement_2deg[sIQR_2deg > quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "Low")]
agreement_2deg[sIQR_2deg > quant_thr_0_7 & sIQR_2deg <= quant_thr_0_9, rel_dataset_agreement := ordered(4, labels = "Below average")]
agreement_2deg[sIQR_2deg > quant_thr_0_3 & sIQR_2deg <= quant_thr_0_7, rel_dataset_agreement := ordered(3, labels = "Average")]
agreement_2deg[sIQR_2deg > quant_thr_0_1 & sIQR_2deg <= quant_thr_0_3, rel_dataset_agreement := ordered(2, labels = "Above average")]
agreement_2deg[sIQR_2deg <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "High")] 

saveRDS(agreement_2deg, paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_2deg.rds"))

agreement_1deg <- 
  unique(agreement_stats_scale[,.(sIQR_1deg, deg_1_lon, deg_1_lat),])

quant_thr_0_1 <- quantile(agreement_1deg$sIQR_1deg, c(0.1))
quant_thr_0_3 <- quantile(agreement_1deg$sIQR_1deg, c(0.3))
quant_thr_0_7 <- quantile(agreement_1deg$sIQR_1deg, c(0.7))
quant_thr_0_9 <- quantile(agreement_1deg$sIQR_1deg, c(0.9))

agreement_1deg[sIQR_1deg > quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "Low")]
agreement_1deg[sIQR_1deg > quant_thr_0_7 & sIQR_1deg <= quant_thr_0_9, rel_dataset_agreement := ordered(4, labels = "Below average")]
agreement_1deg[sIQR_1deg > quant_thr_0_3 & sIQR_1deg <= quant_thr_0_7, rel_dataset_agreement := ordered(3, labels = "Average")]
agreement_1deg[sIQR_1deg > quant_thr_0_1 & sIQR_1deg <= quant_thr_0_3, rel_dataset_agreement := ordered(2, labels = "Above average")]
agreement_1deg[sIQR_1deg <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "High")] 

saveRDS(agreement_1deg, paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_1deg.rds"))

