
source('source/partition_evap.R')

agreement_8deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise_8deg.rds"))

quant_thr_0_1 <- quantile(agreement_8deg$index, c(0.1))
quant_thr_0_3 <- quantile(agreement_8deg$index, c(0.3))
quant_thr_0_7 <- quantile(agreement_8deg$index, c(0.7))
quant_thr_0_9 <- quantile(agreement_8deg$index, c(0.9))

agreement_8deg[index > quant_thr_0_9, dist_dataset_agreement := ordered(1, labels = "High")]
agreement_8deg[index > quant_thr_0_7 & index <= quant_thr_0_9, dist_dataset_agreement := ordered(2, labels = "Above average")]
agreement_8deg[index > quant_thr_0_3 & index <= quant_thr_0_7, dist_dataset_agreement := ordered(3, labels = "Average")]
agreement_8deg[index > quant_thr_0_1 & index <= quant_thr_0_3, dist_dataset_agreement := ordered(4, labels = "Below average")]
agreement_8deg[index <= quant_thr_0_1, dist_dataset_agreement := ordered(5, labels = "Low")] 

saveRDS(agreement_8deg, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_8deg.rds"))

agreement_4deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise_4deg.rds"))

quant_thr_0_1 <- quantile(agreement_4deg$index, c(0.1))
quant_thr_0_3 <- quantile(agreement_4deg$index, c(0.3))
quant_thr_0_7 <- quantile(agreement_4deg$index, c(0.7))
quant_thr_0_9 <- quantile(agreement_4deg$index, c(0.9))

agreement_4deg[index > quant_thr_0_9, dist_dataset_agreement := ordered(1, labels = "High")]
agreement_4deg[index > quant_thr_0_7 & index <= quant_thr_0_9, dist_dataset_agreement := ordered(2, labels = "Above average")]
agreement_4deg[index > quant_thr_0_3 & index <= quant_thr_0_7, dist_dataset_agreement := ordered(3, labels = "Average")]
agreement_4deg[index > quant_thr_0_1 & index <= quant_thr_0_3, dist_dataset_agreement := ordered(4, labels = "Below average")]
agreement_4deg[index <= quant_thr_0_1, dist_dataset_agreement := ordered(5, labels = "Low")] 

saveRDS(agreement_4deg, 
        paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_4deg.rds"))

agreement_2deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_index_gridwise_2deg.rds"))

quant_thr_0_1 <- quantile(agreement_2deg$index, c(0.1))
quant_thr_0_3 <- quantile(agreement_2deg$index, c(0.3))
quant_thr_0_7 <- quantile(agreement_2deg$index, c(0.7))
quant_thr_0_9 <- quantile(agreement_2deg$index, c(0.9))

agreement_2deg[index > quant_thr_0_9, dist_dataset_agreement := ordered(1, labels = "High")]
agreement_2deg[index > quant_thr_0_7 & index <= quant_thr_0_9, dist_dataset_agreement := ordered(2, labels = "Above average")]
agreement_2deg[index > quant_thr_0_3 & index <= quant_thr_0_7, dist_dataset_agreement := ordered(3, labels = "Average")]
agreement_2deg[index > quant_thr_0_1 & index <= quant_thr_0_3, dist_dataset_agreement := ordered(4, labels = "Below average")]
agreement_2deg[index <= quant_thr_0_1, dist_dataset_agreement := ordered(5, labels = "Low")] 

saveRDS(agreement_2deg, 
        paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_2deg.rds"))

agreement_1deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_index_gridwise_1deg.rds"))

quant_thr_0_1 <- quantile(agreement_1deg$index, c(0.1))
quant_thr_0_3 <- quantile(agreement_1deg$index, c(0.3))
quant_thr_0_7 <- quantile(agreement_1deg$index, c(0.7))
quant_thr_0_9 <- quantile(agreement_1deg$index, c(0.9))

agreement_1deg[index > quant_thr_0_9, dist_dataset_agreement := ordered(1, labels = "High")]
agreement_1deg[index > quant_thr_0_7 & index <= quant_thr_0_9, dist_dataset_agreement := ordered(2, labels = "Above average")]
agreement_1deg[index > quant_thr_0_3 & index <= quant_thr_0_7, dist_dataset_agreement := ordered(3, labels = "Average")]
agreement_1deg[index > quant_thr_0_1 & index <= quant_thr_0_3, dist_dataset_agreement := ordered(4, labels = "Below average")]
agreement_1deg[index <= quant_thr_0_1, dist_dataset_agreement := ordered(5, labels = "Low")] 

saveRDS(agreement_1deg, 
        paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_1deg.rds"))
