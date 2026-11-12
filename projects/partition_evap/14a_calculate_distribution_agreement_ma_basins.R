# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/graphics.R')
source('source/mask_paths.R')

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basin.rds"))
data[, year := as.numeric(as.character(year))]
data[,environment_volume := NULL]
data[,area_sum := NULL]

dataset_list <- data[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("ma_basin", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(ma_basin)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("ma_basin", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_ma_basin.rds"))

counts <- data_out[, .(counts = .N) , .(ma_basin)]

agreement <- data_out[KS_test_p > 0.05, .(matches = .N) , .(ma_basin)]
agreement <- counts[agreement, on = .(ma_basin)]
agreement <- agreement[counts > 77]
agreement[, index := matches/counts]

saveRDS(agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_ma_basin.rds"))

