source('source/partition_evap.R')

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))
evap_datasets[, anomaly := evap - mean(evap), .(lat, lon, dataset)]
dataset_list <- evap_datasets[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- evap_datasets[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- evap_datasets[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("lon", "lat", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(anomaly.x, anomaly.y)$p.value), .(lon, lat)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("lon","lat", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "anomaly_ks_test_gridwise.rds"))

counts <- data_out[, .(counts = .N) , .(lon, lat)]

agreement <- data_out[KS_test_p > 0.05, .(matches = .N) , .(lon, lat)]
agreement <- counts[agreement, on = .(lon, lat)]
agreement <- agreement[counts > 77]
agreement[, index := matches/counts]

saveRDS(agreement, paste0(PATH_SAVE_PARTITION_EVAP, "anomaly_distribution_agreement_index_gridwise.rds"))
