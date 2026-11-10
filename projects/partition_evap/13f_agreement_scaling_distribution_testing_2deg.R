# Does grid-scale agreement transfer across scales? ----
# distribution testing 2 deg ----

source('source/partition_evap.R')

evap2_deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_2deg.rds"))

evap_datasets <- evap2_deg[N_2deg > min_2_deg_N]

dataset_list <- evap_datasets[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- evap_datasets[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- evap_datasets[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("deg_2_lon", "deg_2_lat", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_2deg.x, evap_2deg.y)$p.value), .(deg_2_lon, deg_2_lat)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("deg_2_lon","deg_2_lat", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise_2deg.rds"))

counts <- data_out[, .(counts = .N) , .(deg_2_lon, deg_2_lat)]

agreement <- data_out[KS_test_p > 0.05, .(matches = .N) , .(deg_2_lon, deg_2_lat)]
agreement <- counts[agreement, on = .(deg_2_lon, deg_2_lat)]
agreement <- agreement[counts > 77]
agreement[, index := matches/counts]

saveRDS(agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise_2deg.rds"))

