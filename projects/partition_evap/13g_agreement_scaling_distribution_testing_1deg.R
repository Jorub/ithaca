# Does grid-scale agreement transfer across scales? ----
# distribution testing 1 deg ----

source('source/partition_evap.R')

evap1_deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_1deg.rds"))

evap_datasets <- evap1_deg[N_1deg > min_1_deg_N]

dataset_list <- evap_datasets[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- evap_datasets[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- evap_datasets[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("deg_1_lon", "deg_1_lat", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_1deg.x, evap_1deg.y)$p.value), .(deg_1_lon, deg_1_lat)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("deg_1_lon","deg_1_lat", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise_1deg.rds"))

counts <- data_out[, .(counts = .N) , .(deg_1_lon, deg_1_lat)]

agreement <- data_out[KS_test_p > 0.05, .(matches = .N) , .(deg_1_lon, deg_1_lat)]
agreement <- counts[agreement, on = .(deg_1_lon, deg_1_lat)]
agreement <- agreement[counts > 77]
agreement[, index := matches/counts]

saveRDS(agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise_1deg.rds"))

