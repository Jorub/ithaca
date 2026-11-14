# Does grid-scale agreement transfer across scales? ----
# distribution testing 4 deg ----

source('source/partition_evap.R')

evap4_deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean_aggregated_4deg.rds"))

evap_datasets <- evap4_deg[N_4deg > min_4_deg_N]

dataset_list <- evap_datasets[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- evap_datasets[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- evap_datasets[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("deg_4_lon", "deg_4_lat", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_4deg.x, evap_4deg.y)$p.value), .(deg_4_lon, deg_4_lat)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("deg_4_lon","deg_4_lat", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise_4deg.rds"))

# Summarise all valid locations, including those with zero KS matches.
agreement <- data_out[, .(
  counts = sum(is.finite(KS_test_p)),
  matches = sum(is.finite(KS_test_p) & KS_test_p > 0.05, na.rm = TRUE)
), .(deg_4_lon, deg_4_lat)]
agreement <- agreement[counts > 77]
agreement[, index := matches/counts]

saveRDS(agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise_4deg.rds"))

