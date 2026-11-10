# pairwise difference metrics ----
source('source/partition_evap.R')

evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

# Calculate grid-cell mean difference
dataset_list <- evap_mean_datasets[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- evap_mean_datasets[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- evap_mean_datasets[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("lon", "lat"))
    dummy_result <- dummy_data[, .(lon, lat, sym_rel_diff = abs(evap_mean.x-evap_mean.y)/mean(c(evap_mean.x,evap_mean.y)),
                                   pair_mean_et = mean(c(evap_mean.x,evap_mean.y)),
                                   et_delta = evap_mean.x-evap_mean.y,
                                   abs_et_delta = abs(evap_mean.x-evap_mean.y)
    ),]    
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      print("merging")
      data_out <- merge(data_out, dummy_result, by = c("lon","lat", "sym_rel_diff", "pair_mean_et",
                                                       "et_delta", "abs_et_delta",
                                                       "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"))
