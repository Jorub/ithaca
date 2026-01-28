# Functions to support main functions
source('source/evap_trend.R')

## function to merge ----
merge_data <- function(evap_signal, evap_opposers,
                       evap_DCI_opposers, evap_significance_opposers,
                       mask_name){
  
  
  
  
  evap_signal[, dataset := toupper(dataset)]
  evap_signal[dataset == "ETMONITOR", dataset := "ETMonitor"]
  evap_signal[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
  evap_signal[dataset == "ERA5-LAND", dataset := "ERA5-land"]
  evap_signal[dataset == "MERRA2", dataset := "MERRA-2"]
  evap_signal[dataset == "JRA55", dataset := "JRA-55"]
  evap_signal[dataset == "TERRACLIMATE", dataset := "TerraClimate"]
  
  evap_opposers[, dataset_leftout := toupper(dataset_leftout)]
  evap_opposers[dataset_leftout == "ETMONITOR", dataset_leftout := "ETMonitor"]
  evap_opposers[dataset_leftout == "SYNTHESIZEDET", dataset_leftout := "SynthesizedET"]
  evap_opposers[dataset_leftout == "ERA5-LAND", dataset_leftout := "ERA5-land"]
  evap_opposers[dataset_leftout == "MERRA2", dataset_leftout := "MERRA-2"]
  evap_opposers[dataset_leftout == "JRA55", dataset_leftout := "JRA-55"]
  evap_opposers[dataset_leftout == "TERRACLIMATE", dataset_leftout := "TerraClimate"]
  
  evap_DCI_opposers[, dataset := toupper(dataset)]
  evap_DCI_opposers[dataset == "ETMONITOR", dataset := "ETMonitor"]
  evap_DCI_opposers[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
  evap_DCI_opposers[dataset == "ERA5-LAND", dataset := "ERA5-land"]
  evap_DCI_opposers[dataset == "MERRA2", dataset := "MERRA-2"]
  evap_DCI_opposers[dataset == "JRA55", dataset := "JRA-55"]
  evap_DCI_opposers[dataset == "TERRACLIMATE", dataset := "TerraClimate"]
  
  evap_significance_opposers[, dataset := toupper(dataset)]
  evap_significance_opposers[dataset == "ETMONITOR", dataset := "ETMonitor"]
  evap_significance_opposers[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
  evap_significance_opposers[dataset == "ERA5-LAND", dataset := "ERA5-land"]
  evap_significance_opposers[dataset == "MERRA2", dataset := "MERRA-2"]
  evap_significance_opposers[dataset == "JRA55", dataset := "JRA-55"]
  evap_significance_opposers[dataset == "TERRACLIMATE", dataset := "TerraClimate"]
  
  #### Ranked data products
  
  no_trenders <- evap_signal[variable %in%
                               c("sum_N_none_0_2", "sum_N_none_0_1", "sum_N_none_0_05", "sum_N_none_0_01")]
  
  no_trenders[variable == "sum_N_none_0_01", variable := "p <= 0.01", ]
  no_trenders[variable == "sum_N_none_0_05", variable := "p <= 0.05", ]
  no_trenders[variable == "sum_N_none_0_1", variable := "p <= 0.1", ]
  no_trenders[variable == "sum_N_none_0_2", variable := "p <= 0.2", ]
  
  pos_signal <- evap_signal[variable %in%
                              c("sum_N_pos_all", "sum_N_pos_0_2", "sum_N_pos_0_1", "sum_N_pos_0_05", "sum_N_pos_0_01")]
  
  pos_signal[variable == "sum_N_pos_0_01", variable := "p <= 0.01", ]
  pos_signal[variable == "sum_N_pos_0_05", variable := "p <= 0.05", ]
  pos_signal[variable == "sum_N_pos_0_1", variable := "p <= 0.1", ]
  pos_signal[variable == "sum_N_pos_0_2", variable := "p <= 0.2", ]
  pos_signal[variable == "sum_N_pos_all", variable := "p <= 1", ]
  
  
  neg_signal <- evap_signal[variable %in%
                              c("sum_N_neg_all", "sum_N_neg_0_2", "sum_N_neg_0_1", "sum_N_neg_0_05", "sum_N_neg_0_01")]
  
  neg_signal[variable == "sum_N_neg_0_01", variable := "p <= 0.01", ]
  neg_signal[variable == "sum_N_neg_0_05", variable := "p <= 0.05", ]
  neg_signal[variable == "sum_N_neg_0_1", variable := "p <= 0.1", ]
  neg_signal[variable == "sum_N_neg_0_2", variable := "p <= 0.2", ]
  neg_signal[variable == "sum_N_neg_all", variable := "p <= 1", ]
  
  evap_DCI_opposers[, rank_DCI_opposer := rank_datasets]
  evap_opposers[, rank_opposer := rank_opp]
  evap_opposers[, dataset := dataset_leftout]
  evap_significance_opposers[, rank_significance_opposer := rank_datasets]
  no_trenders[, rank_no_trenders := rank_datasets]
  pos_signal[, rank_pos_signal := rank_datasets]
  neg_signal[, rank_neg_signal := rank_datasets]
  
  data_merge <- subset(evap_DCI_opposers, select = c(mask_name, "dataset", "variable", "rank_DCI_opposer"))
  data_merge <- merge(data_merge, 
                      subset(evap_opposers, select = c(mask_name, "dataset", "variable", "rank_opposer")), 
                      by = c(mask_name,
                             "dataset", "variable"),
                      all = T
  )
  data_merge <- merge(data_merge, 
                      subset(evap_significance_opposers, select = c(mask_name, "dataset", "variable", "rank_significance_opposer")),                      
                      by = c(mask_name,
                             "dataset", "variable"),
                             all = T
  )
  data_merge <- merge(data_merge,
                      subset(no_trenders, select = c(mask_name, "dataset", "variable", "rank_no_trenders")),                      
                      by = c(mask_name,
                             "dataset", "variable"),
                      all = T
  )
  
  data_merge <- merge(data_merge,
                      subset(pos_signal, select = c(mask_name, "dataset", "variable", "rank_pos_signal")),                      
                      by = c(mask_name,
                             "dataset", "variable"),
                      all = T
  )
  
  data_merge <- merge(data_merge,
                      subset(neg_signal, select = c(mask_name, "dataset", "variable", "rank_neg_signal")),                      
                      by = c(mask_name,
                             "dataset", "variable"),
                      all = T
  )
  
  data_merge[, p_value := variable]
  data_merge[, variable := NULL]
  
  data_merge <- data_merge[,c(9,1:8)]
  return(data_merge)
  
}

## land use ----
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_dataset_rank_opposing_significance.rds"))

land_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                 evap_significance_opposers, 
                                 mask_name = 'land_cover_short_class')

### data check ----
land_data_topology[,any(is.na(rank_DCI_opposer))]
land_data_topology[,any(is.na(rank_significance_opposer))]
land_data_topology[,any(is.na(rank_opposer))]
land_data_topology[,any(is.na(rank_pos_signal))]
land_data_topology[,any(is.na(rank_neg_signal))]
land_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]

saveRDS(land_data_topology, paste0(PATH_SAVE_EVAP_TREND, "land_use_dataset_trend_topology.rds"))

## biome ----

evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_dataset_rank_opposing_significance.rds"))

biome_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                 evap_significance_opposers, 
                                 mask_name = 'biome_short_class')

### data check ----
biome_data_topology[,any(is.na(rank_DCI_opposer))]
biome_data_topology[,any(is.na(rank_significance_opposer))]
biome_data_topology[,any(is.na(rank_opposer))]
biome_data_topology[,any(is.na(rank_pos_signal))]
biome_data_topology[,any(is.na(rank_neg_signal))]
biome_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]

saveRDS(biome_data_topology, paste0(PATH_SAVE_EVAP_TREND, "biome_dataset_trend_topology.rds"))

## IPCC reference region ----
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_dataset_rank_opposing_significance.rds"))

ipcc_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                  evap_significance_opposers, 
                                  mask_name = 'IPCC_ref_region')

### data check ----
ipcc_data_topology <- ipcc_data_topology[!is.na(IPCC_ref_region)]

ipcc_data_topology[,any(is.na(rank_DCI_opposer))]
ipcc_data_topology[,any(is.na(rank_significance_opposer))]
ipcc_data_topology[,any(is.na(rank_opposer))]
ipcc_data_topology[,any(is.na(rank_pos_signal))]
ipcc_data_topology[,any(is.na(rank_neg_signal))]
ipcc_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]


saveRDS(ipcc_data_topology, paste0(PATH_SAVE_EVAP_TREND, "ipcc_ref_regions_dataset_trend_topology.rds"))

## evaporation quantiles ----

evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_quant_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_dataset_rank_opposing_significance.rds"))

evap_quant_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                 evap_significance_opposers, 
                                 mask_name = 'evap_quant')

### data check ----
evap_quant_data_topology[,any(is.na(rank_DCI_opposer))]
evap_quant_data_topology[,any(is.na(rank_significance_opposer))]
evap_quant_data_topology[,any(is.na(rank_opposer))]
evap_quant_data_topology[,any(is.na(rank_pos_signal))]
evap_quant_data_topology[,any(is.na(rank_neg_signal))]
evap_quant_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]

saveRDS(evap_quant_data_topology, paste0(PATH_SAVE_EVAP_TREND, "evap_quantiles_dataset_trend_topology.rds"))


## elevation classes ----

evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_class_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_class_dataset_rank_opposing_significance.rds"))

elev_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                       evap_significance_opposers, 
                                       mask_name = 'elev_class')

### data check ----
elev_data_topology[,any(is.na(rank_DCI_opposer))]
elev_data_topology[,any(is.na(rank_significance_opposer))]
elev_data_topology[,any(is.na(rank_opposer))]
elev_data_topology[,any(is.na(rank_pos_signal))]
elev_data_topology[,any(is.na(rank_neg_signal))]
elev_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]

saveRDS(elev_data_topology, paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_dataset_trend_topology.rds"))

## Koeppen-Geiger ----

evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_beck_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_beck_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_beck_dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_beck_dataset_rank_opposing_significance.rds"))

KG_beck_data_topology <- merge_data(evap_signal, evap_opposers, evap_DCI_opposers,
                                 evap_significance_opposers, 
                                 mask_name = 'KG_beck')

KG_beck_data_topology <- KG_beck_data_topology[!is.na(KG_beck)]

### data check ----
KG_beck_data_topology[,any(is.na(rank_DCI_opposer))]
KG_beck_data_topology[,any(is.na(rank_significance_opposer))]
KG_beck_data_topology[,any(is.na(rank_opposer))]
KG_beck_data_topology[,any(is.na(rank_pos_signal))]
KG_beck_data_topology[,any(is.na(rank_neg_signal))]
KG_beck_data_topology[p_value != 'p <= 1', any(is.na(rank_no_trenders))]


saveRDS(KG_beck_data_topology, paste0(PATH_SAVE_EVAP_TREND, "KG_beck_dataset_trend_topology.rds"))
