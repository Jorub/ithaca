source('source/evap_trend.R')
## Data ----
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_DCI_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_DCI.rds"))
evap_significance_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "dataset_rank_opposing_significance.rds"))

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

evap_DCI_opposers[, rank_trend_opposer := rank_datasets]
evap_opposers[, rank_opposition_contributor := rank_opp]
evap_opposers[, dataset := dataset_leftout]
evap_significance_opposers[, rank_significance_opposer := rank_datasets]
no_trenders[, rank_dampener := rank_datasets]
pos_signal[, rank_pos_signal := rank_datasets]
neg_signal[, rank_neg_signal := rank_datasets]

data_merge <- subset(evap_DCI_opposers, select = c("dataset", "variable", "rank_trend_opposer"))
data_merge <- merge(data_merge, 
                    subset(evap_opposers, select = c("dataset", "variable", "rank_opposition_contributor")), 
                    by = c("dataset", "variable"),
                    all = T
)
data_merge <- merge(data_merge, 
                    subset(evap_significance_opposers, 
                           select = c("dataset", "variable", "rank_significance_opposer")),                      
                    by = c("dataset", "variable"),
                    all = T
)
data_merge <- merge(data_merge,
                    subset(no_trenders, select = c("dataset", "variable", "rank_dampener")),                      
                    by = c("dataset", "variable"),
                    all = T
)

data_merge <- merge(data_merge,
                    subset(pos_signal, select = c("dataset", "variable", "rank_pos_signal")),                      
                    by = c("dataset", "variable"),
                    all = T
)

data_merge <- merge(data_merge,
                    subset(neg_signal, select = c("dataset", "variable", "rank_neg_signal")),                      
                    by = c("dataset", "variable"),
                    all = T
)

data_merge[, p_value := variable]
data_merge[, variable := NULL]

saveRDS(data_merge, paste0(PATH_SAVE_EVAP_TREND, "global_dataset_trend_topology.rds"))
write.table(data_merge, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_dataset_trend_topology.csv"))
