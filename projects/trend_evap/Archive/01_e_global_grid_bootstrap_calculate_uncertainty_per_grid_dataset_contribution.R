# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Dataset contribution: Change in area fraction for each probability group per grid per dataset----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_c
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend <- evap_trend[dataset_count >= 12]

### probability groups with dataset left out----
datasets <- unique(evap_trend$dataset)

evap_trend_pos <- evap_trend[dataset != datasets[1] & trend_direction == "positive significant", 
                             .(N_pos_theil_sen = .N,
                               dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_neg  <- evap_trend[dataset != datasets[1] & trend_direction == "negative significant", 
                              .(N_neg_theil_sen = .N,
                                dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_ins_pos <- evap_trend[dataset != datasets[1] & trend_direction == "positive", 
                                 .(N_ins_pos_theil_sen = .N,
                                   dataset_leftout = datasets[1]), .(lon,lat)]

evap_trend_ins_neg <- evap_trend[dataset != datasets[1] & trend_direction == "negative", 
                                 .(N_ins_neg_theil_sen = .N,
                                   dataset_leftout = datasets[1]), .(lon,lat)]
#### Repeat for other datasets
for(dataset_leftout in datasets[2:length(datasets)]){
  dummy_pos <- evap_trend[dataset != dataset_leftout & trend_direction == "positive significant", 
                          .(N_pos_theil_sen = .N,
                            dataset_leftout = dataset_leftout), .(lon, lat)]
  evap_trend_pos <- merge(dummy_pos, evap_trend_pos, by = c('lon', 'lat', 'N_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  dummy_neg <- evap_trend[dataset != dataset_leftout & trend_direction == "negative significant", 
                          .(N_neg_theil_sen = .N,
                            dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_neg <- merge(dummy_neg, evap_trend_neg, by = c('lon', 'lat', 'N_neg_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_pos <- evap_trend[dataset != dataset_leftout & trend_direction == "positive", 
                              .(N_ins_pos_theil_sen = .N,
                                dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_ins_pos <- merge(dummy_ins_pos, evap_trend_ins_pos, by = c('lon', 'lat', 'N_ins_pos_theil_sen', 'dataset_leftout'), all = TRUE)
  
  dummy_ins_neg <- evap_trend[dataset != dataset_leftout & trend_direction == "negative", 
                              .(N_ins_neg_theil_sen = .N,
                                dataset_leftout = dataset_leftout), .(lon,lat)]
  evap_trend_ins_neg <- merge(dummy_ins_neg, evap_trend_ins_neg, by = c('lon', 'lat', 'N_ins_neg_theil_sen', 'dataset_leftout'), all = TRUE)
}

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins_pos, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_ins_neg, by = c("lon", "lat", 'dataset_leftout'), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat, dataset_leftout)]
evap_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
evap_trend_summary[, threshold_probable := 1,.(lon,lat)]

min_consensus <- 0.9

evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > threshold_likely  & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative probable",]

evap_trend_summary[N_pos_theil_sen > 0 & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]
evap_trend_summary[N_neg_theil_sen > 0 & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary <- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_uncertainty_dataset_leftout_bootstrap.rds"))

evap_trend_summary[, count_all:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary <- evap_trend_summary[ count_all >= 182]
evap_uncertainty <- evap_trend_summary[,.(trend_area = sum(area)),.(trend,
                                                                    dataset_leftout)]
evap_uncertainty[, area := sum(trend_area), .(dataset_leftout)]
evap_uncertainty[, area_fraction := trend_area/area, .(dataset_leftout)]

### Input Data generated in projects/trend_evap/bootstrap/01_d
evap_trend_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices_opp_allowed_bootstrap.rds"))
evap_trend_indices <- evap_trend_indices[count >= 14]
evap_uncertainty_all <- evap_trend_indices[,.(trend_area = sum(area)),.(trend)]
evap_uncertainty_all[, area:= sum(trend_area)]
evap_uncertainty_all[, area_fraction:= trend_area/area]

evap_probability <- merge(evap_uncertainty, evap_uncertainty_all, by = c("trend"), all = T)
evap_probability[, diff_area_fraction := area_fraction.y-area_fraction.x]

saveRDS(evap_probability, paste0(PATH_SAVE_EVAP_TREND, "global_stats_probability_group_contribution_dataset_leftout_bootstrap.rds"))

