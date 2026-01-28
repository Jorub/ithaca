# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend <- evap_trend[dataset_count >= 12]

## Analysis 
### Overview ---=
evap_trend[p <= 0.1 & p > 0.05, .N, dataset]
evap_trend[p < 0.05, .N, dataset]
evap_trend[, .N, dataset]
evap_trend[p > 0.1, .N, dataset]

### DCI ----
evap_trend_dci <- evap_trend[,.(DCI_theil_sen = sum(slope/abs(slope)*significant_theil_sen, na.rm = TRUE)/.N),
                             .(lon,lat)]

### probability groups ----
min_consensus <- 0.9

evap_trend_pos <- evap_trend[trend_direction == "positive significant", .(N_pos_theil_sen = .N), .(lon,lat)]
evap_trend_neg <- evap_trend[trend_direction == "negative significant", .(N_neg_theil_sen = .N), .(lon,lat)]
evap_trend_ins_pos <- evap_trend[trend_direction == "positive", .(N_ins_pos_theil_sen = .N), .(lon,lat)]
evap_trend_ins_neg <- evap_trend[trend_direction == "negative", .(N_ins_neg_theil_sen = .N), .(lon,lat)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_ins_neg, by = c("lon", "lat"), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary[, threshold_likely := 0.33*count,.(lon,lat)]
evap_trend_summary[, threshold_probable := 1,.(lon,lat)]

evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen >= threshold_probable & N_pos_theil_sen <= threshold_likely & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > threshold_likely  & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen >= threshold_probable & N_neg_theil_sen <= threshold_likely & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) > min_consensus, trend:= "negative probable",]

evap_trend_summary[N_pos_theil_sen > 0 & N_pos_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]
evap_trend_summary[N_neg_theil_sen > 0 & N_neg_theil_sen/(N_neg_theil_sen+N_pos_theil_sen) <= min_consensus, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary<- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

land_area <- evap_trend_summary[,sum(area)]
uncertain_area <- evap_trend_summary[ trend == "uncertain", sum(area)]
notrend_area <- evap_trend_summary[trend == "no trend", sum(area)]
pos_area <- evap_trend_summary[trend %in% c("positive likely", "positive probable"), sum(area)]
neg_area <- evap_trend_summary[trend %in% c("negative likely", "negative probable"), sum(area)]

uncertain_area/land_area
notrend_area/land_area
pos_area/land_area
neg_area/land_area

evap_trend_summary[N_pos_theil_sen > 8, .N]
evap_trend_summary[N_neg_theil_sen > 8, .N]

### Merge data ----
evap_trend_lon_lat <- merge(evap_trend_summary, evap_trend_dci)

## Save data ----
saveRDS(evap_trend_lon_lat, paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices_opp_allowed_bootstrap.rds"))

