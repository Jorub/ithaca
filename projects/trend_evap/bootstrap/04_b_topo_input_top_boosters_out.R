# Topology input top boosters out ----
source('source/evap_trend.R')

## Data ----
### Input data generated in trend_evap/bootstrap/01_c
evap_trend_all <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap.rds"))  

evap_trend <- evap_trend_all[!dataset %in% c("gldas-vic", "gldas-noah")]

## Analysis ----
### p value as 0.05 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon)]
evap_trend_sig <- evap_trend[p <= 0.05, .(N_sig_0_05 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary, evap_trend_none, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary, evap_trend_sig, by = c("lon", "lat"), all = TRUE)

### all slopes ----
evap_trend_pos <- evap_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[slope < 0, .(N_neg_all = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)

### Fill NA as 0 for count
evap_trend_summary[is.na(evap_trend_summary)] <- 0

### DCI ----
evap_trend_summary[, DCI_0_05 := (N_pos_0_05-N_neg_0_05)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_all := (N_pos_all-N_neg_all)/(N_pos_all+N_neg_all)]

### trend groups
evap_trend_summary[ N_pos_0_05 > 0, trend_0_05 := "positive"]
evap_trend_summary[ N_neg_0_05 > 0, trend_0_05 := "negative"]
evap_trend_summary[ N_pos_0_05 > 0 & N_neg_0_05 > 0, trend_0_05 := "opposing"]
evap_trend_summary[ N_pos_0_05 == 0 & N_neg_0_05 == 0, trend_0_05 := "none"]

evap_trend_summary[trend_0_05 == "opposing", p_val_opposing := "<=0.05"]

## Save data ----
saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_input_pos_boosters_out.rds"))

# negative boosters out ----
evap_trend <- evap_trend_all[!dataset %in% c("bess", "fldas")]

## Analysis ----
### p value as 0.05 as threshold ----
evap_trend_pos <- evap_trend[p <= 0.05 & slope >= 0, .(N_pos_0_05 = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[p <= 0.05 & slope < 0, .(N_neg_0_05 = .N), .(lat, lon)]
evap_trend_none <- evap_trend[p > 0.05, .(N_none_0_05 = .N), .(lat, lon)]
evap_trend_sig <- evap_trend[p <= 0.05, .(N_sig_0_05 = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary, evap_trend_none, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary, evap_trend_sig, by = c("lon", "lat"), all = TRUE)

### all slopes ----
evap_trend_pos <- evap_trend[slope >= 0, .(N_pos_all = .N), .(lat, lon)]
evap_trend_neg <- evap_trend[slope < 0, .(N_neg_all = .N), .(lat, lon)]

evap_trend_summary <- merge(evap_trend_summary , evap_trend_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)

### Fill NA as 0 for count
evap_trend_summary[is.na(evap_trend_summary)] <- 0

### DCI ----
evap_trend_summary[, DCI_0_05 := (N_pos_0_05-N_neg_0_05)/(N_pos_all+N_neg_all)]
evap_trend_summary[, DCI_all := (N_pos_all-N_neg_all)/(N_pos_all+N_neg_all)]

### trend groups
evap_trend_summary[ N_pos_0_05 > 0, trend_0_05 := "positive"]
evap_trend_summary[ N_neg_0_05 > 0, trend_0_05 := "negative"]
evap_trend_summary[ N_pos_0_05 > 0 & N_neg_0_05 > 0, trend_0_05 := "opposing"]
evap_trend_summary[ N_pos_0_05 == 0 & N_neg_0_05 == 0, trend_0_05 := "none"]

evap_trend_summary[trend_0_05 == "opposing", p_val_opposing := "<=0.05"]

## Save data ----
saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_input_neg_boosters_out.rds"))

