# Rank datasets according to opposition contribution ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
## Created in trend_evap/01_d
evap_trend_all <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Created in trend_evap/01_e
evap_trend_leftout <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))

## Estimate opposing fraction of all data ----
evap_sel <- subset(evap_trend_all, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
grid_cell_area <- unique(evap_sel[, .(lon, lat)]) %>% grid_area() # m2
evap_sel  <- grid_cell_area[evap_sel, on = .(lon, lat)]
setnames(evap_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area <- evap_sel[, sum(area)]

evap_sel_melt <- melt(evap_sel, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))
saveRDS(evap_sel_melt, paste0(PATH_SAVE_EVAP_TREND, "global_grid_datasets_opposing_p_thresholds_bootstrap.rds"))

## Merge opposing fraction with datasets leftout
evap_leftout_sel <- subset(evap_trend_leftout, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon", "dataset_leftout"))
evap_leftout_sel  <- grid_cell_area[evap_leftout_sel, on = .(lon, lat)]

setnames(evap_leftout_sel, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

evap_leftout_sel_melt <- melt(evap_leftout_sel, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

saveRDS(evap_leftout_sel_melt, paste0(PATH_SAVE_EVAP_TREND, "global_grid_datasets_leftout_opposing_p_thresholds_bootstrap.rds"))

evap_opposing_merge <- merge(evap_sel_melt, evap_leftout_sel_melt, by = c("lat", "lon", "area", "variable"), all.y = T)
setnames(evap_opposing_merge, old = c("value.x", "value.y"), new = c("trend_all", "trend_dataset_leftout"))

evap_opposing_merge[, contributing_opposition := 0]
evap_opposing_merge[trend_all == "opposing" & trend_dataset_leftout != "opposing", contributing_opposition := 1, .(dataset_leftout, variable)]
evap_opposing_merge <- evap_opposing_merge[!is.na(dataset_leftout),]

saveRDS(evap_opposing_merge, paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_contributing_opposition.rds"))

total_area <- evap_opposing_merge[, sum(area), .(variable, dataset_leftout)]
total_area <- total_area$V1[1]

evap_opposing <- evap_opposing_merge[contributing_opposition == 1, 
                                                   .(sum_opposition = sum(area)/total_area), 
                                                   .(variable, dataset_leftout)]

evap_opposing[, rank_opp := rank(-sum_opposition), .(variable)]


## Save data ----
saveRDS(evap_opposing, paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
