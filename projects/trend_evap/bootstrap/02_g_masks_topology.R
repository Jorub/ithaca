# Aggregate over masks and rank
source('source/evap_trend.R')
source('source/geo_functions.R')

topo_grid <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_cell_topology.rds"))

PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
grid_cell_area <- unique(topo_grid [, .(lon, lat)]) %>% grid_area() # m2
topo_grid   <- grid_cell_area[topo_grid , on = .(lon, lat)]

## IPCC ----

data_merge <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    topo_grid, 
                    by = c("lon", "lat"))
data_merge <- data_merge[!is.na(IPCC_ref_region)]
data_melt <- melt(data_merge, id.vars = c("lon", "lat", "area", "dataset", "IPCC_ref_region"))
data_topo <- data_melt[, .(sum_area = sum(area)), 
                          .(dataset, IPCC_ref_region, variable, value)]
data_topo[value == 1,  value_chr := "TRUE_chr"]
data_topo[value == 0,  value_chr := "FALSE_chr"]

data_topo_cast <- dcast(data_topo, dataset+IPCC_ref_region+variable~value_chr, value.var = "sum_area")
data_topo_cast[is.na(TRUE_chr),TRUE_chr:= 0]
data_topo_cast[, sum_area := TRUE_chr]
data_topo_cast[, TRUE_chr := NULL]
data_topo_cast[, FALSE_chr := NULL]
data_topo_cast[, rank := rank(-sum_area, ties = "first"), .(variable, IPCC_ref_region)]

topo_grid_cast <- dcast(data_topo_cast, dataset+IPCC_ref_region ~variable, value.var = "rank")
saveRDS(topo_grid_cast, paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_regions_topology_rank_roles.rds"))
