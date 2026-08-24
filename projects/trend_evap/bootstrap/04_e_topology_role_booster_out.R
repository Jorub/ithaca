# Role strength ----
source('source/evap_trend.R')
source('source/geo_functions.R')

topo_grid <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_pos_out.rds"))

grid_cell_area <- unique(topo_grid [, .(lon, lat)]) %>% grid_area() # m2
topo_grid   <- grid_cell_area[topo_grid , on = .(lon, lat)]

topo_grid_melt <- melt(topo_grid, id.vars = c("lon", "lat", "area", "dataset"))

topo_grid_area <- topo_grid_melt[value == 1, .(sum_area = sum(area)), .(dataset, variable)]

topo_grid_area[, rank := rank(-sum_area), .(variable)] 

topo_grid_cast <- dcast(topo_grid_area, dataset ~variable, value.var = "rank")

saveRDS(topo_grid_cast, paste0(PATH_SAVE_EVAP_TREND, "global_topology_pos_out.rds"))


topo_grid <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_neg_out.rds"))

grid_cell_area <- unique(topo_grid [, .(lon, lat)]) %>% grid_area() # m2
topo_grid   <- grid_cell_area[topo_grid , on = .(lon, lat)]

topo_grid_melt <- melt(topo_grid, id.vars = c("lon", "lat", "area", "dataset"))

topo_grid_area <- topo_grid_melt[value == 1, .(sum_area = sum(area)), .(dataset, variable)]

topo_grid_area[, rank := rank(-sum_area), .(variable)] 

topo_grid_cast <- dcast(topo_grid_area, dataset ~variable, value.var = "rank")

saveRDS(topo_grid_cast, paste0(PATH_SAVE_EVAP_TREND, "global_topology_neg_out.rds"))

### reference ranks

topo_grid <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_cell_topology.rds"))

grid_cell_area <- unique(topo_grid [, .(lon, lat)]) %>% grid_area() # m2
topo_grid   <- grid_cell_area[topo_grid , on = .(lon, lat)]

topo_grid_melt <- melt(topo_grid, id.vars = c("lon", "lat", "area", "dataset"))

topo_grid_area <- topo_grid_melt[value == 1, .(sum_area = sum(area)), .(dataset, variable)]

topo_grid_area_pos_out <- copy(topo_grid_area)
topo_grid_area_pos_out <- topo_grid_area_pos_out[!dataset %in% c("GLDAS-VIC", "GLDAS-NOAH")]
topo_grid_area_pos_out[, rank := rank(-sum_area), .(variable)] 
topo_grid_area_pos_out_cast <- dcast(topo_grid_area_pos_out, dataset ~variable, value.var = "rank")

saveRDS(topo_grid_area_pos_out, paste0(PATH_SAVE_EVAP_TREND, "global_topology_reference_ranks_pos_out.rds"))

topo_grid_area_neg_out <- copy(topo_grid_area)
topo_grid_area_neg_out <- topo_grid_area_neg_out[!dataset %in% c("BESS", "FLDAS")]
topo_grid_area_neg_out[, rank := rank(-sum_area), .(variable)] 
topo_grid_area_neg_out_cast <- dcast(topo_grid_area_neg_out, dataset ~variable, value.var = "rank")

saveRDS(topo_grid_area_neg_out, paste0(PATH_SAVE_EVAP_TREND, "global_topology_reference_ranks_neg_out.rds"))

