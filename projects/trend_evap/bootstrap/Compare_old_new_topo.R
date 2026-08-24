source('source/evap_trend.R')

global_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))

global_topo_old <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_dataset_trend_topology.rds"))

global_topo_old <- global_topo_old[p_value == "p <= 0.05"]

global_topo_old[, p_value := NULL]


global_topo_test <- global_topo[, c(1,5,7,6,4,2,3)]

global_topo_test_diff <- global_topo_test 
global_topo_test_diff[, 2:7] <- global_topo_test_diff[, 2:7] - global_topo_old[, 2:7] 
