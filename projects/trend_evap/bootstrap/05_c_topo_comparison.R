# Compare 20 year period with sensitivty periods ----
source('source/evap_trend.R')

global_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))

global_topo_period_a <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_period_a.rds"))
global_topo_period_b <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_period_b.rds"))
write.table(global_topo_period_a, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_dataset_trend_topology_2005_2019.csv"))
write.table(global_topo_period_b, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_dataset_trend_topology_2000_2014.csv"))

diff_period_a <- global_topo_period_a
diff_period_a[,2:7] <- global_topo_period_a[,2:7]-global_topo[,2:7]

diff_period_b <- global_topo_period_b
diff_period_b[,2:7] <- global_topo_period_b[,2:7]-global_topo[,2:7]

saveRDS(diff_period_a, paste0(PATH_SAVE_EVAP_TREND, "rank_diff_global_topology_period_a.rds"))
saveRDS(diff_period_b, paste0(PATH_SAVE_EVAP_TREND, "rank_diff_global_topology_period_b.rds"))
