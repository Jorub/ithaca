# Figure 4 - Prep topology figure -----
## Opposers, positive and negative signal boosters, no trenders ----
source('source/evap_trend.R')

## Data ----
topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_dataset_trend_topology.rds"))

topology_sel <- topology[p_value == "p <= 0.05"]
topology_sel_melt <- melt(topology_sel, id.vars = c("dataset", "p_value"))
topology_sel_melt[variable == "rank_pos_signal", plot_name := "Positive \nbooster"]
topology_sel_melt[variable == "rank_neg_signal", plot_name := "Negative \nbooster"]
topology_sel_melt[variable == "rank_dampener", plot_name := "Signal \ndampener"]
topology_sel_melt[variable == "rank_trend_opposer", plot_name := "Trend\nopposer"]
topology_sel_melt[variable == "rank_opposition_contributor", plot_name := "Opposition\ncontributor"]
topology_sel_melt[variable == "rank_significance_opposer", plot_name := "Significance \nopposer"]

saveRDS(topology_sel_melt, paste0(PATH_SAVE_EVAP_TREND, "melt_global_dataset_trend_topology.rds"))
