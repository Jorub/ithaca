# Figure 4 - Topology -----
## P-value stability
source('source/evap_trend.R')

## Data ----
topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))
topology[, p_value := "p <= 0.05"]
topology_0_01 <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles_0_01.rds"))
topology_0_01[, p_value := "p <= 0.01"]
topology_0_1 <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles_0_1.rds"))
topology_0_1[, p_value := "p <= 0.1"]
topology_0_2 <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles_0_2.rds"))
topology_0_2[, p_value := "p <= 0.2"]

topology_rbind <- rbind(topology, topology_0_01, topology_0_1, topology_0_2)

saveRDS(topology_rbind, paste0(PATH_SAVE_EVAP_TREND, "global_dataset_trend_topology.rds"))
write.table(topology_rbind, paste0(PATH_SAVE_EVAP_TREND_TABLES, "global_dataset_trend_topology.csv"))

topology_melt <- melt(topology_rbind, id.vars = c("dataset", "p_value"))

topology_melt[variable == "pos_signal", plot_name := "Positive \nbooster"]
topology_melt[variable == "neg_signal", plot_name := "Negative \nbooster"]
topology_melt[variable == "signal_dampener", plot_name := "Signal \ndampener"]
topology_melt[variable == "opposing_majority_trend", plot_name := "Trend\nopposer"]
topology_melt[variable == "opposition_contributor", plot_name := "Opposition\ncontributor"]
topology_melt[variable == "opposing_significance", plot_name := "Significance \nopposer"]



topology_melt[, plot_name := factor(plot_name, levels = c("Trend\nopposer",
                                                          "Significance \nopposer",
                                                          "Opposition\ncontributor",
                                                          "Positive \nbooster",
                                                          "Negative \nbooster",
                                                          "Signal \ndampener"))]
              
topology_melt[p_value == "p <= 0.05", rank_0_05 := value]
topology_melt[, rank_0_05 := min(rank_0_05, na.rm = T), .(dataset, variable)]
topology_melt[, rank_difference := value - rank_0_05]

topology_melt[, rank_difference_fac := cut(rank_difference, breaks = c(-8,-6,-4,-1.1,1.1,4,6,8),
                                           labels = c("[-7,-6]", "[-5,-4]","[-3,-2]",
                                                      "[-1,1]", "[2,3]", "[4,5]", "[6,7]"))]

fill_topology_rank <- c("[-7,-6]" = "#440154FF", "[-5,-4]"= "#414487FF","[-3,-2]" = "#7378BE",
                        "[-1,1]" ="gray90", "[2,3]" = "#E6F000", 
                        "[4,5]" = "#C7E020FF", "[6,7]"= "#7AD151FF")

topology_melt[p_value == "p <= 0.01", p_value := "\u2264 0.01"]
topology_melt[p_value == "p <= 0.1", p_value := "\u2264 0.1"]
topology_melt[p_value == "p <= 0.2", p_value := "\u2264 0.2"]

ggplot(topology_melt[p_value %in% c("\u2264 0.01", "\u2264 0.1", "\u2264 0.2")])+
  geom_raster(aes(x = plot_name, fill = as.factor(rank_difference_fac), y = p_value))+
  labs(x = "Topology", 
       title = "Global topology stability across p-value thresholds", 
       y = "P-value threshold", fill = "Alternative p-value thresholds \nstronger \u2190 0 \u2192 weaker")+
  scale_fill_manual(values = fill_topology_rank)+
  facet_grid(rows = vars(dataset), scales = "free", space = "free")+
  geom_hline(yintercept = 0, color = "black", lwd = 1)+
  theme_bw()+
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig_global_topology_p_value_stability.png"), 
       width = 8, height = 8)
