# Figure 4 - Topology -----
## Time periods stability
source('source/evap_trend.R')

## Data ----
PATH_SAVE_EVAP_TREND <- "data/evap_trend/"

diff_period_a_topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "rank_diff_global_topology_period_a.rds"))
diff_period_b_topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "rank_diff_global_topology_period_b.rds"))


diff_period_a_topology[, time_period := "2000-2014"]
diff_period_b_topology[, time_period := "2005-2019"]

diff_period <- rbind(diff_period_b_topology, diff_period_a_topology)

topology_melt <- melt(diff_period, id.vars = c("dataset", "time_period"))

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
                                                          "Signal \ndampener"
))]

topology_melt[, rank_difference_fac := cut(value, breaks = c(-8,-6,-4,-1.1,1.1,4,6,8),
                                           labels = c("[-7,-6]", "[-5,-4]","[-3,-2]",
                                                      "[-1,1]", "[2,3]", "[4,5]", "[6,7]"))]

fill_topology_rank <- c("[-7,-6]" = "#440154FF", "[-5,-4]"= "#414487FF","[-3,-2]" = "#7378BE",
                        "[-1,1]" ="gray90", "[2,3]" = "#E6F000", 
                        "[4,5]" = "#C7E020FF", "[6,7]"= "#7AD151FF")

ggplot(topology_melt)+
  geom_raster(aes(x = plot_name, fill = as.factor(rank_difference_fac), y = time_period))+
  labs(x = "Topology", 
       title = "Global topology stability for different time periods", 
       y = "Dataset", fill = "Corresponding 15-year period\nstronger \u2190 0 \u2192 weaker")+
  scale_fill_manual(values = fill_topology_rank)+
  geom_hline(yintercept = 0, color = "black", lwd = 1)+
  facet_grid(rows = vars(dataset), scales = "free", space = "free")+
  theme_bw()+
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig_global_topology_time_period_stability.png"), 
       width = 8, height = 8)
