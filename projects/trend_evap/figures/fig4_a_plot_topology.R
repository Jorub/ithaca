# Figure 4 - Topology -----
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
topology_sel_melt[, Rank := cut(value, breaks = c(0,2,4,6,8,10,12,14),
                                labels = c("[1,2]", "[3,4]", "[5,6]",
                                           "[7,8]", "[9,10]", "[11,12]",
                                           "[13,14]"))]


topology_sel_melt[, plot_name := factor(plot_name, levels = c("Trend\nopposer",
                                                              "Significance \nopposer",
                                                              "Opposition\ncontributor",
                                                              "Positive \nbooster",
                                                              "Negative \nbooster",
                                                              "Signal \ndampener"
                                                              ))]


dataset_order <- topology_sel_melt[plot_name == "Trend\nopposer", dataset[order(-value)]]
topology_sel_melt[, dataset := factor(dataset, levels = dataset_order)]

fill_topology_rank <- c("[1,2]" = "#440154FF", "[3,4]" = "#414487FF",
                        "[5,6]" = "#2A7B8EFF",  "[7,8]" = "#22A384FF",
                        "[9,10]" = "#7AD151FF", "[11,12]" = "#C7E020FF",
                        "[13,14]" = "#FDE725FF")


ggplot(topology_sel_melt)+
  geom_point(aes(y = dataset, x = plot_name, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+
  labs(x = "Topology", title = "Global topology of trend signatures", y = "")+
  theme_bw()

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig4_global_topology.png"), 
       width = 8, height = 4)
