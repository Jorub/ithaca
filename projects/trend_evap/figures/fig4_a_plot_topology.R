# Figure 4 - Topology -----
## Opposers, positive and negative signal boosters, no trenders ----
source('source/evap_trend.R')

## Data ----
topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))

topology_melt <- melt(topology, id.vars = c("dataset"))
topology_melt[variable == "pos_signal", plot_name := "Positive \nbooster"]
topology_melt[variable == "neg_signal", plot_name := "Negative \nbooster"]
topology_melt[variable == "signal_dampener", plot_name := "Signal \ndampener"]
topology_melt[variable == "opposing_majority_trend", plot_name := "Trend\nopposer"]
topology_melt[variable == "opposition_contributor", plot_name := "Opposition\ncontributor"]
topology_melt[variable == "opposing_significance", plot_name := "Significance \nopposer"]
topology_melt[, Rank := cut(value, breaks = c(0,2,4,6,8,10,12,14),
                                labels = c("[1,2]", "[3,4]", "[5,6]",
                                           "[7,8]", "[9,10]", "[11,12]",
                                           "[13,14]"))]


topology_melt[, plot_name := factor(plot_name, levels = c("Trend\nopposer",
                                                              "Significance \nopposer",
                                                              "Opposition\ncontributor",
                                                              "Positive \nbooster",
                                                              "Negative \nbooster",
                                                              "Signal \ndampener"
                                                              ))]


dataset_order <- topology_melt[plot_name == "Trend\nopposer", dataset[order(-value)]]
topology_melt[, dataset := factor(dataset, levels = dataset_order)]

fill_topology_rank <- c("[1,2]" = "#440154FF", "[3,4]" = "#414487FF",
                        "[5,6]" = "#2A7B8EFF",  "[7,8]" = "#22A384FF",
                        "[9,10]" = "#7AD151FF", "[11,12]" = "#C7E020FF",
                        "[13,14]" = "#FDE725FF")


ggplot(topology_melt)+
  geom_point(aes(y = dataset, x = plot_name, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+
  labs(x = "Topology", title = "Global topology of trend signatures", y = "")+
  theme_bw()

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig4_global_topology.png"), 
       width = 8, height = 4)
