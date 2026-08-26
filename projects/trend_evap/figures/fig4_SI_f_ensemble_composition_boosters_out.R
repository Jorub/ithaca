source('source/evap_trend.R')

topo_pos_out <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_pos_out.rds"))
topo_neg_out <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_neg_out.rds"))

pos_out_melt <- melt(topo_pos_out , id.vars = c("dataset"))
neg_out_melt <- melt(topo_neg_out , id.vars = c("dataset"))

ref_topo_pos_out <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_reference_ranks_pos_out.rds"))
ref_topo_neg_out <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_reference_ranks_neg_out.rds"))

merge_pos_out <- merge(pos_out_melt, ref_topo_pos_out, by = c("dataset", "variable"))
merge_neg_out <- merge(neg_out_melt, ref_topo_neg_out, by = c("dataset", "variable"))

merge_pos_out[, rank_diff := value - rank]
merge_neg_out[, rank_diff := value - rank]

merge_pos_out[variable == "pos_signal", plot_name := "Positive \nbooster"]
merge_pos_out[variable == "neg_signal", plot_name := "Negative \nbooster"]
merge_pos_out[variable == "signal_dampener", plot_name := "Signal \ndampener"]
merge_pos_out[variable == "opposing_majority_trend", plot_name := "Trend\nopposer"]
merge_pos_out[variable == "opposition_contributor", plot_name := "Opposition\ncontributor"]
merge_pos_out[variable == "opposing_significance", plot_name := "Significance \nopposer"]

merge_neg_out[variable == "pos_signal", plot_name := "Positive \nbooster"]
merge_neg_out[variable == "neg_signal", plot_name := "Negative \nbooster"]
merge_neg_out[variable == "signal_dampener", plot_name := "Signal \ndampener"]
merge_neg_out[variable == "opposing_majority_trend", plot_name := "Trend\nopposer"]
merge_neg_out[variable == "opposition_contributor", plot_name := "Opposition\ncontributor"]
merge_neg_out[variable == "opposing_significance", plot_name := "Significance \nopposer"]

merge_pos_out[, plot_name := factor(plot_name, levels = c("Trend\nopposer",
                                                          "Significance \nopposer",
                                                          "Opposition\ncontributor",
                                                          "Positive \nbooster",
                                                          "Negative \nbooster",
                                                          "Signal \ndampener"
))]
merge_neg_out[, plot_name := factor(plot_name, levels = c("Trend\nopposer",
                                                          "Significance \nopposer",
                                                          "Opposition\ncontributor",
                                                          "Positive \nbooster",
                                                          "Negative \nbooster",
                                                          "Signal \ndampener"
))]
merge_pos_out[, rank_difference_fac := cut(rank_diff, breaks = c(-8,-6,-4,-1.1,1.1,4,6,8),
                                           labels = c("[-7,-6]", "[-5,-4]","[-3,-2]",
                                                      "[-1,1]", "[2,3]", "[4,5]", "[6,7]"))]
merge_neg_out[, rank_difference_fac := cut(rank_diff, breaks = c(-8,-6,-4,-1.1,1.1,4,6,8),
                                           labels = c("[-7,-6]", "[-5,-4]","[-3,-2]",
                                                      "[-1,1]", "[2,3]", "[4,5]", "[6,7]"))]

fill_topology_rank <- c("[-7,-6]" = "#440154FF", "[-5,-4]"= "#414487FF","[-3,-2]" = "#7378BE",
                        "[-1,1]" ="gray90", "[2,3]" = "#E6F000", 
                        "[4,5]" = "#C7E020FF", "[6,7]"= "#7AD151FF")

ggplot(merge_pos_out)+
  geom_raster(aes(x = plot_name, fill = as.factor(rank_difference_fac), y = dataset))+
  labs(x = "Topology", 
       title = "Global topology stability for ensemble composition\nTwo strongest positive signal boosters removed", 
       y = "Dataset", fill = "After removal\nstronger \u2190 0 \u2192 weaker")+
  scale_fill_manual(values = fill_topology_rank)+
  geom_hline(yintercept = 0, color = "black", lwd = 1)+
  theme_bw()+
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggplot(merge_neg_out)+
  geom_raster(aes(x = plot_name, fill = as.factor(rank_difference_fac), y = dataset))+
  labs(x = "Topology", 
       title = "Global topology stability for ensemble composition\nTwo strongest negative signal boosters removed", 
       y = "Dataset", fill = "After removal\nstronger \u2190 0 \u2192 weaker")+
  scale_fill_manual(values = fill_topology_rank)+
  geom_hline(yintercept = 0, color = "black", lwd = 1)+
  theme_bw()+
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
