# Figure 5 - Showcase Topology -----
source('source/evap_trend.R')

## Data ----
global_topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))
dataset_order <- global_topology[, dataset[order(-opposing_majority_trend)]]

topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_regions_topology_rank_roles.rds"))

topology_melt <- melt(topology, id.vars = c("dataset", "IPCC_ref_region"))
topology_melt[variable == "pos_signal", plot_name := "Positive \nbooster"]
topology_melt[variable == "neg_signal", plot_name := "Negative \nbooster"]
topology_melt[variable == "signal_dampener", plot_name := "Signal \ndampener"]
topology_melt[variable == "opposing_majority_trend", plot_name := "Trend\nopposer"]
topology_melt[variable == "opposition_contributor", plot_name := "Opposition\ncontributor"]
topology_melt[variable == "opposing_significance", plot_name := "Significance \nopposer"]
topology_melt[, value_fac := as.factor(value)]
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


topology_melt[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
topology_melt[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
topology_melt[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
topology_melt[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
topology_melt[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
topology_melt[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]

topology_melt[, dataset := factor(dataset, levels = dataset_order)]


fill_topology_rank <- c("[1,2]" = "#440154FF", "[3,4]" = "#414487FF",
                        "[5,6]" = "#2A7B8EFF",  "[7,8]" = "#22A384FF",
                        "[9,10]" = "#7AD151FF", "[11,12]" = "#C7E020FF",
                        "[13,14]" = "#FDE725FF")

# Africa
ggplot(topology_melt[region == "Africa"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for African IPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_africa.png"), 
       width = 8, height = 8, bg = "white")

# Asia
ggplot(topology_melt[region == "Asia"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for Asian IPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_asia.png"), 
       width = 8, height = 8, bg = "white")

# Australasia
ggplot(topology_melt[region == "Australasia"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for Australasian IPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_australasia.png"), 
       width = 8, height = 8, bg = "white")


# Europe
ggplot(topology_melt[region == "Europe"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for European IPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_europe.png"), 
       width = 8, height = 8, bg = "white")

# North America
ggplot(topology_melt[region == "North America"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for North and Central American\nIPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_north_america.png"), 
       width = 8, height = 8, bg = "white")

# South America
ggplot(topology_melt[region == "South America"])+
  geom_point(aes(y = dataset, x = IPCC_ref_region, fill = Rank, size = Rank), shape = 21)+
  scale_fill_manual(values = fill_topology_rank, guide = "legend",
                    name = "Rank\nStrongest \u2192 Weakest") +
  scale_size_discrete(range = c(4,1),
                      name = "Rank\nStrongest \u2192 Weakest")+  
  labs(x = "", title = "Topology of trend signatures for South American IPCC reference regions",
       size = "", color = "Rank", y = "")+
  facet_wrap(~plot_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_topology_ipcc_south_america.png"), 
       width = 8, height = 8, bg = "white")
