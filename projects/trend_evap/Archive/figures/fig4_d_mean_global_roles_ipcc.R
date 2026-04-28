# Figure 4 - Showcase Topology -----
source('source/evap_trend.R')

## Data ----
global_topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_dataset_trend_topology.rds"))
global_topology <- global_topology[p_value == "p <= 0.05"]
dataset_order <- global_topology[, dataset[order(-rank_DCI_opposer)]]

topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ref_regions_dataset_trend_topology.rds"))
topology <- topology[p_value == "p <= 0.05"]
topology_sel <- topology[rank_DCI_opposer < 5]

ggplot(topology_sel)+
  geom_bar(aes(group = rank_DCI_opposer, x = rank_neg_signal), fill = "darkblue", stat = "count")+
  geom_bar(aes(group = rank_DCI_opposer, x = rank_pos_signal), 
           fill = "darkred", stat = "count", width = 0.8, alpha = 0.8)+
  facet_wrap(~IPCC_ref_region)+
  theme_bw()


topology_sel <- topology[rank_opposer < 4]

ggplot(topology_sel)+
  geom_bar(aes(group = rank_opposer, x = rank_neg_signal), fill = "darkblue", stat = "count")+
  geom_bar(aes(group = rank_opposer, x = rank_pos_signal), 
           fill = "darkred", stat = "count", width = 0.8, alpha = 0.8)+
  facet_wrap(~IPCC_ref_region)+
  theme_bw()

  
