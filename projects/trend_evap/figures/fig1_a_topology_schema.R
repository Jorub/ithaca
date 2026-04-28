# Figure 1 ----
# Topology example ----
## From time series to category ----

source('source/evap_trend.R')
source('source/geo_functions.R')

# Data ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_datasets <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_datasets_clean.rds"))
evap_trend_opposer <-readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_dataset_opposing_DCI.rds"))
evap_significance_opposer <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_dataset_opposing_significance.rds"))

# Positive booster ----
evap_trend_pos  <- evap_trend[p < 0.01 & slope > 10]
id <- 1
data_sel <- evap_trend_pos[id,]
evap_datasets_pos <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 

evap_datasets_pos <- evap_datasets_pos[,.(lat, lon, year, dataset, evap)]
evap_datasets_pos[, category := "Positive signal booster"]
evap_datasets_pos[, highlight := FALSE]
evap_datasets_pos[dataset == data_sel$dataset, highlight := TRUE]

# Negative booster ----
evap_trend_neg  <- evap_trend[p < 0.01 & slope < -10]
id <- 100
data_sel <- evap_trend_neg[id,]
evap_datasets_neg <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 

evap_datasets_neg <- evap_datasets_neg[,.(lat, lon, year, dataset, evap)]
evap_datasets_neg[, category := "Negative signal booster"]
evap_datasets_neg[, highlight := FALSE]
evap_datasets_neg[dataset == data_sel$dataset, highlight := TRUE]


# Signal damper ----
evap_trend_no <- evap_trend[p > 0.5]
id <- 100
data_sel <- evap_trend_no[id,]
evap_datasets_no <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 

evap_datasets_no <- evap_datasets_no[,.(lat, lon, year, dataset, evap)]
evap_datasets_no[, category := "Signal dampener"]
evap_datasets_no[, highlight := FALSE]
evap_datasets_no[dataset == data_sel$dataset, highlight := TRUE]

# Trend opposer ----
evap_trend_opposer <- evap_trend_opposer[DCI_0_05 > 0.5] 
evap_trend_opposer <- evap_trend_opposer[opposing_0_05 == 1] 
evap_trend_opposer <- evap_trend_opposer[p < 0.05]

id <- 10
data_sel <- evap_trend_opposer[id,]
evap_datasets_opp <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 

evap_datasets_opp <- evap_datasets_opp[,.(lat, lon, year, dataset, evap)]
evap_datasets_opp[, category := "Trend opposer"]
evap_datasets_opp[, highlight := FALSE]
evap_datasets_opp[dataset == data_sel$dataset, highlight := TRUE]

# Significance opposer ----
evap_significance_opposer_sel <- evap_significance_opposer[majority_0_05 == "none" & opposing_0_05 == 1 & p < 0.01 & N_none_0_05 > 12] 

id <- 7962
data_sel <- evap_significance_opposer_sel[id,]
evap_datasets_sig <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 
evap_datasets_sig <- evap_datasets_sig[,.(lat, lon, year, dataset, evap)]
evap_datasets_sig[, category := "Significance opposer"]
evap_datasets_sig[, highlight := FALSE]
evap_datasets_sig[dataset == data_sel$dataset, highlight := TRUE]

# Opposition contributor ----
evap_opposition_contributor_sel <- evap_significance_opposer[N_pos_0_05 > 4 & N_neg_0_05  == 1 & slope < -10, ] 
id = 1
data_sel <- evap_opposition_contributor_sel[id,]
evap_datasets_cont <- evap_datasets[lat == data_sel$lat & lon == data_sel$lon] 
evap_datasets_cont <- evap_datasets_cont[,.(lat, lon, year, dataset, evap)]
evap_datasets_cont[, category := "Opposition contributor"]
evap_datasets_cont[, highlight := FALSE]
evap_datasets_cont[dataset == data_sel$dataset, highlight := TRUE]

topology_time <- rbind(evap_datasets_pos,
                       evap_datasets_neg,
                       evap_datasets_no,
                       evap_datasets_opp,
                       evap_datasets_sig,
                       evap_datasets_cont)

topology_time[, year := as.numeric(as.character(year))]
topology_time[, category := factor(category, levels = c(
  "Positive signal booster", "Negative signal booster", "Signal dampener",
  "Trend opposer","Significance opposer","Opposition contributor"
))]

ggplot(topology_time)+
  geom_line(aes(x = year, y = evap, group = dataset), color = "gray")+
  geom_line(data = topology_time[highlight == TRUE],
            aes(x = year, y = evap, color = highlight, group = dataset), color = "darkred")+
  theme_bw()+
  labs(y = "Annual evapotranspiration [mm]")+
  facet_wrap(~category, scales = "free")

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig1_topology_methods.png"), 
       width = 8, height = 4, bg = "white")

# Save data to Zenodo
write.csv(topology_time, paste0(PATH_SAVE_EVAP_TREND_TABLES,"data_fig1_topology_methods.csv"))