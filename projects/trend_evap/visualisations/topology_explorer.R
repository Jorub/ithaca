# Visualize topology ----
## Opposers, positive and negative signal boosters, no trenders
source('source/evap_trend.R')
source('projects/trend_evap/visualisations/functions_main_topology_masks.R')

# category: global, IPCC, elevation, Koeppen_Geiger, biomes, landcover, evap_quantile ----

## Example 1 Topology over evaporation quantiles ----
### get data ----
data <- get_data(dataset = 'evap_quantile')

variable_list <- levels(data$evap_quant)

### Lowest quantile ----
data_to_view <- data[evap_quant == variable_list[1]]

### Visualize topology ranks of all datasets ----
visualise_topology_rank(data = data_to_view)

### Highest quantile ----
data_to_view <- data[evap_quant == variable_list[10]]

### Visualize topology ranks of all datasets ----
visualise_topology_rank(data = data_to_view)

### Explore single dataset
dataset_list <- data[, unique(dataset)]
dataset_list

data_bess <- data[dataset == dataset_list[1]]
data_bess

ggplot(data_bess[p_value == 'p <= 0.05'])+
  geom_point(aes(y = evap_quant, x = rank_opposer))+
  theme_bw()

ggplot(data_bess[p_value == 'p <= 0.05'])+
  geom_point(aes(y = evap_quant, x = rank_neg_signal))+
  theme_bw()

## Example 2 Topology over IPCC reference regions ----
data <- get_data(dataset = 'IPCC')
### View column names ----
names(data)

### Example 2.1: Dataset Fingerprint

data_mean <- data[p_value == 'p <= 0.05', .(mean_rank_DCI_opposer = mean(rank_DCI_opposer),
                      mean_rank_opposer = mean(rank_opposer),
                      mean_rank_significance = mean(rank_significance_opposer),
                      mean_rank_pos_signal = mean(rank_pos_signal),
                      mean_rank_neg_signal = mean(rank_neg_signal),
                      mean_rank_no_trend = mean(rank_no_trenders, na.rm = T)
                      ), .(dataset)]


## Example 2.2: Select IPCC region ----
IPCC_list <- data[,unique(IPCC_ref_region)]

### Filter data ----
data_to_view <- data[IPCC_ref_region == 'NEU']

### Visualize topology ranks of all datasets ----
visualise_topology_rank(data = data_to_view)



### Example 2.3: Explore dataset of interest
dataset_list <- data[, unique(dataset)]
dataset_list

data_bess <- data[dataset == dataset_list[1]]
data_bess

# summary ranks
data_bess[,mean(rank_DCI_opposer)]
data_bess[,mean(rank_opposer)]
data_bess[,mean(rank_significance_opposer)]
data_bess[,mean(rank_neg_signal)]
data_bess[,mean(rank_pos_signal)]
data_bess[,mean(rank_no_trenders, na.rm =T)]

# View where BESS is the strongest opposer of significant trends (lower rank)
data_bess[IPCC_ref_region %in% IPCC_Africa, region := "Africa"]
data_bess[IPCC_ref_region %in% IPCC_Asia, region := "Asia"]
data_bess[IPCC_ref_region %in% IPCC_Australasia, region := "Australasia"]
data_bess[IPCC_ref_region %in% IPCC_Europe, region := "Europe"]
data_bess[IPCC_ref_region %in% IPCC_Namerica, region := "North America"]
data_bess[IPCC_ref_region %in% IPCC_Samerica, region := "South America"]

ggplot(data_bess[p_value == 'p <= 0.05'])+
  geom_point(aes(y = IPCC_ref_region, x = rank_neg_signal))+
  geom_vline(xintercept = 7)+
  facet_wrap(~region, scales = 'free_y')+
  theme_bw()

ggplot(data_bess[p_value == 'p <= 0.05'])+
  geom_point(aes(y = IPCC_ref_region, x = rank_opposer))+
  facet_wrap(~region, scales = 'free_y')+
  theme_bw()

# Return IPCC reference regions with BESS signals
data_bess[p_value == 'p <= 0.05' & rank_neg_signal < 6, IPCC_ref_region]
data_bess[p_value == 'p <= 0.05' & rank_pos_signal < 6, IPCC_ref_region]
data_bess[p_value == 'p <= 0.05' & rank_no_trenders < 6, IPCC_ref_region]

# Return IPCC reference regions where BESS is top opposer (compared to other datasets in ensemble)
data_bess[p_value == 'p <= 0.05' & rank_opposer < 6, IPCC_ref_region]
data_bess[p_value == 'p <= 0.05' & rank_DCI_opposer < 6, IPCC_ref_region]
data_bess[p_value == 'p <= 0.05' & rank_significance_opposer < 6, IPCC_ref_region]


# Return IPCC reference regions where BESS is least opposing (compared to other datasets in ensemble)
data_bess[p_value == 'p <= 0.05' & rank_opposer > 9, IPCC_ref_region]

