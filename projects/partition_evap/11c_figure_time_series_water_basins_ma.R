# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(ggpubr)

## data ----
data_ma_time <- read.table(paste0(PATH_MASK, "ma_et_al_water_balance/ETwb_56_ma_et_al.csv"), dec =',', sep =';', header = T)
data_ma_time <- data_ma_time[18:34,]
data_ma_time_long <- melt(as.data.table(data_ma_time), 
                          id.vars = 'Year', value.name = 'evap', variable.name = 'ma_basin')
data_ma_time_long[, ma_basin_chr := as.character(ma_basin)]
data_ma_time_long[, ma_basin := sapply(strsplit(ma_basin_chr, split = '_'), "[[", 2)]
data_ma_time_long[, ma_basin := as.factor(ma_basin)]
data_ma_time_long[, ma_basin_chr := NULL]
data_ma_time_long[, dataset := 'Ma']

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basin.rds"))
data[, year := as.numeric(as.character(year))]
data[,environment_volume := NULL]
data[,area_sum := NULL]
data <- data[year < 2017]
data_merged <- merge(data, data_ma_time_long, by.x = c('year', 'ma_basin', 'evap_mean', 'dataset'), by.y = c('Year', 'ma_basin', 'evap', 'dataset'), all = T)

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basins_merged.rds"))

## data ----
data_ma_quartile <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "quartile_agreement_ma_basins.rds"))
data_ma_distribution <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_ma_basins.rds"))

# Basins with agreement above average and high in over 50 % volume fration
data_sum_distribution <- data_ma_distribution[agreement_fac %in% c('High', 'Above average'), .(evap_volume_fraction = sum(evap_volume_fraction)), .(ma_basin)]
data_sum_quartile <- data_ma_quartile[rel_dataset_agreement %in% c('High', 'Above average'), .(evap_volume_fraction = sum(evap_volume_fraction)), .(ma_basin)]

basins_distribution <- data_sum_distribution[evap_volume_fraction >=0.5, ma_basin]
basins_quartile <- data_sum_quartile[evap_volume_fraction >=0.5, ma_basin]

data_merged[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
data_merged[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
data_merged[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
data_merged[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]
data_merged[dataset %in% 'Ma', dataset_type := "Water balance"]

data_merged[, ma_basin := as.numeric(as.factor(ma_basin))]

data_merged[ma_basin < 21, continent := 'North America']
data_merged[ma_basin > 20 & ma_basin < 27, continent := 'South America']
data_merged[ma_basin > 27 & ma_basin < 33, continent := 'Europe']
data_merged[ma_basin > 33 & ma_basin < 47, continent := 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent := 'Africa']
data_merged[ma_basin > 51, continent := 'Oceania']

common_basins <- basins_distribution[basins_distribution %in% basins_quartile]
fig_box <- ggplot(data_merged[(ma_basin %in% common_basins)])+
  geom_boxplot(aes(x = dataset_type, y = evap_mean, col = dataset))+
  scale_color_manual(values = cols_data)+
  facet_wrap(~continent+ma_basin, scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), axis.title.y = element_text(margin = margin(r = 10)),  
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10),
        strip.placement = "outside", legend.position = 'bottom')+
  labs(y = expression(paste('Evaporation [mm y'^-1,']')),
       x = '',
       col = 'Dataset')

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig_ET_basin_boxplots.png"), 
       width = 8, height = 8)

## calculate means and compare to water balance

data_merged_mean <- data_merged[ma_basin %in% common_basins, .(mean_evap = mean(evap_mean)), .(dataset, ma_basin)]
data_merged_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_merged_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

data_merged_mean[, diff_data := mean_evap-water_balance]
data_merged_mean[diff_data < 0, diff := 'under']
data_merged_mean[diff_data > 0, diff := 'over']
data_merged_mean <- data_merged_mean[!is.na(diff)]
data_merged_mean[, rank := rank(abs(diff_data)), .(ma_basin)]
ma_basin_under <- data_merged_mean[diff == 'under', .(under_count = .N), .(ma_basin)]
ma_basin_under <- ma_basin_under[order(under_count),]
data_merged_mean[, ma_basin := factor(ma_basin, levels = ma_basin_under$ma_basin)]

fig_rank <- ggplot(data_merged_mean, aes(x = rank, y = as.factor(ma_basin), 
                             fill = dataset, color = diff))+
  geom_tile(lwd = 2, linetype = 1, width = 0.8, height = 0.8) +
  scale_fill_manual(values = cols_data)+
  scale_color_manual(values = c('red', 'black'))+
  theme_bw()+
  labs(y = 'Basin',
       x = 'Rank according to mean ET difference',
       fill = 'Dataset',
       color = 'Estimate')+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), axis.title.y = element_text(margin = margin(r = 10)),  
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10))
  


ggarrange(fig_box, fig_rank, nrow = 2, common.legend = T, 
         heights = c(1.3, 1.0))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig_ET_basin_comparison.png"), 
       width = 8, height = 11)

## version 2

## figures ----
data_ma_quartile[, ma_basin := factor(ma_basin, levels = ma_basin_under$ma_basin)]
data_ma_distribution[, ma_basin := factor(ma_basin, levels = ma_basin_under$ma_basin)]

fig_agreement_quartile <- ggplot(data_ma_quartile[ma_basin %in% common_basins]) +
  geom_bar(aes(x = as.factor(ma_basin), y = evap_volume_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Basin')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10))


fig_agreement_distribution <- ggplot(data_ma_distribution[ma_basin %in% common_basins]) +
  geom_bar(aes(x = as.factor(ma_basin), y = evap_volume_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Basin')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10))

fig_rank_v2 <- ggplot(data_merged_mean, aes(x = as.factor(ma_basin), y = rank, 
                                         fill = dataset, color = diff))+
  geom_tile(lwd = 1, linetype = 1, width = 0.8, height = 0.8) +
  scale_fill_manual(values = cols_data)+
  scale_color_manual(values = c('red', 'black'))+
  theme_bw()+
  labs(x = 'Basin',
       y = expression(paste("Rank of |",bar(ET[Dataset]),"-",bar(ET[WB]),"|")),
       fill = 'Dataset',
       color = 'Estimate')+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)),  
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14), 
        legend.margin = margin(r = 10, l = 10))


ggarrange(fig_agreement_quartile, fig_agreement_distribution, 
          fig_rank_v2, nrow = 3, heights = c(1,1,1.3))
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig_ET_basin_agreement_rank.png"), 
       width = 8, height = 11)
