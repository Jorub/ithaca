# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/graphics.R')

library(ggpubr)

## data ----
data_ma_quartile <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "quartile_agreement_ma_basins.rds"))
data_ma_distribution <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_ma_basins.rds"))

## figures ----
fig_agreement_quartile <- ggplot(data_ma_quartile) +
  geom_bar(aes(x = as.factor(ma_basin), y = evap_volume_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Basin Number')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10))


fig_agreement_distribution <- ggplot(data_ma_distribution) +
  geom_bar(aes(x = as.factor(ma_basin), y = evap_volume_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Basin Number')  +
  ylab('Fraction [-]')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.margin = margin(r = 10, l = 10))

ggarrange(fig_agreement_quartile, fig_agreement_distribution, nrow = 2)
