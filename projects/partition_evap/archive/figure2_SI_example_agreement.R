# Figure showing example of agreements

source("source/partition_evap.R")
agreement <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds"))

quart_high_dist_high <- agreement[rel_dataset_agreement == 'High' &
                                    dist_dataset_agreement == 'High' &
                                    evap_quant == "0.4-0.5"] 
quart_high_dist_high_sel <- quart_high_dist_high[1,]

quart_low_dist_high <- agreement[rel_dataset_agreement == 'Low' &
                                    dist_dataset_agreement == 'High'&
                                   evap_quant == "0.2-0.3"] 
quart_low_dist_high_sel <- quart_low_dist_high[1,]

quart_high_dist_low <- agreement[rel_dataset_agreement == 'High' &
                                   dist_dataset_agreement == 'Low' & evap_quant == "0.6-0.7"] 

quart_high_dist_low_sel <- quart_high_dist_low[1,]

quart_low_dist_low <- agreement[rel_dataset_agreement == 'Low' &
                                   dist_dataset_agreement == 'Low'&
                                  evap_quant == "0.4-0.5"] 
quart_low_dist_low_sel <- quart_low_dist_low[1,]

example_locations <- rbind(quart_high_dist_high_sel,
                           quart_low_dist_high_sel,
                           quart_high_dist_low_sel,
                           quart_low_dist_low_sel)

example_locations_data <- merge(example_locations, evap_datasets, by = c('lon', 'lat'))

example_locations_data[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
example_locations_data[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
example_locations_data[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
example_locations_data[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]

example_locations_data[rel_dataset_agreement == "High", 
                       rel_dataset_agreement := "High quartile\nagreement"]

example_locations_data[rel_dataset_agreement == "Low", 
                       rel_dataset_agreement := "Low quartile\nagreement"]

example_locations_data[dist_dataset_agreement == "High", 
                       dist_dataset_agreement := "High distribution\nagreement"]

example_locations_data[dist_dataset_agreement == "Low", 
                       dist_dataset_agreement := "Low distribution\nagreement"]

ggplot(example_locations_data)+
  geom_boxplot(aes(x = dataset_type, y = evap, col = dataset))+
  scale_color_manual(values = cols_data)+
  facet_grid(rel_dataset_agreement~dist_dataset_agreement, scales = 'free')+
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  labs(y = expression(paste('Evapotranspiration [mm year'^-1,']')), x = '')+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, 
              "supplement/fig2_SI_example_low_high_agreement.png"), 
       width = 8, height = 6)

