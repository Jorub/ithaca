# Fig SI support tile plots for over and underestimator and best
# heatplots
source('source/partition_evap.R')
source('source/graphics.R')

library(ggpubr)

evap_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "grid_performance_datasets.rds"))
cols_data_dt <- data.table(dataset = names(cols_data), colors = cols_data) 

evap_summary_over <- evap_summary[performance == "Over"]
evap_summary_over[, rank := rank(area_fraction)]
evap_summary_over <- merge(evap_summary_over, cols_data_dt, by = "dataset")  
evap_summary_over <- evap_summary_over[order(rank)]
evap_summary[, dataset := factor(dataset, levels = evap_summary_over$dataset)]
evap_summary[performance == "Over", performance := "Higher"]
evap_summary[performance == "Under", performance := "Lower"]


performance_global <- ggplot(evap_summary)+
  geom_bar(aes(x = dataset, y = area_fraction, fill = performance), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Higher" = colset_RdBu_5[5], "Lower" = colset_RdBu_5[1]))+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.y = element_text(colour = evap_summary_over$colors),
        legend.title = element_text(hjust = 0.5))+
  labs(x = "Datasets", y = "Global area fraction [-]", fill = "Deviation to\nensemble mean")+
  coord_flip()



ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig5_SI_dataset_global_area.png"), 
       width = 8, height = 4)
