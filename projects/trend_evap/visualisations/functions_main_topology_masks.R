# Functions to help download and visualize topology ----
source('source/evap_trend.R')
source('projects/trend_evap/visualisations/functions_sub_topology_masks.R')

library(ggpubr)

get_data <- function(dataset){
  
  dataset_options <- c(
    "IPCC", "biome", "landuse",  "Koeppen_Geiger",
    "evap_quantile",  "elevation_classes"
  )
  if(!dataset %in% dataset_options){
    return('dataset input not valid.\n dataset options are', dataset_options)
    exit()
  }
  switch(dataset,
         IPCC = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ref_regions_dataset_trend_topology.rds"))},
         biome = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_dataset_trend_topology.rds"))},
         landuse = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_dataset_trend_topology.rds"))},
         Koeppen_Geiger = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_beck_dataset_trend_topology.rds"))},
         evap_quantile = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_quantiles_dataset_trend_topology.rds"))}, 
         elevation_classes = {data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_dataset_trend_topology.rds"))}
  )
  return(data)
}

visualise_topology_rank <- function(data){
  fig_signal_none <- ggplot(data)+
    geom_tile(aes(x = rank_no_trenders, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof Nonsignificant Trend", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  fig_signal_pos <- ggplot(data)+
    geom_tile(aes(x = rank_pos_signal, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof Significant Positive Trend", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  fig_signal_neg <- ggplot(data)+
    geom_tile(aes(x = rank_neg_signal, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof Significant Negative Trend", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  
  fig_opposers <- ggplot(data)+
    geom_tile(aes(x = 1, y = p_value, fill = dataset))+
    geom_tile(aes(x = rank_opposer, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof Outliers", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  
  fig_DCI_opposer <- ggplot(data)+
    geom_tile(aes(x = rank_DCI_opposer, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof DCI deviation", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  fig_significance_opposers <- ggplot(data)+
    geom_tile(aes(x = rank_significance_opposer, y = p_value, fill = dataset))+
    scale_fill_manual(values = cols_data_c)+
    labs(x = "Ranked by Area Fraction \nof Signal Opposer", fill = "Dataset ", y = "")+
    theme_minimal()+
    theme(axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.title = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))
  
  
  ggarrange(fig_DCI_opposer, fig_significance_opposers, fig_opposers, fig_signal_pos, fig_signal_neg, fig_signal_none, align = "hv",
            common.legend = T, nrow = 2, ncol = 3, labels = c("a", "b", "c", "d", "e", "f"))
  
}


