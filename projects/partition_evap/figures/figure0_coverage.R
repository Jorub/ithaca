# Map of data count and product coverage ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')

library(rnaturalearth)
library(ggpubr)

## Load data ----

evap_data_coverage <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_data_count.rds"))

## plots ---- 
### data count ----


map_coverage <- ggplot() +
  geom_tile(data = evap_data_coverage, aes(fill = as.factor(data_count),
                                           col = as.factor(data_count), x = lon, y = lat)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = c(
    "darkblue",  
              "royalblue4",  
              "royalblue3",  
              "royalblue1",  
              "lightblue",  
              "darkorchid",  
              "darkred",  
              "firebrick",  
              "orange4",  
              "orange3",  
              "orange1",  
              "gold", 
              "darkgreen", 
              "gray80" 
  )) +
  scale_color_manual(values = c(
    "darkblue",  
              "royalblue4",  
              "royalblue3",  
              "royalblue1",  
              "lightblue",  
              "darkorchid",  
              "darkred",  
              "firebrick",  
              "orange4",  
              "orange3",  
              "orange1",  
              "gold", 
              "darkgreen", 
              "gray80"   
  ), guide = "none") +
  labs(x = NULL, y = NULL, fill = "Number of \ndatasets") +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-80, 90),
    expand = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-120, 120, 60),
    labels = label_longitude
  ) +
  scale_y_continuous(
    breaks = seq(-60, 60, 30),
    labels = label_latitude
  ) +
  theme_map_SI +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_map_data_count.pdf"), 
       width = figure_widths/2,
       height = figure_widths/4.4,
       units = "cm",
       plot = map_coverage,
       device = cairo_pdf)
