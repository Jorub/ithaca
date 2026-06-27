# Map of data count and product coverage ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')

library(rnaturalearth)
library(ggpubr)

## Load data ----
load("~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata")
load(paste0(PATH_SAVE_PARTITION_EVAP, "paths.Rdata"))

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets[, year_count := .N, .(dataset, lon, lat)]
evap_datasets <- evap_datasets[year_count == 20]

evap_datasets[, data_count := .N, .(lon, lat, year)]
evap_datasets <- evap_datasets[!(dataset == "etmonitor" & year == "2000")]
evap_datasets[dataset == "etsynthesis", dataset := "synthesizedet"]
evap_datasets <- evap_datasets[order(dataset)] 

evap_data_count <- unique(evap_datasets[,.(lon, lat, data_count)])
evap_data_count_coverage <- unique(evap_datasets[,.(lon, lat, data_count, dataset)])
evap_data_count_coverage_filter <- evap_data_count_coverage[data_count >= 1]
evap_data_count_coverage_lon_lat <- unique(evap_data_count_coverage_filter[,.(lon, lat)])
evap_data_count_coverage_lon_lat[, data_coverage:= 'TRUE']

## plots ---- 
### data count ----
to_plot_sf <- evap_data_count[, .(lon, lat, data_count)
][, value := as.numeric(as.factor(data_count))]

mask_to_val <- unique(to_plot_sf[,(.(data_count = data_count, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all.x = T)

fig_count_v2 <- ggplot(to_plot_sf) +
  geom_sf(aes(color = as.factor(data_count), fill = as.factor(data_count))) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
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
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_map_SI+
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_map_data_count.pdf"), 
       width = figure_widths,
       height = figure_widths,
       units = "cm",
       dpi = 300,
       device = cairo_pdf)
