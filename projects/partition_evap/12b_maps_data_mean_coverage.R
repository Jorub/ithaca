# Map of data count and product coverage ----
source('source/partition_evap.R')

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

datalist <- unique(evap_datasets$dataset)

for(data in datalist){
  dummy <- merge(evap_data_count_coverage_lon_lat, 
                 evap_data_count_coverage_filter[dataset == data],
                 by = c('lon', 'lat'), all = T)  
  dummy[is.na(data_count), dataset := data]
  dummy[is.na(data_count), data_coverage := 'FALSE']
  if(data == datalist[1]){
    evap_coverage <- dummy
  }else{
    evap_coverage <- merge(evap_coverage, dummy, by = c('lon', 'lat', 'dataset', 'data_coverage', 'data_count'), all = T)
  }
}


## earth box ----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -160, lat = c(50, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")


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
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(data_count), fill = as.factor(data_count))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
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
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_map_data_count.png"), width = 8, height = 5)

### data coverage  ----

plot_dataset_coverage <- function(data){
  cols_coverage <- c('FALSE' = 'firebrick', 'TRUE' = 'gray80')
  evap_data <- evap_coverage[dataset == data]
  to_plot_sf <- evap_data[, .(lon, lat, dataset, data_coverage)
  ][, value := as.numeric(as.factor(data_coverage))]
  
  mask_to_val <- unique(to_plot_sf[,(.(data_coverage = as.character(data_coverage), 
                                       dataset = dataset,
                                       value = value))])
  
  to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
    rasterFromXYZ(res = c(0.25, 0.25),
                  crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_as_stars() %>% st_as_sf()
  
  to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all = T)
  
  fig_data <- ggplot(to_plot_sf) +
    geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
    geom_sf(aes(color = data_coverage, fill = data_coverage)) +
    geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
    labs(x = NULL, y = NULL, fill = 'Coverage') +
    scale_fill_manual(values = cols_coverage )+
    scale_color_manual(values = cols_coverage , guide = 'none')+
    coord_sf(expand = FALSE, crs = "+proj=robin") +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
    geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
          panel.border = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          panel.grid.major = element_line(colour = "gray60"),
          axis.text = element_blank(), 
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 16))+
    ggtitle(label = data)
  ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                "supplement/fig_SI_map_data_coverage_",data,".png"), 
         width = 8, height = 5)
  return(fig_data)
}

fig_bess <- plot_dataset_coverage(data = 'bess')
fig_camele <- plot_dataset_coverage(data = 'camele')
fig_era5L <- plot_dataset_coverage(data = 'era5-land')
fig_etmonitor <- plot_dataset_coverage(data = 'etmonitor')
fig_fldas <- plot_dataset_coverage(data = 'fldas')
fig_gldas_clsm <- plot_dataset_coverage(data = 'gldas-clsm')
fig_gldas_noah <- plot_dataset_coverage(data = 'gldas-noah')
fig_gldas_vic <- plot_dataset_coverage(data = 'gldas-vic')
fig_gleam <- plot_dataset_coverage(data = 'gleam')
fig_jra55 <- plot_dataset_coverage(data = 'jra55')
fig_merra2 <- plot_dataset_coverage(data = 'merra2')
fig_mod16a <- plot_dataset_coverage(data = 'mod16a')
fig_terraclimate <- plot_dataset_coverage(data = 'terraclimate')
fig_synthesizedet <- plot_dataset_coverage(data = 'synthesizedet')



