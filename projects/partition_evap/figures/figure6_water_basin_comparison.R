# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(ggpubr)
library(rnaturalearth)
library(dplyr)
library(ggrepel)

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

fig_box_2 <- ggplot(data_merged[(ma_basin %in% 2)])+
  geom_boxplot(aes(x = dataset_type, y = evap_mean, col = dataset))+
  scale_color_manual(values = cols_data)+
  facet_wrap(~ma_basin, scales = "free_y", ncol = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)),  
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14, margin(r = 10, l = 10)), 
        legend.margin = margin(r = 15, l = 10),
        strip.placement = "outside", legend.position = 'bottom',
        strip.background = element_rect(fill = "gray90", color = "black"))+
  labs(y = expression(paste('Evaporation [mm y'^-1,']')),
       x = '',
       col = 'Dataset')

fig_box_together <- ggplot(data_merged[(ma_basin != 2 & ma_basin %in% common_basins)])+
  geom_boxplot(aes(x = dataset_type, y = evap_mean, col = dataset))+
  scale_color_manual(values = cols_data)+
  facet_wrap(~ma_basin, scales = "free_y", ncol = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)),  
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14), 
        legend.margin = margin(r = 10, l = 10),
        strip.placement = "outside", legend.position = 'bottom',
        strip.background = element_rect(fill = "gray90", color = "black")
  )+
  labs(y = expression(paste('Evaporation [mm y'^-1,']')),
       x = '',
       col = 'Dataset')



## basins ----
# World and Land borders -----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
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


fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])

ma_sf_terr <- ma_sf[ma_sf$BasinID %in% common_basins,]

fig_ma <- ggplot() +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  geom_sf(data = ma_sf_terr , fill = "royalblue1", color = "gray23", alpha = 0.4) +
  #geom_sf_text(data = ma_sf_terr, aes(label = BasinID), size = 3,
  #             fontface = "bold") +
  geom_label_repel(
    data = ma_sf_terr,
    aes(label = BasinID, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,  # always draw the line
    box.padding = 0.5,
    point.padding = 0.1,
    size = 4,
    segment.color = "black",
    nudge_x = 0.15,          # adjust nudging as needed
    nudge_y = 0.15
  ) +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = FALSE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


fig_top <- ggarrange(fig_ma,fig_box_2, widths = c(2, 1.2),
                     common.legend = T, nrow = 1)

fig_all <- ggarrange(fig_top, fig_box_together, nrow = 2, heights = c(2,3),
                     common.legend = T, legend = 'none')
fig_all

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig6_water_balance_comparison.png"), 
       width = 8, height = 10)
