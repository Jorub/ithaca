# Supplementary figure: Masks ----
source('source/geo_functions.R')
source('source/graphics.R')
source('source/evap_trend.R')
source('source/partition_evap.R')

library(rnaturalearth)
library(dplyr)

# Data ----
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

levels(evap_mask$land_cover_short_class) <- c("Barren", "Croplands", "Forests", "Grasslands", "Other", "Savannas", 
                                              "Shrublands", "Snow/Ice", "Water" )
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
IPCC_list <- evap_mask[, unique(as.character(IPCC_ref_region))]
IPCC_list <- IPCC_list[!is.na(IPCC_list)]

ipcc_sf_terr <- ipcc_sf[ipcc_sf$Acronym %in% IPCC_list,]

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

# Figures ----

## biomes ----

evap_mask[, biome_short_class := as.factor(biome_short_class)]

to_plot_sf <- evap_mask[, .(lon, lat, biome_short_class)
][, value := as.numeric(biome_short_class)]

mask_to_val <- unique(to_plot_sf[,(.(biome_short_class = biome_short_class, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all.x = T)

fig_biome_short_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = biome_short_class, fill = biome_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_biome) +
  scale_color_manual(values = colset_biome,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Biome") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 2) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 2) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_evap_biomes.png"), width = 8, height = 5)

## evap_quantile ----
levels(evap_mask$evap_quant) <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                  "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                  "0.8-0.9", "0.9-1")


to_plot_sf <- evap_mask[, .(lon, lat, evap_quant)
][, value := as.numeric(evap_quant)]

mask_to_val <- unique(to_plot_sf[,(.(evap_quant = evap_quant, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all.x = T)


fig_evap_quant_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = evap_quant, fill = evap_quant)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_prec_quant) +
  scale_color_manual(values = colset_prec_quant,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Evaporation\nquantile") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 2) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 2) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_evap_quantiles.png"), width = 8, height = 5)

## landcover ----

to_plot_sf <- evap_mask[, .(lon, lat, land_cover_short_class)
][, value := as.numeric(land_cover_short_class)]
mask_to_val <- unique(to_plot_sf[,(.(land_cover_short_class = land_cover_short_class, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all = T)

fig_landcover <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = land_cover_short_class, fill = land_cover_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_land_cover_short) +
  scale_color_manual(values = colset_land_cover_short,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Landcover") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 18),
        legend.position = "right")+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_landcover.png"), width = 8, height = 5)

## IPCC ----
evap_mask[, IPCC_ref_region := as.factor(IPCC_ref_region)]

to_plot_sf <- evap_mask[, .(lon, lat, IPCC_ref_region)
][, value := as.numeric(IPCC_ref_region)]

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_ipcc <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  geom_sf(data = ipcc_sf_terr , fill = "royalblue1", color = "gray23", alpha = 0.4) +
  geom_sf_text(data = ipcc_sf_terr, aes(label = Acronym), size = 2,
               fontface = "bold") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 2) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 2) +
  theme_bw() +
  ggtitle("IPCC reference regions v4")+
  theme(panel.background = element_rect(fill = NA), panel.ontop = FALSE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_ipcc.png"), width = 8, height = 5)


## Koeppen-Geiger ----
evap_mask[, KG_beck := as.factor(KG_beck)]

to_plot_sf <- evap_mask[, .(lon, lat, KG_beck)
][, value := as.numeric(KG_beck)]

mask_to_val <- unique(to_plot_sf[,(.(KG_beck = KG_beck, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all.x = T)


fig_KG <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = KG_beck, fill = KG_beck), lwd = 0.1) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_kg) +
  scale_color_manual(values = cols_kg,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Koeppen-Geiger\nclasses") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 18),
        legend.position = "right")+
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_KG.png"), width = 8, height = 5)

## Elevation ----
evap_mask[, elev_class := as.factor(elev_class)]

to_plot_sf <- evap_mask[, .(lon, lat, elev_class)
][, value := as.numeric(elev_class)]

mask_to_val <- unique(to_plot_sf[,(.(elev_class = elev_class, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all.x = T)


fig_elev <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = elev_class, fill = elev_class), lwd = 0.1) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_elev) +
  scale_color_manual(values = colset_elev,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Elevation\nclasses") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 18),
        legend.position = "right")+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
              "fig_SI_elev.png"), width = 8, height = 5)



