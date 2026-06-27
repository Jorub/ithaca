# Supplementary figure: Masks ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(dplyr)

# Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$land_cover_short_class) <- c("Barren", "Croplands", "Forests", "Grasslands", "Other", "Savannas", 
                                              "Shrublands", "Snow/Ice", "Water" )
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
IPCC_list <- evap_mask[, unique(as.character(IPCC_ref_region))]
IPCC_list <- IPCC_list[!is.na(IPCC_list)]

ipcc_sf_terr <- ipcc_sf[ipcc_sf$Acronym %in% IPCC_list,]

# Figures ----

## biomes ----

evap_mask[, biome_short_class := as.factor(biome_short_class)]

to_plot_dt <- evap_mask[, .(lon, lat, biome_short_class,
                                              value = as.numeric(biome_short_class))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, biome_short_class)]
)


to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  biome_short_class
]

biome_levels <- unique(evap_mask$biome_short_class)

to_plot_stars$biome_short_class <- array(
  factor(
    rel_vals,
    levels = biome_levels
  ),
  dim = dim(to_plot_stars$value)
)

fig_biome_short_class <- ggplot(to_plot_sf) +
  geom_sf(aes(color = biome_short_class, fill = biome_short_class)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = colset_biome) + 
  scale_color_manual(values = colset_biome,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  ggtitle("Landcover")+
  theme_map_SI


### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_evap_biomes.png"), 
       width = 8, height = 5,
       plot = fig_biome_short_class,
       dpi = 300)

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
  geom_sf(aes(color = land_cover_short_class, fill = land_cover_short_class)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = colset_land_cover_short) +
  scale_color_manual(values = colset_land_cover_short,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  ggtitle("Landcover")+
  theme_map_SI+
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

#### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_landcover.png"), 
       width = 8, 
       height = 5,
       dpi = 300,
       plot = fig_landcover)

## evap_quantile ----
levels(evap_mask$evap_quant) <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                  "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                  "0.8-0.9", "0.9-1")


to_plot_sf <- evap_mask[, .(lon, lat, evap_quant)
][, value := as.numeric(evap_quant)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(evap_quant = 
                                      case_when(to_plot_sf$value == 1 ~ "0-0.1", 
                                                to_plot_sf$value == 2 ~ "0.1-0.2", 
                                                to_plot_sf$value == 3 ~ "0.2-0.3", 
                                                to_plot_sf$value == 4 ~ "0.3-0.4", 
                                                to_plot_sf$value == 5 ~ "0.4-0.5", 
                                                to_plot_sf$value == 6 ~ "0.5-0.6", 
                                                to_plot_sf$value == 7 ~ "0.6-0.7", 
                                                to_plot_sf$value == 8 ~ "0.7-0.8", 
                                                to_plot_sf$value == 9 ~ "0.8-0.9", 
                                                to_plot_sf$value == 10 ~ "0.9-1"))

to_plot_sf$evap_quant <- factor(to_plot_sf$evap_quant, 
                                levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                           "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                           "0.8-0.9", "0.9-1"), ordered = TRUE)
fig_evap_quant_class <- ggplot(to_plot_sf) +
  geom_sf(aes(color = evap_quant, fill = evap_quant)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = c(colset_prec_quant)) + 
  scale_color_manual(values = c(colset_prec_quant), 
                     guide = "none") + 
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  ggtitle("Evaporation quantiles")+
  theme_map_SI

### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_evap_quantiles.png"), 
       width = 8, 
       height = 5,
       dpi = 300,
       plot = fig_evap_quant_class)
       
## IPCC ----
evap_mask[, IPCC_ref_region := as.factor(IPCC_ref_region)]

to_plot_sf <- evap_mask[, .(lon, lat, IPCC_ref_region)
][, value := as.numeric(IPCC_ref_region)]

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_ipcc <- ggplot(to_plot_sf) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  geom_sf(data = ipcc_sf_terr , fill = "gold", color = "gray23", alpha = 0.4) +
  geom_sf_text(data = ipcc_sf_terr, aes(label = Acronym), size = 2,
               fontface = "bold") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  ggtitle("IPCC reference regions v4")+
  theme_map_SI

### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_ipcc.png"), 
       width = 8, 
       height = 5,
       dpi = 300,
       plot = fig_ipcc)

## Köppen Geiger ----

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
  geom_sf(aes(color = KG_beck, fill = KG_beck), lwd = 0.1) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = cols_kg) +
  scale_color_manual(values = cols_kg,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  ggtitle("Köppen-Geiger classes")+
  theme_map_SI

### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_kg_beck.png"), 
       width = 8, 
       height = 5,
       dpi = 300,
       plot = fig_KG)

## Elevation ----
to_plot_sf <- evap_mask[, .(lon, lat, elev_class)
][, value := as.numeric(elev_class)]
mask_to_val <- unique(to_plot_sf[,(.(elev_class = elev_class, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, mask_to_val, by = "value", all = T)

fig_elev_class <- ggplot(to_plot_sf) +
  geom_sf(aes(color = elev_class, fill = elev_class)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = c(colset_elev_mono)) + 
  scale_color_manual(values = c(colset_elev_mono), 
                     guide = "none") + 
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  ggtitle("Elevation classes")+
  theme_map_SI

### gg----
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/fig_SI_elevation.png"), 
       width = 8, 
       height = 5,
       dpi = 300,
       plot = fig_elev_class)