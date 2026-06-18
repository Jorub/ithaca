# Plots global maps of dataset agreement classes 
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)
library(ggpubr)
## Figure a ----
## Data
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_datasets_grid_mean <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))
distribution <-  distribution[!is.na(index),]


### Distribution agreement thresholds at quantile 0.1, 0.3, 0.7. 0.9
quant_dist_0_1 <- quantile(distribution$index, c(0.1))
quant_dist_0_3 <- quantile(distribution$index, c(0.3))
quant_dist_0_7 <- quantile(distribution$index, c(0.7))
quant_dist_0_9 <- quantile(distribution$index, c(0.9))

distribution[index <= quant_dist_0_1, agreement_fac := ordered(1, labels = "low")] 
distribution[index > quant_dist_0_1 & index <= quant_dist_0_3, agreement_fac := ordered(3, labels = "below average")]
distribution[index > quant_dist_0_3 & index <= quant_dist_0_7, agreement_fac := ordered(4, labels = "average")]
distribution[index > quant_dist_0_7 & index <= quant_dist_0_9, agreement_fac := ordered(5, labels = "above average")]
distribution[index > quant_dist_0_9, agreement_fac := ordered(7, labels = "high")]

levels(distribution$agreement_fac) <- c( "Low", "Below average", "Average",
                                         "Above average","High")

agreement <- merge(evap_mask[, .(lon, lat, rel_dataset_agreement, std_quant_range)], distribution, by = c("lon", "lat")) 
agreement[rel_dataset_agreement %in% c("high", "above average") & agreement_fac %in% c("High", "Above average"), common:= "Joint high & above average"]
agreement[rel_dataset_agreement %in% c("low", "below average") & agreement_fac %in% c("Low", "Below average"), common:= "Joint low & below average"]
agreement[rel_dataset_agreement %in% c("high", "above average") & agreement_fac %in% c("Low", "Below average"), common:= "High & above average quartile agreement\nLow & below average distribution agreement"]
agreement[rel_dataset_agreement %in% c("low", "below average") & agreement_fac %in% c("High", "Above average"), common:= "High & above average distribution agreement\nLow & below average quartile agreement"]


## World and Land borders ----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels
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

## Figures

### distribution index
to_plot_sf <- agreement[, .(lon, lat, common)
][, value := as.numeric(as.factor(common))]

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_overlap <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = colset_RdBu_5[c(4,3,5,1)], labels = levels(as.factor(agreement$common))) +
  scale_color_manual(values = colset_RdBu_5[c(4,3,5,1)],
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Agreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = FALSE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 1))+
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) 

###

lm_model <- evap_mask[, lm(ens_mean_q75-ens_mean_q25~ 0 + ens_mean_mean)]
summary(lm_model)

ggplot(evap_mask, aes(x = ens_mean_mean, y = ens_mean_q75-ens_mean_q25)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0.2029555, color = "red")+
  geom_density_2d() +
  labs(x = "Ensemble mean [mm year]", y = "Inter-quartile range [mm year]") +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = FALSE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16),
        legend.position = "bottom")



## Figue map data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,"dataset_agreement_grid_wise.rds"))

### World and Land borders ----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

### distribution index ----
to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, dist_dataset_agreement)
][, value := as.numeric(dist_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_distribution_index <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), labels = levels(dataset_agreement_grid_wise$dist_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Distribution\nagreement") +
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
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))


### Relative dataset agreement ----
to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_rel_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5),
                    labels = levels(dataset_agreement_grid_wise$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(dataset_agreement_grid_wise$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nagreement") +
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
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))


gg_agreement_v2 <- ggarrange(fig_distribution_index, fig_rel_dataset_agreement,
                             
                             labels = c('a', 'b', 'c', 'd'), font.label = list(size = 20), common.legend = FALSE,
                             legend = 'right',
                             nrow = 2, ncol = 2)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig2_main_dataset_range_agreement_maps.png"), 
     width = 20, height = 11, res = 300, units = 'in')
gg_agreement_v2
dev.off()
