# Figure 2: agreement maps at different aggregation scales ----
source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")

library(data.table)
library(sf)
library(stars)
library(raster)


# Data 1 degree ----
agreement_1deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_1deg.rds"))

agreement_1deg_ma <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                "magnitude_agreement_1deg.rds"))

agreement_1deg <- merge(agreement_1deg, agreement_1deg_ma, by = c("deg_1_lon", "deg_1_lat"))

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

agreement_1deg[, dist_dataset_agreement := factor(
  dist_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]


# Panel a: Magnitude agreement ----

## prep ----
to_plot_dt <- agreement_1deg[, .(deg_1_lon, deg_1_lat, rel_dataset_agreement,
                                              value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_1_lon, deg_1_lat, value)] %>% 
  rasterFromXYZ(res = c(1, 1),
                crs = "+proj=longlat +datum=WGS88 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  rel_dataset_agreement
]

to_plot_stars$rel_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$rel_dataset_agreement <- factor(
  to_plot_stars$rel_dataset_agreement,
  levels = agreement_levels
)

levels(to_plot_stars$rel_dataset_agreement)

## plot ----
fig_a_1deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = rel_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Magnitude agreement 1° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg


# Panel b: distribution agreement ----
## prep ----
to_plot_dt <- agreement_1deg[, .(deg_1_lon, deg_1_lat, dist_dataset_agreement,
                                              value = as.numeric(dist_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, dist_dataset_agreement)]
)

problem_to_val[, dist_dataset_agreement := factor(dist_dataset_agreement,
                                                  levels = agreement_levels,
                                                  ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_1_lon, deg_1_lat, value)] %>% 
  rasterFromXYZ(res = c(1, 1),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  dist_dataset_agreement
]

to_plot_stars$dist_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$dist_dataset_agreement <- factor(
  to_plot_stars$dist_dataset_agreement,
  levels = agreement_levels
)

## plot ----
fig_b_1deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = dist_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Distribution agreement 1° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg

## join plots ----
fig2_1deg <- ggarrange(fig_a_1deg, fig_b_1deg)

# Data 2 degrees----
agreement_2deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_2deg.rds"))

agreement_2deg_ma <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                    "magnitude_agreement_2deg.rds"))

agreement_2deg <- merge(agreement_2deg, agreement_2deg_ma, by = c("deg_2_lon", "deg_2_lat"))

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

agreement_2deg[, dist_dataset_agreement := factor(
  dist_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]


# Panel a: Magnitude agreement ----

## prep ----
to_plot_dt <- agreement_2deg[, .(deg_2_lon, deg_2_lat, rel_dataset_agreement,
                                 value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_2_lon, deg_2_lat, value)] %>% 
  rasterFromXYZ(res = c(2, 2),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  rel_dataset_agreement
]

to_plot_stars$rel_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$rel_dataset_agreement <- factor(
  to_plot_stars$rel_dataset_agreement,
  levels = agreement_levels
)

levels(to_plot_stars$rel_dataset_agreement)

## plot ----
fig_a_2deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = rel_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Magnitude agreement 2° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg


# Panel b: distribution agreement ----
## prep ----
to_plot_dt <- agreement_2deg[, .(deg_2_lon, deg_2_lat, dist_dataset_agreement,
                                 value = as.numeric(dist_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, dist_dataset_agreement)]
)

problem_to_val[, dist_dataset_agreement := factor(dist_dataset_agreement,
                                                  levels = agreement_levels,
                                                  ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_2_lon, deg_2_lat, value)] %>% 
  rasterFromXYZ(res = c(2, 2),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  dist_dataset_agreement
]

to_plot_stars$dist_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$dist_dataset_agreement <- factor(
  to_plot_stars$dist_dataset_agreement,
  levels = agreement_levels
)

## plot ----
fig_b_2deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = dist_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Distribution agreement 2° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg

## joint plots ----
fig2_2deg <- ggarrange(fig_a_2deg, fig_b_2deg)

# Data 4 degrees----
agreement_4deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_4deg.rds"))

agreement_4deg_ma <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                    "magnitude_agreement_4deg.rds"))

agreement_4deg <- merge(agreement_4deg, agreement_4deg_ma, by = c("deg_4_lon", "deg_4_lat"))

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

agreement_4deg[, dist_dataset_agreement := factor(
  dist_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]


# Panel a: Magnitude agreement ----

## prep ----
to_plot_dt <- agreement_4deg[, .(deg_4_lon, deg_4_lat, rel_dataset_agreement,
                                 value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_4_lon, deg_4_lat, value)] %>% 
  rasterFromXYZ(res = c(4, 4),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  rel_dataset_agreement
]

to_plot_stars$rel_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$rel_dataset_agreement <- factor(
  to_plot_stars$rel_dataset_agreement,
  levels = agreement_levels
)

levels(to_plot_stars$rel_dataset_agreement)

## plot ----
fig_a_4deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = rel_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Magnitude agreement 4° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg


# Panel b: distribution agreement ----
## prep ----
to_plot_dt <- agreement_4deg[, .(deg_4_lon, deg_4_lat, dist_dataset_agreement,
                                 value = as.numeric(dist_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, dist_dataset_agreement)]
)

problem_to_val[, dist_dataset_agreement := factor(dist_dataset_agreement,
                                                  levels = agreement_levels,
                                                  ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_4_lon, deg_4_lat, value)] %>% 
  rasterFromXYZ(res = c(4, 4),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  dist_dataset_agreement
]

to_plot_stars$dist_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$dist_dataset_agreement <- factor(
  to_plot_stars$dist_dataset_agreement,
  levels = agreement_levels
)

## plot ----
fig_b_4deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = dist_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Distribution agreement 4° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg

## joint plots ----
fig2_4deg <- ggarrange(fig_a_4deg, fig_b_4deg)
fig2_4deg  <- annotate_figure(
  fig2_4deg ,
  top = text_grob("4° aggregation", face = "bold", size = 14)
)
# Data 8 degrees----
agreement_8deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                 "distribution_agreement_8deg.rds"))

agreement_8deg_ma <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                    "magnitude_agreement_8deg.rds"))

agreement_8deg <- merge(agreement_8deg, agreement_8deg_ma, by = c("deg_8_lon", "deg_8_lat"))

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

agreement_8deg[, dist_dataset_agreement := factor(
  dist_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]


# Panel a: Magnitude agreement ----

## prep ----
to_plot_dt <- agreement_8deg[, .(deg_8_lon, deg_8_lat, rel_dataset_agreement,
                                 value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_8_lon, deg_8_lat, value)] %>% 
  rasterFromXYZ(res = c(8, 8),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  rel_dataset_agreement
]

to_plot_stars$rel_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$rel_dataset_agreement <- factor(
  to_plot_stars$rel_dataset_agreement,
  levels = agreement_levels
)

levels(to_plot_stars$rel_dataset_agreement)

## plot ----
fig_a_8deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = rel_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Magnitude agreement 8° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg


# Panel b: distribution agreement ----
## prep ----
to_plot_dt <- agreement_8deg[, .(deg_8_lon, deg_8_lat, dist_dataset_agreement,
                                 value = as.numeric(dist_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, dist_dataset_agreement)]
)

problem_to_val[, dist_dataset_agreement := factor(dist_dataset_agreement,
                                                  levels = agreement_levels,
                                                  ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_8_lon, deg_8_lat, value)] %>% 
  rasterFromXYZ(res = c(8, 8),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  dist_dataset_agreement
]

to_plot_stars$dist_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$dist_dataset_agreement <- factor(
  to_plot_stars$dist_dataset_agreement,
  levels = agreement_levels
)

## plot ----
fig_b_8deg <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = dist_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Distribution agreement 8° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg


## join plots ----
fig2_8deg <- ggarrange(fig_a_8deg, fig_b_8deg)
fig2_8deg  <- annotate_figure(
  fig2_8deg ,
  top = text_grob("8° aggregation", face = "bold", size = 14)
)


fig2_aggregation_ma <- ggarrange(fig_a_1deg, fig_a_2deg,
                                 fig_a_4deg, fig_a_8deg, 
                              nrow = 4, common.legend = T, legend = "right",
                              labels = c("a", "c", "e", "g"))

fig2_aggregation_da <- ggarrange(fig_b_1deg, fig_b_2deg,
                                 fig_b_4deg, fig_b_8deg, 
                                 nrow = 4, common.legend = T, legend = "right",
                                 labels = c("b", "d", "f", "h"))

fig2_aggregation <- ggarrange(fig2_aggregation_ma, 
                              fig2_aggregation_da, 
                              ncol = 2)

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig2_scale_agreement_maps.png"
  ),
  plot = fig2_aggregation,
  width = figure_widths,
  height = 23*1.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig2_scale_agreement_maps.pdf"
  ),
  plot = fig2_aggregation,,
  width = figure_widths,
  height = 23*1.4,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

