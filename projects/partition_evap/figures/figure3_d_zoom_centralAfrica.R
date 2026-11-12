# Figure 2: zoom into central Africa ----

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")

library(data.table)
library(sf)
library(stars)
library(raster)

agreement_8deg <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                    "magnitude_da_scale_transfer_8deg.RDS"))

agreement_stats_scale <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))


data_filter <- agreement_stats_scale[between(lon, 18, 36) &
                                       between(lat, 5, 16)]


## prep ----
to_plot_dt <- data_filter[, .(lon, lat, rel_dataset_agreement,
                                              value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
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
fig_agreement <- ggplot() +
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
       title = "Magnitude agreement") +
  scale_y_continuous(breaks = seq(-15, 15, 30)) +
  coord_sf(
    xlim = c(18, 36),
    ylim = c(5, 16),
    expand = FALSE
  )+
  theme_map

## prep ----
to_plot_dt <- agreement_8deg[, .(deg_8_lon, deg_8_lat, classes,
                                 value = as.numeric(classes))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, classes)]
)

problem_to_val[, classes := factor(classes,
                                   levels = class_levels,
                                   ordered = T)]


to_plot_stars <- to_plot_dt[, .(deg_8_lon, deg_8_lat, value)] %>% 
  rasterFromXYZ(res = c(8, 8),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  classes
]

to_plot_stars$classes <- array(
  factor(
    rel_vals,
    levels = class_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$classes <- factor(
  to_plot_stars$classes,
  levels = class_levels
)

levels(to_plot_stars$classes)

scale_transfer_cols <- c(
  "Other"      = "#D9D9D9",
  "Persistent" = "#0072B2",
  "Emergent"   = "#009E73",
  "Lost"       = "#D55E00"
)

## plot ----
fig_change <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols, 
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols ,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Magnitude agreement 8° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(
    xlim = c(18, 36),
    ylim = c(5, 16),
    expand = FALSE
  )+  
  theme_map_leg

ggarrange(fig_agreement, fig_change, align = "hv")
