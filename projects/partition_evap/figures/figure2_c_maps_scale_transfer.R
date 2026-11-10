# Figure 2: agreement maps at different aggregation scales ----
source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")

library(data.table)
library(sf)
library(stars)
library(raster)

# Data 8 degrees----
agreement_stats_scale <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))
agreement_stats_scale[, area_8deg := sum(area), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, N_8deg := .N, .(deg_8_lon, deg_8_lat)]

agreement_stats_scale[, `:=`(
  area_8deg = sum(area, na.rm = TRUE),
  N_8deg = .N,
  higher_agreement_frac_8deg = sum(
    area[rel_dataset_agreement %in% c("High", "Above average")],
    na.rm = TRUE
  ) / sum(area, na.rm = TRUE)
), .(deg_8_lon, deg_8_lat)]


thresholds_sIQR <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_sIQR.RDS'))
agreement_stats_scale[sIQR_8deg > thresholds_sIQR[5], rel_da_8deg := ordered(5, labels = "Low")]
agreement_stats_scale[sIQR_8deg > thresholds_sIQR[4] & sIQR_8deg <= thresholds_sIQR[5], rel_da_8deg := ordered(4, labels = "Below average")]
agreement_stats_scale[sIQR_8deg > thresholds_sIQR[3] & sIQR_8deg <= thresholds_sIQR[4], rel_da_8deg := ordered(3, labels = "Average")]
agreement_stats_scale[sIQR_8deg > thresholds_sIQR[2] & sIQR_8deg <= thresholds_sIQR[3], rel_da_8deg := ordered(2, labels = "Above average")]
agreement_stats_scale[sIQR_8deg <= thresholds_sIQR[2], rel_da_8deg := ordered(1, labels = "High")] 

agreement_8deg_unique <- unique(agreement_stats_scale[,.(deg_8_lon, deg_8_lat,
                                                         sIQR_8deg, rel_da_8deg, 
                                                         higher_agreement_frac_8deg,
                                                         N_8deg)])

agreement_8deg_ma <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, 
                                    "magnitude_agreement_8deg.rds"))

agreement_8deg <- merge(agreement_8deg_unique, agreement_8deg_ma, by = c("deg_8_lon", "deg_8_lat"))

agreement_8deg <- agreement_8deg[N_8deg > 128]

agreement_8deg[, classes:= "Other"]

agreement_8deg[rel_dataset_agreement %in% c("High", "Above average")
               & higher_agreement_frac_8deg > 0.5, 
                                            classes:= "Persistent"]

agreement_8deg[rel_dataset_agreement %in% c("High", "Above average")
               & higher_agreement_frac_8deg < 0.5, 
               classes:= "Emergent"]

agreement_8deg[!rel_dataset_agreement %in% c("High", "Above average")
               & higher_agreement_frac_8deg > 0.5, 
               classes:= "Lost"]

class_levels <- c("Other", "Persistent", "Emergent", "Lost")
agreement_8deg[, classes := factor(classes, levels = class_levels)]

saveRDS(agreement_8deg, paste0(PATH_SAVE_PARTITION_EVAP,"magnitude_da_scale_transfer_8deg.RDS"))

# Panel a: Magnitude agreement ----

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
fig_a_8deg <- ggplot() +
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
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_leg
