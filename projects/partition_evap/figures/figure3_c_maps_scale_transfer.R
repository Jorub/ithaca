# Figure 2: agreement transitions at different aggregation scales ----
library(data.table)
library(sf)
library(stars)
library(raster)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")

# Configuration ----
higher_agreement <- c("High", "Above average")
higher_agreement_area_threshold <- 0.5
agreement_levels <- c("Low", "Below average", "Average",
                      "Above average", "High")
class_levels <- c("Other", "Persistent", "Emergent", "Lost")

scale_transfer_cols <- c(
  "Other" = "#D9D9D9",
  "Persistent" = "#56B4E9",
  "Emergent" = "#006837",
  "Lost" = "#D55E00"
)

# Data ----
# This table contains both agreement metrics and the aggregation coordinates.
agreement_stats_scale <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"
))
thresholds_sIQR <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "thresholds_sIQR.RDS"
))
thresholds_dist <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "thresholds_distribution_agreement.RDS"
))

# Each row must represent one fine-grid cell before calculating area fractions.
stopifnot(!anyDuplicated(agreement_stats_scale[, .(lon, lat)]))

# Scale-specific classes determine transitions. Fixed 0.25-degree reference
# classes are retained separately as rel_da_* and dist_da_*.
# Exactly 50% is assigned to the majority group (Persistent or Lost).

# Data 1 degree ----
# Summarise all rows in each group so each coarse cell has one area fraction.
agreement_1deg_unique <- agreement_stats_scale[, .(
  area_1deg = sum(area, na.rm = TRUE),
  N_1deg = .N,
  higher_agreement_frac_1deg = sum(
    area[rel_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE),
  higher_distribution_frac_1deg = sum(
    area[dist_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE)
), .(deg_1_lon, deg_1_lat)]

agreement_1deg_ma <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_1deg.rds"
))
agreement_1deg_da <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_agreement_1deg.rds"
))

stopifnot(
  !anyDuplicated(agreement_1deg_ma[, .(deg_1_lon, deg_1_lat)]),
  !anyDuplicated(agreement_1deg_da[, .(deg_1_lon, deg_1_lat)])
)

## Magnitude agreement ----
agreement_1deg <- merge(
  agreement_1deg_unique[, .(deg_1_lon, deg_1_lat, area_1deg, N_1deg,
                              higher_agreement_frac_1deg)],
  agreement_1deg_ma, by = c("deg_1_lon", "deg_1_lat")
)
agreement_1deg <- agreement_1deg[N_1deg > min_1_deg_N]
stopifnot(!anyDuplicated(agreement_1deg[, .(deg_1_lon, deg_1_lat)]))

# Lower sIQR indicates higher magnitude agreement.
agreement_1deg[, rel_da_1deg := factor(fcase(
  sIQR_1deg > thresholds_sIQR[5], "Low",
  sIQR_1deg > thresholds_sIQR[4], "Below average",
  sIQR_1deg > thresholds_sIQR[3], "Average",
  sIQR_1deg > thresholds_sIQR[2], "Above average",
  sIQR_1deg <= thresholds_sIQR[2], "High",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_1deg[, classes := fcase(
  is.na(rel_dataset_agreement) |
    is.na(higher_agreement_frac_1deg), NA_character_,
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_1deg >= higher_agreement_area_threshold,
  "Persistent",
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_1deg < higher_agreement_area_threshold,
  "Emergent",
  !rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_1deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_1deg[, classes := factor(classes, levels = class_levels)]

## Distribution agreement ----
agreement_1deg_da <- merge(
  agreement_1deg_unique[, .(deg_1_lon, deg_1_lat, area_1deg, N_1deg,
                              higher_distribution_frac_1deg)],
  agreement_1deg_da, by = c("deg_1_lon", "deg_1_lat")
)
agreement_1deg_da <- agreement_1deg_da[N_1deg > min_1_deg_N]
stopifnot(!anyDuplicated(agreement_1deg_da[, .(deg_1_lon, deg_1_lat)]))

# Higher match ratio (index) indicates higher distribution agreement.
agreement_1deg_da[, dist_da_1deg := factor(fcase(
  index > thresholds_dist[5], "High",
  index > thresholds_dist[4], "Above average",
  index > thresholds_dist[3], "Average",
  index > thresholds_dist[2], "Below average",
  index <= thresholds_dist[2], "Low",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_1deg_da[, classes := fcase(
  is.na(dist_dataset_agreement) |
    is.na(higher_distribution_frac_1deg), NA_character_,
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_1deg >= higher_agreement_area_threshold,
  "Persistent",
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_1deg < higher_agreement_area_threshold,
  "Emergent",
  !dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_1deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_1deg_da[, classes := factor(classes, levels = class_levels)]

# Panel a: magnitude agreement 1° ----
## prep ----
to_plot_dt <- agreement_1deg[, .(
  deg_1_lon, deg_1_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_1_lon, deg_1_lat, value)] %>%
  rasterFromXYZ(res = c(1, 1),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_a_1deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Magnitude agreement 1° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

# Panel b: distribution agreement 1° ----
## prep ----
to_plot_dt <- agreement_1deg_da[, .(
  deg_1_lon, deg_1_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_1_lon, deg_1_lat, value)] %>%
  rasterFromXYZ(res = c(1, 1),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_b_1deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Distribution agreement 1° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

## join plots ----
fig2_1deg <- ggarrange(fig_a_1deg, fig_b_1deg)

# Data 2 degrees ----
# Summarise all rows in each group so each coarse cell has one area fraction.
agreement_2deg_unique <- agreement_stats_scale[, .(
  area_2deg = sum(area, na.rm = TRUE),
  N_2deg = .N,
  higher_agreement_frac_2deg = sum(
    area[rel_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE),
  higher_distribution_frac_2deg = sum(
    area[dist_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE)
), .(deg_2_lon, deg_2_lat)]

agreement_2deg_ma <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_2deg.rds"
))
agreement_2deg_da <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_agreement_2deg.rds"
))

stopifnot(
  !anyDuplicated(agreement_2deg_ma[, .(deg_2_lon, deg_2_lat)]),
  !anyDuplicated(agreement_2deg_da[, .(deg_2_lon, deg_2_lat)])
)

## Magnitude agreement ----
agreement_2deg <- merge(
  agreement_2deg_unique[, .(deg_2_lon, deg_2_lat, area_2deg, N_2deg,
                              higher_agreement_frac_2deg)],
  agreement_2deg_ma, by = c("deg_2_lon", "deg_2_lat")
)
agreement_2deg <- agreement_2deg[N_2deg > min_2_deg_N]
stopifnot(!anyDuplicated(agreement_2deg[, .(deg_2_lon, deg_2_lat)]))

# Lower sIQR indicates higher magnitude agreement.
agreement_2deg[, rel_da_2deg := factor(fcase(
  sIQR_2deg > thresholds_sIQR[5], "Low",
  sIQR_2deg > thresholds_sIQR[4], "Below average",
  sIQR_2deg > thresholds_sIQR[3], "Average",
  sIQR_2deg > thresholds_sIQR[2], "Above average",
  sIQR_2deg <= thresholds_sIQR[2], "High",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_2deg[, classes := fcase(
  is.na(rel_dataset_agreement) |
    is.na(higher_agreement_frac_2deg), NA_character_,
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_2deg >= higher_agreement_area_threshold,
  "Persistent",
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_2deg < higher_agreement_area_threshold,
  "Emergent",
  !rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_2deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_2deg[, classes := factor(classes, levels = class_levels)]

## Distribution agreement ----
agreement_2deg_da <- merge(
  agreement_2deg_unique[, .(deg_2_lon, deg_2_lat, area_2deg, N_2deg,
                              higher_distribution_frac_2deg)],
  agreement_2deg_da, by = c("deg_2_lon", "deg_2_lat")
)
agreement_2deg_da <- agreement_2deg_da[N_2deg > min_2_deg_N]
stopifnot(!anyDuplicated(agreement_2deg_da[, .(deg_2_lon, deg_2_lat)]))

# Higher match ratio (index) indicates higher distribution agreement.
agreement_2deg_da[, dist_da_2deg := factor(fcase(
  index > thresholds_dist[5], "High",
  index > thresholds_dist[4], "Above average",
  index > thresholds_dist[3], "Average",
  index > thresholds_dist[2], "Below average",
  index <= thresholds_dist[2], "Low",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_2deg_da[, classes := fcase(
  is.na(dist_dataset_agreement) |
    is.na(higher_distribution_frac_2deg), NA_character_,
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_2deg >= higher_agreement_area_threshold,
  "Persistent",
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_2deg < higher_agreement_area_threshold,
  "Emergent",
  !dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_2deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_2deg_da[, classes := factor(classes, levels = class_levels)]

# Panel a: magnitude agreement 2° ----
## prep ----
to_plot_dt <- agreement_2deg[, .(
  deg_2_lon, deg_2_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_2_lon, deg_2_lat, value)] %>%
  rasterFromXYZ(res = c(2, 2),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_a_2deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Magnitude agreement 2° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

# Panel b: distribution agreement 2° ----
## prep ----
to_plot_dt <- agreement_2deg_da[, .(
  deg_2_lon, deg_2_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_2_lon, deg_2_lat, value)] %>%
  rasterFromXYZ(res = c(2, 2),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_b_2deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Distribution agreement 2° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

## join plots ----
fig2_2deg <- ggarrange(fig_a_2deg, fig_b_2deg)

# Data 4 degrees ----
# Summarise all rows in each group so each coarse cell has one area fraction.
agreement_4deg_unique <- agreement_stats_scale[, .(
  area_4deg = sum(area, na.rm = TRUE),
  N_4deg = .N,
  higher_agreement_frac_4deg = sum(
    area[rel_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE),
  higher_distribution_frac_4deg = sum(
    area[dist_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE)
), .(deg_4_lon, deg_4_lat)]

agreement_4deg_ma <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_4deg.rds"
))
agreement_4deg_da <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_agreement_4deg.rds"
))

stopifnot(
  !anyDuplicated(agreement_4deg_ma[, .(deg_4_lon, deg_4_lat)]),
  !anyDuplicated(agreement_4deg_da[, .(deg_4_lon, deg_4_lat)])
)

## Magnitude agreement ----
agreement_4deg <- merge(
  agreement_4deg_unique[, .(deg_4_lon, deg_4_lat, area_4deg, N_4deg,
                              higher_agreement_frac_4deg)],
  agreement_4deg_ma, by = c("deg_4_lon", "deg_4_lat")
)
agreement_4deg <- agreement_4deg[N_4deg > min_4_deg_N]
stopifnot(!anyDuplicated(agreement_4deg[, .(deg_4_lon, deg_4_lat)]))

# Lower sIQR indicates higher magnitude agreement.
agreement_4deg[, rel_da_4deg := factor(fcase(
  sIQR_4deg > thresholds_sIQR[5], "Low",
  sIQR_4deg > thresholds_sIQR[4], "Below average",
  sIQR_4deg > thresholds_sIQR[3], "Average",
  sIQR_4deg > thresholds_sIQR[2], "Above average",
  sIQR_4deg <= thresholds_sIQR[2], "High",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_4deg[, classes := fcase(
  is.na(rel_dataset_agreement) |
    is.na(higher_agreement_frac_4deg), NA_character_,
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_4deg >= higher_agreement_area_threshold,
  "Persistent",
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_4deg < higher_agreement_area_threshold,
  "Emergent",
  !rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_4deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_4deg[, classes := factor(classes, levels = class_levels)]

## Distribution agreement ----
agreement_4deg_da <- merge(
  agreement_4deg_unique[, .(deg_4_lon, deg_4_lat, area_4deg, N_4deg,
                              higher_distribution_frac_4deg)],
  agreement_4deg_da, by = c("deg_4_lon", "deg_4_lat")
)
agreement_4deg_da <- agreement_4deg_da[N_4deg > min_4_deg_N]
stopifnot(!anyDuplicated(agreement_4deg_da[, .(deg_4_lon, deg_4_lat)]))

# Higher match ratio (index) indicates higher distribution agreement.
agreement_4deg_da[, dist_da_4deg := factor(fcase(
  index > thresholds_dist[5], "High",
  index > thresholds_dist[4], "Above average",
  index > thresholds_dist[3], "Average",
  index > thresholds_dist[2], "Below average",
  index <= thresholds_dist[2], "Low",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_4deg_da[, classes := fcase(
  is.na(dist_dataset_agreement) |
    is.na(higher_distribution_frac_4deg), NA_character_,
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_4deg >= higher_agreement_area_threshold,
  "Persistent",
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_4deg < higher_agreement_area_threshold,
  "Emergent",
  !dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_4deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_4deg_da[, classes := factor(classes, levels = class_levels)]

# Panel a: magnitude agreement 4° ----
## prep ----
to_plot_dt <- agreement_4deg[, .(
  deg_4_lon, deg_4_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_4_lon, deg_4_lat, value)] %>%
  rasterFromXYZ(res = c(4, 4),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_a_4deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Magnitude agreement 4° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

# Panel b: distribution agreement 4° ----
## prep ----
to_plot_dt <- agreement_4deg_da[, .(
  deg_4_lon, deg_4_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_4_lon, deg_4_lat, value)] %>%
  rasterFromXYZ(res = c(4, 4),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_b_4deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Distribution agreement 4° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

## join plots ----
fig2_4deg <- ggarrange(fig_a_4deg, fig_b_4deg)

# Data 8 degrees ----
# Summarise all rows in each group so each coarse cell has one area fraction.
agreement_8deg_unique <- agreement_stats_scale[, .(
  area_8deg = sum(area, na.rm = TRUE),
  N_8deg = .N,
  higher_agreement_frac_8deg = sum(
    area[rel_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE),
  higher_distribution_frac_8deg = sum(
    area[dist_dataset_agreement %in% higher_agreement], na.rm = TRUE
  ) / sum(area, na.rm = TRUE)
), .(deg_8_lon, deg_8_lat)]

agreement_8deg_ma <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_8deg.rds"
))
agreement_8deg_da <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_agreement_8deg.rds"
))

stopifnot(
  !anyDuplicated(agreement_8deg_ma[, .(deg_8_lon, deg_8_lat)]),
  !anyDuplicated(agreement_8deg_da[, .(deg_8_lon, deg_8_lat)])
)

## Magnitude agreement ----
agreement_8deg <- merge(
  agreement_8deg_unique[, .(deg_8_lon, deg_8_lat, area_8deg, N_8deg,
                              higher_agreement_frac_8deg)],
  agreement_8deg_ma, by = c("deg_8_lon", "deg_8_lat")
)
agreement_8deg <- agreement_8deg[N_8deg > min_8_deg_N]
stopifnot(!anyDuplicated(agreement_8deg[, .(deg_8_lon, deg_8_lat)]))

# Lower sIQR indicates higher magnitude agreement.
agreement_8deg[, rel_da_8deg := factor(fcase(
  sIQR_8deg > thresholds_sIQR[5], "Low",
  sIQR_8deg > thresholds_sIQR[4], "Below average",
  sIQR_8deg > thresholds_sIQR[3], "Average",
  sIQR_8deg > thresholds_sIQR[2], "Above average",
  sIQR_8deg <= thresholds_sIQR[2], "High",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_8deg[, classes := fcase(
  is.na(rel_dataset_agreement) |
    is.na(higher_agreement_frac_8deg), NA_character_,
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_8deg >= higher_agreement_area_threshold,
  "Persistent",
  rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_8deg < higher_agreement_area_threshold,
  "Emergent",
  !rel_dataset_agreement %in% higher_agreement &
    higher_agreement_frac_8deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_8deg[, classes := factor(classes, levels = class_levels)]

## Distribution agreement ----
agreement_8deg_da <- merge(
  agreement_8deg_unique[, .(deg_8_lon, deg_8_lat, area_8deg, N_8deg,
                              higher_distribution_frac_8deg)],
  agreement_8deg_da, by = c("deg_8_lon", "deg_8_lat")
)
agreement_8deg_da <- agreement_8deg_da[N_8deg > min_8_deg_N]
stopifnot(!anyDuplicated(agreement_8deg_da[, .(deg_8_lon, deg_8_lat)]))

# Higher match ratio (index) indicates higher distribution agreement.
agreement_8deg_da[, dist_da_8deg := factor(fcase(
  index > thresholds_dist[5], "High",
  index > thresholds_dist[4], "Above average",
  index > thresholds_dist[3], "Average",
  index > thresholds_dist[2], "Below average",
  index <= thresholds_dist[2], "Low",
  default = NA_character_
), levels = agreement_levels, ordered = TRUE)]

agreement_8deg_da[, classes := fcase(
  is.na(dist_dataset_agreement) |
    is.na(higher_distribution_frac_8deg), NA_character_,
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_8deg >= higher_agreement_area_threshold,
  "Persistent",
  dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_8deg < higher_agreement_area_threshold,
  "Emergent",
  !dist_dataset_agreement %in% higher_agreement &
    higher_distribution_frac_8deg >= higher_agreement_area_threshold,
  "Lost",
  default = "Other"
)]
agreement_8deg_da[, classes := factor(classes, levels = class_levels)]

# Panel a: magnitude agreement 8° ----
## prep ----
to_plot_dt <- agreement_8deg[, .(
  deg_8_lon, deg_8_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_8_lon, deg_8_lat, value)] %>%
  rasterFromXYZ(res = c(8, 8),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_a_8deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Magnitude agreement 8° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

# Panel b: distribution agreement 8° ----
## prep ----
to_plot_dt <- agreement_8deg_da[, .(
  deg_8_lon, deg_8_lat, classes, value = as.numeric(classes)
)]

problem_to_val <- unique(to_plot_dt[, .(value, classes)])
problem_to_val[, classes := factor(classes, levels = class_levels,
                                   ordered = TRUE)]

to_plot_stars <- to_plot_dt[, .(deg_8_lon, deg_8_lat, value)] %>%
  rasterFromXYZ(res = c(8, 8),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)
rel_vals <- problem_to_val[data.table(value = vals), on = "value", classes]

to_plot_stars$classes <- array(
  factor(rel_vals, levels = class_levels),
  dim = dim(to_plot_stars$value)
)
to_plot_stars$classes <- factor(to_plot_stars$classes, levels = class_levels)

## plot ----
fig_b_8deg <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = classes)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = scale_transfer_cols,
                    breaks = class_levels,
                    limits = class_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = scale_transfer_cols, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Higher agreement",
       title = "Distribution agreement 8° aggregation") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_leg

## join plots ----
fig2_8deg <- ggarrange(fig_a_8deg, fig_b_8deg)

# Join aggregation scales ----
fig2_aggregation_ma <- ggarrange(
  fig_a_1deg, fig_a_2deg, fig_a_4deg, fig_a_8deg,
  nrow = 4, common.legend = TRUE, legend = "right",
  labels = c("a", "c", "e", "g")
)
fig2_aggregation_da <- ggarrange(
  fig_b_1deg, fig_b_2deg, fig_b_4deg, fig_b_8deg,
  nrow = 4, common.legend = TRUE, legend = "right",
  labels = c("b", "d", "f", "h")
)
fig2_aggregation <- ggarrange(fig2_aggregation_ma, fig2_aggregation_da,
                              ncol = 2)

# Save data ----
saveRDS(agreement_1deg, paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_da_scale_transfer_1deg.RDS"
))
saveRDS(agreement_1deg_da, paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_da_scale_transfer_1deg.RDS"
))
saveRDS(agreement_2deg, paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_da_scale_transfer_2deg.RDS"
))
saveRDS(agreement_2deg_da, paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_da_scale_transfer_2deg.RDS"
))
saveRDS(agreement_4deg, paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_da_scale_transfer_4deg.RDS"
))
saveRDS(agreement_4deg_da, paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_da_scale_transfer_4deg.RDS"
))
saveRDS(agreement_8deg, paste0(
  PATH_SAVE_PARTITION_EVAP, "magnitude_da_scale_transfer_8deg.RDS"
))
saveRDS(agreement_8deg_da, paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_da_scale_transfer_8deg.RDS"
))

# Save figure ----
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig2_scale_agreement_transitions.png"),
  plot = fig2_aggregation,
  width = figure_widths,
  height = 23 * 1.4,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig2_scale_agreement_transitions.pdf"),
  plot = fig2_aggregation,
  width = figure_widths,
  height = 23 * 1.4,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
  bg = "white"
)
