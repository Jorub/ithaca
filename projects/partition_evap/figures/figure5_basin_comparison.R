# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(ggpubr)
library(rnaturalearth)
library(dplyr)
library(ggrepel)
library(scales)

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

data_ma_quartile <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "quartile_agreement_ma_basins.rds"))
data_ma_distribution <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_ma_basins.rds"))

# Basins with agreement above average and high in over 50 % volume fraction ----
data_sum_distribution <- data_ma_distribution[agreement_fac %in% c('High', 'Above average'), .(evap_volume_fraction = sum(evap_volume_fraction)), .(ma_basin)]
data_sum_quartile <- data_ma_quartile[rel_dataset_agreement %in% c('High', 'Above average'), .(evap_volume_fraction = sum(evap_volume_fraction)), .(ma_basin)]

basins_distribution <- data_sum_distribution[evap_volume_fraction >= 0.5, ma_basin]
basins_quartile <- data_sum_quartile[evap_volume_fraction >= 0.5, ma_basin]

data_merged[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
data_merged[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote sensing"]
data_merged[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
data_merged[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]
data_merged[dataset %in% 'Ma', dataset_type := "Water balance"]

data_merged[, ma_basin := as.numeric(as.factor(ma_basin))]

data_merged[ma_basin < 21, continent := 'North America']
data_merged[ma_basin > 20 & ma_basin < 27, continent := 'South America']
data_merged[ma_basin > 27 & ma_basin < 33, continent := 'Europe']
data_merged[ma_basin > 33 & ma_basin < 47, continent := 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent := 'Africa']
data_merged[ma_basin > 51, continent := 'Oceania']

common_basins <- basins_distribution[basins_distribution %in% basins_quartile]

## basins ----
# World and Land borders -----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
world_no_antarctica <- world_sf[world_sf$continent != "Antarctica", ]


## Labels ----
labs_y <- data.frame(
  lon = -160,
  lat = c( 30, 0, -30, -60)
)

labs_y_labels <- seq(30, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -62)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

## basin ----

fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])
ma_sf_terr <- ma_sf[ma_sf$BasinID %in% common_basins,]


fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])

ma_sf <- ma_sf %>%
  mutate(
    BasinID = as.character(BasinID),
    basin_group = if_else(
      BasinID %in% as.character(common_basins),
      "High agreement basins",
      "Other basins"
    ),
    basin_group = factor(
      basin_group,
      levels = c("Other basins", "High agreement basins")
    )
  )

ma_sf_terr <- ma_sf[ma_sf$BasinID %in% common_basins,]

# plot basin ----
## theme ----
theme_map_fig5 <- theme_bw() +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    axis.ticks.length = unit(0, "cm"),
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.2),
    axis.text = element_blank(),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
  )

## gg----
fig_basins <- ggplot() +
  geom_sf(data = world_no_antarctica, fill = "light gray", color = "light gray") +
  geom_sf(data = ma_sf, fill = "white", color = "gray35", alpha = 1) +
  geom_sf(data = ma_sf_terr, fill = "royalblue4", color = "black", alpha = 0.7) +
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
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_fig5

# calculate means and compare to water balance ----

data_merged_mean <- data_merged[ma_basin %in% common_basins, .(mean_evap = mean(evap_mean), dataset_type = first(dataset_type), continent = first(continent)), .(dataset, ma_basin)]
data_merged_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_merged_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

data_merged_mean[, ratio_ET := mean_evap/water_balance]
data_merged_mean[ratio_ET < 1, diff := 'under']
data_merged_mean[ratio_ET > 1, diff := 'over']
data_merged_mean <- data_merged_mean[!is.na(diff)]

# panel b ----
## Reorder data ----
### Group order definitions ----

continent_order <- c(
  "Africa",
  "Asia",
  "Europe",
  "North America",
  "South America",
  "Oceania"
)

continent_order <- c(
  continent_order,
  setdiff(
    sort(unique(as.character(data_merged_mean$continent))),
    continent_order
  )
)

dataset_type_order <- unique(as.character(data_merged_mean$dataset_type))

label_space <- 3.2

### Basin order ----

basin_order <- data_merged_mean %>%
  distinct(ma_basin, continent) %>%
  mutate(
    ma_basin = as.character(ma_basin),
    continent = as.character(continent),
    continent = factor(continent, levels = continent_order),
    basin_num = suppressWarnings(as.numeric(ma_basin))
  ) %>%
  arrange(continent, basin_num, ma_basin) %>%
  mutate(
    x_id = row_number(),
    x_plot = x_id + label_space
  )

### Dataset order ----

dataset_order <- data_merged_mean %>%
  distinct(dataset, dataset_type) %>%
  mutate(
    dataset = as.character(dataset),
    dataset_type = as.character(dataset_type),
    dataset_type = factor(dataset_type, levels = dataset_type_order)
  ) %>%
  arrange(dataset_type, dataset) %>%
  mutate(
    y_id = rev(row_number())
  )

### Plot dimensions ----

n_basins <- nrow(basin_order)
n_datasets <- nrow(dataset_order)

### Continent blocks ----

continent_blocks <- basin_order %>%
  group_by(continent) %>%
  summarise(
    xmin = min(x_plot),
    xmax = max(x_plot),
    x_mid = mean(range(x_plot)),
    .groups = "drop"
  )

continent_separators <- continent_blocks %>%
  filter(xmax < max(basin_order$x_plot)) %>%
  mutate(
    x_sep = xmax + 0.5
  )

### Dataset type blocks ----

dataset_type_blocks <- dataset_order %>%
  group_by(dataset_type) %>%
  summarise(
    ymin = min(y_id),
    ymax = max(y_id),
    y_mid = mean(range(y_id)),
    .groups = "drop"
  )

dataset_type_separators <- dataset_type_blocks %>%
  filter(ymin > 1) %>%
  mutate(
    y_sep = ymin - 0.5
  )

### Manual label positions ----

dataset_type_labels <- dataset_type_blocks %>%
  mutate(
    x_label = 1.05
  )

dataset_name_labels <- dataset_order %>%
  mutate(
    x_label = label_space + 0.25
  )

## merge data ----

fig5b_data <- data_merged_mean %>%
  mutate(
    ma_basin = as.character(ma_basin),
    dataset = as.character(dataset)
  ) %>%
  left_join(
    basin_order %>%
      select(ma_basin, x_id, x_plot),
    by = "ma_basin"
  ) %>%
  left_join(
    dataset_order %>%
      select(dataset, y_id),
    by = "dataset"
  )

## Figure 5b heatmap ----

fig5b <- ggplot(
  fig5b_data,
  aes(x = x_plot, y = y_id, fill = ratio_ET)
) +
  geom_tile(
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = number(ratio_ET, accuracy = 0.01)),
    size = 3
  ) +
  geom_vline(
    data = continent_separators,
    aes(xintercept = x_sep),
    inherit.aes = FALSE,
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_hline(
    data = dataset_type_separators,
    aes(yintercept = y_sep),
    inherit.aes = FALSE,
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_text(
    data = continent_blocks,
    aes(
      x = x_mid,
      y = n_datasets + 0.85,
      label = continent
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.5
  ) +
  geom_text(
    data = dataset_type_labels,
    aes(
      x = 2.0 ,
      y = y_mid,
      label = dataset_type
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.5,
    hjust = 1
  ) +
  geom_text(
    data = dataset_name_labels,
    aes(
      x = x_label,
      y = y_id,
      label = dataset
    ),
    inherit.aes = FALSE,
    size = 3.2,
    hjust = 1
  ) +
  scale_fill_gradient2(
    low = "#A63A3A",
    mid = "white",
    high = "#4D648D",
    midpoint = 1,
    limits = c(0.5, 1.5),
    oob = squish,
    name = expression(frac(Mean~ET[WB], Mean~ET[Product]))
  ) +
  scale_x_continuous(
    breaks = basin_order$x_plot,
    labels = basin_order$ma_basin,
    limits = c(0.2, max(basin_order$x_plot) + 0.6),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = dataset_order$y_id,
    labels = NULL,
    limits = c(0.5, n_datasets + 1.15),
    expand = c(0, 0)
  ) +
  labs(
    x = "Basin",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(25, 10, 10, 15)
  )

fig5b


