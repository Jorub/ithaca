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
agreement_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "high_agreement_ma_basins_area.rds"))

# Basins with joint agreement above average and high in over 40 % area fraction ----
basins_higher_area <- agreement_summary[joint_da_higher > 0.4]

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

common_basins <- basins_higher_area$ma_basin 

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
fig_map_5a <- ggplot() +
  geom_sf(data = world_no_antarctica, fill = "light gray", color = "light gray") +
  geom_sf(data = ma_sf, fill = "white", color = "gray35", alpha = 1) +
  geom_sf(data = ma_sf_terr, fill = "grey15", color = "black", alpha = 0.7) +
  geom_label_repel(
    data = ma_sf_terr,
    aes(label = BasinID, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,  # always draw the line
    box.padding = 0.5,
    point.padding = 0.1,
    size = 4,
    segment.color = "black",
    nudge_x = 0.25,          # adjust nudging as needed
    nudge_y = 0.25
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

# Figure 5 shared visual style ------------------------------------------------
## theme and color ----
fig5_col_low  <- "#D55E00"   # product ET below WB
fig5_col_mid  <- "#F7F7F7"   # close to WB
fig5_col_high <- "#0072B2"   # product ET above WB
fig5_col_neut <- "grey65"
fig5_col_dark <- "grey25"
          
color_ref <- c(
  "Central product range below WB"   = fig5_col_low,
  "Central product range crosses WB" = fig5_col_neut,
  "Central product range above WB"   = fig5_col_high
)
        
        theme_fig5 <- theme_bw(base_size = 11) +
          theme(
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "white", colour = NA),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 9, colour = fig5_col_dark),
            axis.ticks = element_line(colour = "grey45", linewidth = 0.25),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 9),
            strip.background = element_rect(fill = "grey92", colour = NA),
            strip.text = element_text(size = 9, face = "bold", colour = fig5_col_dark),
            plot.margin = margin(5, 5, 5, 5)
          )

# Figure 5b heatmap ----
fig5b <- ggplot(
  fig5b_data,
  aes(x = x_plot, y = y_id, fill = ratio_ET)
) +
  geom_tile(
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      label = number(ratio_ET, accuracy = 0.01),
      colour = abs(ratio_ET - 1) > 0.25
    ),
    size = 2.8
  ) +
  scale_colour_manual(
    values = c(`FALSE` = "grey20", `TRUE` = "white"),
    guide = "none"
  ) +
  geom_vline(
    data = continent_separators,
    aes(xintercept = x_sep),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = "grey20"
  ) +
  geom_hline(
    data = dataset_type_separators,
    aes(yintercept = y_sep),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = "grey20"
  ) +
  geom_text(
    data = continent_blocks,
    aes(
      x = x_mid,
      y = n_datasets + 0.82,
      label = continent
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.2,
    colour = fig5_col_dark
  ) +
  geom_text(
    data = dataset_type_labels,
    aes(
      x = 2.0,
      y = y_mid,
      label = dataset_type
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.2,
    hjust = 1,
    colour = fig5_col_dark
  ) +
  geom_text(
    data = dataset_name_labels,
    aes(
      x = x_label,
      y = y_id,
      label = dataset
    ),
    inherit.aes = FALSE,
    size = 3.0,
    hjust = 1,
    colour = fig5_col_dark
  ) +
  scale_fill_gradient2(
    low = fig5_col_low,
    mid = fig5_col_mid,
    high = fig5_col_high,
    midpoint = 1,
    limits = c(0.5, 1.5),
    oob = squish,
    name = "Product ET / WB ET"
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
  theme_fig5 +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(16, 8, 8, 18)
  )

fig5b

# Figure 5c error vs joint agreement ----
data_merged[, ma_basin := factor(ma_basin)]

## Product mean ET per basin ----
data_mean <- data_merged[, .(
  product_et   = mean(evap_mean, na.rm = TRUE),
  dataset_type = first(dataset_type),
  continent    = first(continent)
), by = .(dataset, ma_basin)]

## Separate Ma as independent water-balance reference ----
wb <- data_mean[dataset == "Ma", .(
  wb_et = product_et
), by = ma_basin]

data_prod <- merge(
  data_mean[dataset != "Ma"],
  wb,
  by = "ma_basin",
  all.x = TRUE
)

data_prod[, ratio_ET := product_et / wb_et]

## Add agreement information ----
plot_dt <- merge(
  data_prod,
  agreement_summary,
  by = "ma_basin",
  all.x = TRUE
)

plot_dt <- plot_dt[!is.na(ratio_ET) & !is.na(joint_da_higher)]

# If joint_da_higher is stored as 0-1, convert to percent.
x_scale <- if (max(plot_dt$joint_da_higher, na.rm = TRUE) <= 1) 100 else 1
plot_dt[, joint_high_pct := joint_da_higher * x_scale]

## Basin-level summaries ----
basin_sum <- plot_dt[, .(
  ratio_min = min(ratio_ET, na.rm = TRUE),
  ratio_q25 = quantile(ratio_ET, 0.25, na.rm = TRUE),
  ratio_med = median(ratio_ET, na.rm = TRUE),
  ratio_q75 = quantile(ratio_ET, 0.75, na.rm = TRUE),
  ratio_max = max(ratio_ET, na.rm = TRUE),
  n_products = .N
), by = .(ma_basin, joint_high_pct)]

basin_sum[, wb_relation := fifelse(
  ratio_q75 < 1, "Central product range below WB",
  fifelse(ratio_q25 > 1, "Central product range above WB",
          "Central product range crosses WB")
)]

## colors ----

label_dt <- basin_sum[
  joint_high_pct >= quantile(joint_high_pct, 0.75, na.rm = TRUE) &
    (ratio_q75 < 1 | ratio_q25 > 1)
]

## ggplot ----
fig_5c <- ggplot() +
  geom_point(
    data = plot_dt,
    aes(x = joint_high_pct, y = ratio_ET),
    color = "grey70",
    alpha = 0.35,
    size = 1.0
  ) +
  geom_linerange(
    data = basin_sum,
    aes(x = joint_high_pct, ymin = ratio_min, ymax = ratio_max),
    color = "grey70",
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_linerange(
    data = basin_sum,
    aes(
      x = joint_high_pct,
      ymin = ratio_q25,
      ymax = ratio_q75,
      color = wb_relation
    ),
    linewidth = 1.15
  ) +
  geom_point(
    data = basin_sum,
    aes(x = joint_high_pct, y = ratio_med, fill = wb_relation),
    shape = 21,
    color = "grey15",
    size = 2.3,
    stroke = 0.25
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey15",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 40,
    linetype = "dotted",
    color = "grey45",
    linewidth = 0.4
  ) +
  geom_text(
    data = label_dt,
    aes(x = joint_high_pct, y = ratio_med, label = ma_basin),
    nudge_y = -0.045,
    size = 3.2,
    colour = fig5_col_dark,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = color_ref, name = NULL) +
  scale_fill_manual(values = color_ref, name = NULL) +
  labs(
    x = "Basin fraction with joint high or above-average agreement (%)",
    y = "Product ET / WB ET"
  ) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  theme_fig5 +
  theme(
    legend.position = "right",
    plot.margin = margin(8, 8, 10, 18)
  )

# Figure 5 join plot ----
fig5 <- ggarrange(
  fig_map_5a,
  fig5b,
  fig_5c,
  nrow = 3,
  labels = c("a", "b", "c"),
  font.label = list(size = 12, face = "bold"),
  heights = c(0.85, 1.35, 1.0)
)

fig5
# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.png"
  ),
  plot = fig5,
  width = 20,
  height = 1.5*20,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.pdf"
  ),
  plot = fig5,
  width = 20,
  height = 1.5*20,
  units = "cm",
  dpi = 300
)

