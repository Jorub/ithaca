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
data[, environment_volume := NULL]
data[, area_sum := NULL]
data <- data[year < 2017]
data_merged <- merge(data, data_ma_time_long, by.x = c('year', 'ma_basin', 'evap_mean', 'dataset'), by.y = c('Year', 'ma_basin', 'evap', 'dataset'), all = T)

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basins_merged.rds"))

agreement_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "high_agreement_ma_basins_area.rds"))

# Analysis ----
## Basins with joint agreement above average and high in over 40 % area fraction ----
basins_higher_area <- agreement_summary[joint_da_higher > 0.4]
common_basins <- basins_higher_area$ma_basin 

### assign data type
data_merged[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote sensing"]
data_merged[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
data_merged[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
data_merged[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]
data_merged[dataset %in% 'Ma', dataset_type := "Water balance"]

data_merged[, ma_basin := as.numeric(as.factor(ma_basin))]

### assign continent

data_merged[ma_basin < 21, continent := 'North America']
data_merged[ma_basin > 20 & ma_basin < 27, continent := 'S. America']
data_merged[ma_basin > 27 & ma_basin < 33, continent := 'Europe']
data_merged[ma_basin > 33 & ma_basin < 47, continent := 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent := 'Africa']
data_merged[ma_basin > 51, continent := 'Oceania']

data_merged[, ma_basin := as.factor(ma_basin)]

## calculate means and compare to water balance ----
data_merged_mean <- data_merged[ma_basin %in% common_basins, 
                                .(mean_evap = mean(evap_mean), dataset_type = first(dataset_type), continent = first(continent)), 
                                .(dataset, ma_basin)]
data_merged_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_merged_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

data_merged_mean[, ratio_ET := mean_evap/water_balance]
data_merged_mean[ratio_ET < 1, diff := 'under']
data_merged_mean[ratio_ET > 1, diff := 'over']
data_merged_mean <- data_merged_mean[!is.na(diff)]

data_mean <- data_merged[,.(mean_evap = mean(evap_mean), dataset_type = first(dataset_type), continent = first(continent)), 
                          .(dataset, ma_basin)]
data_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

data_mean[, ratio_ET := mean_evap/water_balance]
data_mean[ratio_ET < 1, diff := 'under']
data_mean[ratio_ET > 1, diff := 'over']
data_mean <- data_mean[!is.na(diff)]

plot_dt <- merge(
  data_mean,
  agreement_summary,
  by = "ma_basin",
  all.x = TRUE
)

x_scale <- if (max(plot_dt$joint_da_higher, na.rm = TRUE) <= 1) 100 else 1
plot_dt[, joint_high_pct := joint_da_higher * x_scale]

## Basin-level summaries ----
basin_sum <- plot_dt[, .(
  ratio_min = min(ratio_ET, na.rm = TRUE),
  ratio_q25 = quantile(ratio_ET, 0.25, na.rm = TRUE),
  ratio_med = median(ratio_ET, na.rm = TRUE),
  ratio_q75 = quantile(ratio_ET, 0.75, na.rm = TRUE),
  ratio_max = max(ratio_ET, na.rm = TRUE),
  n_products = .N,
  joint_high_pct = first(joint_high_pct)
), by = .(ma_basin)]

basin_sum[, ratio_et := fifelse(
  ratio_q75 < 1, "Below WB",
  fifelse(ratio_q25 > 1, "Above WB",
          "Crosses WB")
)]

# Panel b and c ----
## color ----
fig5_col_low  <- "#D55E00"   # product ET below WB
fig5_col_mid  <- "#F7F7F7"   # close to WB
fig5_col_high <- "#0072B2"   # product ET above WB
fig5_col_neut <- "grey65"
fig5_col_dark <- "grey25"
          
color_ref <- c(
          "Below WB"   = fig5_col_low,
          "Crosses WB" = fig5_col_neut,
          "Above WB"   = fig5_col_high
)
## theme ----
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
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "bottom"
  )

## figure 5 b ----
fig_5b <- ggplot(
  data_merged_mean,
  aes(x = ma_basin, y = dataset, fill = ratio_ET)
) +
  geom_tile(
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      label = number(ratio_ET, accuracy = 0.01),
      colour = abs(ratio_ET - 1) > 0.28
    ),
    size = 2.55
  ) +
  scale_colour_manual(
    values = c(`FALSE` = "grey20", `TRUE` = "white"),
    guide = "none"
  ) +
  facet_grid(
    rows = vars(dataset_type),
    cols = vars(continent),
    scales = "free",
    space = "free",
    switch = "y"
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
  labs(
    x = NULL,
    y = "Dataset",
    title = "High agreement basins"
  ) +
  theme_fig5 +
  theme(
    axis.text.x = element_text(size = 9),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 9),
    strip.placement = "outside",
    strip.text.x = element_text(
      size = 9,
      face = "bold",
      colour = fig5_col_dark
    ),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      size = 9,
      face = "bold",
      colour = fig5_col_dark
    ),
    panel.spacing.x = unit(0.08, "lines"),
    panel.spacing.y = unit(0.08, "lines"),
    plot.margin = margin(6, 6, 2, 16)
  )

fig_5b
## figure 5 c ----

### label ----
label_dt <- basin_sum[
  joint_high_pct >= 40 &
    (ratio_q75 < 1 | ratio_q25 > 1)
]

### ggplot ----
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
      color = ratio_et
    ),
    linewidth = 1.15
  ) +
  geom_point(
    data = basin_sum,
    aes(x = joint_high_pct, y = ratio_med, fill = ratio_et),
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
    aes(x = joint_high_pct, y = ratio_q25, label = ma_basin),
    nudge_y = -0.1,
    size = 3,
    colour = fig5_col_dark,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = color_ref) +
  scale_fill_manual(values = color_ref) +
  labs(
    x = "Basin area fraction of spatial overlap of high or above-average agreement (%)",
    y = "Product ET / WB ET",
    color = "Central product range",
    fill = "Central product range"
  ) +
  coord_cartesian(ylim = c(0.3, 2)) +
  theme_fig5 +
  theme(
    plot.margin = margin(8, 8, 10, 18)
  )

fig_5c

## ggarrange ----

fig_5bc <-
  ggarrange(fig_5b, fig_5c, nrow = 2, align = "v",
            labels = c('b', 'c'),
            heights = c(1, 0.7))

fig_5bc

# map data ----
## World and Land borders -----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
world_no_antarctica <- world_sf[world_sf$continent != "Antarctica", ]


### Labels ----
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

### basin ----

fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])
ma_sf_terr <- ma_sf %>%
  filter(BasinID %in% as.character(common_basins))

ma_sf <- ma_sf %>%
  mutate(BasinID = as.character(BasinID)) %>%
  left_join(
    basin_sum,
    by = c("BasinID" = "ma_basin")
  ) %>%
  mutate(
    basin_group = if_else(
      BasinID %in% as.character(common_basins),
      "High agreement basins",
      "Other basins"
    ),
    basin_group = factor(
      basin_group,
      levels = c("Other basins", "High agreement basins")
    ),
    ratio_et = factor(
      ratio_et,
      levels = names(color_ref)
    )
  )



# figure 5 a plot basin ----
## themes ----
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
fig_map_5a <- 
  ggplot() +
  geom_sf(data = world_no_antarctica, fill = "light gray", color = "light gray") +
  geom_sf(data = ma_sf, aes(fill = ratio_et) ,color = "gray35", alpha = 1) +
  geom_sf(data = ma_sf_terr, fill = "transparent", color = "black", lwd = 0.3) +
  geom_label_repel(
    data = ma_sf_terr,
    aes(label = BasinID, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,  # always draw the line
    box.padding = 0.5,
    point.padding = 0.1,
    size = 2,
    segment.color = "black",
    nudge_x = 0.2,          # adjust nudging as needed
    nudge_y = 0.2,
    max.overlaps = Inf
  ) +
  scale_fill_manual(values = color_ref) +
  labs(x = NULL, y = NULL, fill = "Central product range") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_fig5+theme(legend.position = "bottom")

# join all ----

fig_5 <- ggarrange(fig_map_5a,
                   fig_5bc,
                   nrow = 2,
                   heights = c(0.7, 1.3),
                   labels = c('a', ''))


# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.png"
  ),
  plot = fig_5,
  width = 20,
  height = 1.4*20,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.pdf"
  ),
  plot = fig_5,
  width = 20,
  height = 1.3*20,
  units = "cm",
  dpi = 300
)

