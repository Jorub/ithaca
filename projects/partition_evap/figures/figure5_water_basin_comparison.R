# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(rnaturalearth)
library(dplyr)

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

qs <- quantile(
  agreement_summary[ma_basin != "Mean", joint_da_higher],
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)

qs_percent <- ceiling(qs*100)
qs_percent[4] <- qs_percent[4]-1

basins_higher_area <- agreement_summary[joint_da_higher > qs[4]]
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
  ratio_mean = mean(ratio_ET, na.rm = TRUE),
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

## heatplot data ----
plot_dt_heat <- merge(
  data_mean,
  agreement_summary,
  by = "ma_basin",
  all.x = TRUE
)

x_scale <- if (max(plot_dt_heat$joint_da_higher, na.rm = TRUE) <= 1) 100 else 1
plot_dt_heat[, joint_high_pct := joint_da_higher * x_scale]

basin_order <- plot_dt_heat[, .(
  joint_high_pct = first(joint_high_pct)
), by = ma_basin][order(joint_high_pct, ma_basin)]

plot_dt_heat[, ma_basin_ord := factor(
  ma_basin,
  levels = basin_order$ma_basin
)]

mean_dataset_ratio <- 
  plot_dt_heat[,.(mean_ratio = mean(ratio_ET)),
               .(dataset)]

dataset_ratio_summary <- 
  plot_dt_heat[,.(ratio_ET = exp(mean(log(ratio_ET)))),
               .(dataset)]

dataset_ratio_summary[, group := "Mean"]
dataset_ratio_summary[, ma_basin := "Mean"]
dataset_ratio_summary[, ma_basin_ord := "Mean"]
dataset_ratio_summary[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote sensing"]
dataset_ratio_summary[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
dataset_ratio_summary[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
dataset_ratio_summary[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]

dataset_order <- mean_dataset_ratio[,order(mean_ratio)]
dataset_level <- mean_dataset_ratio$dataset[dataset_order]

plot_dt_heat[, dataset := factor(dataset,
  levels = dataset_level
)]

plot_dt_heat[, group:= "Basin"]

### Ensemble-mean row for each basin ----
ensemble_basin_ratio <- plot_dt_heat[
  ,
  .(
    ratio_ET = mean(ratio_ET, na.rm = TRUE),
    joint_high_pct = first(joint_high_pct)
  ),
  by = .(ma_basin, ma_basin_ord)
]

ensemble_basin_ratio[
  ,
  `:=`(
    dataset = "ensemble mean",
    dataset_type = "Ensemble",
    group = "Basin"
  )
]

# Ensemble-mean value for the right-side "Mean" column
ensemble_mean_ratio <- ensemble_basin_ratio[
  ,
  .(
    ratio_ET = exp(mean(log(ratio_ET), na.rm = TRUE))
  )
]

ensemble_mean_ratio[
  ,
  `:=`(
    dataset = "ensemble mean",
    dataset_type = "Ensemble",
    group = "Mean",
    ma_basin = "Mean",
    ma_basin_ord = "Mean"
  )
]

plot_dt_heat_merge <- merge(plot_dt_heat, dataset_ratio_summary, 
                            by = c("dataset", "ratio_ET", "group", "dataset_type", "ma_basin", "ma_basin_ord"),
                      all = T)

plot_dt_heat_merge_summary <- rbindlist(
  list(
    plot_dt_heat,
    dataset_ratio_summary,
    ensemble_basin_ratio,
    ensemble_mean_ratio
  ),
  fill = TRUE
)

### Order product rows and put ensemble mean at top ----

dataset_level_summary <- c(dataset_level, "ensemble mean")

plot_dt_heat_merge_summary[
  ,
  dataset := factor(
    dataset,
    levels = dataset_level_summary
  )
]

plot_dt_heat_merge_summary[
  ,
  dataset_type := factor(
    dataset_type,
    levels = c(
      "Ensemble",
      "Composite",
      "Hydr./LSM model",
      "Reanalysis",
      "Remote sensing"
    )
  )
]

plot_dt_heat_merge_summary[
  ,
  group := factor(
    group,
    levels = c("Basin", "Mean")
  )
]


### agreement strip -----
agreement_strip <- plot_dt_heat[
  ,
  .(
    joint_high_pct = first(joint_high_pct)
  ),
  by = .(ma_basin, ma_basin_ord)
]

agreement_strip_mean <- agreement_strip[
  ,
  .(
    joint_high_pct = mean(joint_high_pct, na.rm = TRUE)
  )
]

agreement_strip_mean[
  ,
  `:=`(
    ma_basin = "Mean",
    ma_basin_ord = "Mean"
  )
]

agreement_strip <- rbindlist(
  list(agreement_strip, agreement_strip_mean),
  fill = TRUE
)

agreement_strip[,metric := "agreement overlap"]

agreement_strip[,dataset_type := "Ensemble"]


agreement_strip[
  ,
  dataset_type := factor(
    dataset_type,
    levels = c(
      "Ensemble",
      "Composite",
      "Hydr./LSM model",
      "Reanalysis",
      "Remote sensing"
    )
  )
]
agreement_strip[,dataset := "ensemble mean"]

agreement_strip[, group := "Basin"]

agreement_strip[ma_basin == "Mean", group := "Mean"]




agreement_strip[
  ,
  joint_high_q := cut(
    joint_high_pct,
    breaks = qs_percent,
    include.lowest = TRUE
  )
]

# figure 5 b ----
### label ----
label_dt <- basin_sum[
  joint_high_pct >= qs_percent[4] &
    (ratio_q75 < 1 | ratio_q25 > 1)
]

##
plot_dt[, joint_high_pct_fac := cut(joint_high_pct, 
                                    breaks = c(20, 40, 60, 100))]

### ggplot ----
fig_5b <- ggplot() +
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
    xintercept = qs_percent[2:4],
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
    x = "Basin area fraction of high or above-average agreement overlap (%)",
    y = expression(ET[Product] / ET[WB]),
    color = "Central product range",
    fill = "Central product range"
  ) +
  coord_cartesian(ylim = c(0.4, 2.5)) +
  scale_y_log10(
    breaks = c(0.5, 0.67, 1, 1.5, 2),
    labels = c("0.5", "0.67", "1", "1.5", "2")
  )+
  theme_fig5 +
  theme(
    plot.margin = margin(8, 8, 10, 18),
    legend.position = "none"
  )

fig_5b

# Figure 5 c alternate ----

fig_5c <- ggplot() +
  geom_tile(data = plot_dt_heat_merge_summary,
            aes(x = ma_basin_ord, y = dataset, fill = ratio_ET),
    color = "white",
    linewidth = 0.25
  ) +
  facet_grid(
    rows = vars(dataset_type), 
    cols = vars(group),
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
    name = expression(ET[Product] / ET[WB])
  ) +
  geom_vline(xintercept = c(15.5, 28.5, 42.5),
             linetype = "dotted")+
  labs(
    x = "Basin",
    y = NULL
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = agreement_strip,
    aes(x = ma_basin_ord, y = metric, fill = joint_high_q)
  ) +
  scale_fill_manual(
    values = colors_agreement_strip,
    name = "Agreement overlap [%]"
  ) +
  theme_fig5 +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 7
    ),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      size = 9,
      face = "bold",
      colour = fig5_col_dark
    ),
    panel.spacing.y = unit(0.08, "lines"),
    plot.margin = margin(6, 6, 2, 16)
  )

fig_5c

## ggarrange ----

fig_5bc <-
  ggarrange( fig_5b, fig_5c, nrow = 2,
            labels = c('b', 'c'),
            heights = c(0.6, 1))

fig_5bc

# map data ----
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

## Panel a count bar plot ----

basin_count <- ma_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(ratio_et)) %>%
  count(ratio_et, name = "n") %>%
  mutate(
    ratio_et = factor(ratio_et, levels = names(color_ref)),
    ratio_et_label = factor(
      as.character(ratio_et),
      levels = c("Below WB", "Crosses WB", "Above WB"),
      labels = c(
        "Below~ET[WB]",
        "Crosses~ET[WB]",
        "Above~ET[WB]"
      )
    )
  )

fig_map_5a_counts <- ggplot(
  basin_count,
  aes(x = n, y = ratio_et_label, fill = ratio_et)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3
  ) +
  scale_fill_manual(
    values = color_ref,
    name = "Central product range",
    drop = F
  )+
  scale_y_discrete(labels = function(x) parse(text = x)) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)),
    breaks = pretty_breaks(n = 3)
  ) +
  labs(
    x = "Basins [n]",
    y = NULL,
    title = "Central\nproduct range"
  ) +
  theme_fig5 +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(4, 2, 4, 2)
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
  scale_fill_manual(
    values = color_ref,
    breaks = c("Below WB", "Crosses WB", "Above WB"),
    labels = c(
      expression(paste("Below ", ET[WB])),
      expression(paste("Crosses ", ET[WB])),
      expression(paste("Above ", ET[WB]))
    ),
    name = "Central product range"
  )+
  labs(x = NULL, y = NULL, fill = "Central product range") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_fig5


# Panel c: merge map and inset ----
bar_joint_grob <- ggplotGrob(fig_map_5a_counts)

build <- ggplot_build(fig_map_5a)

x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width <- diff(x_range)
y_height <- diff(y_range)

fig_5a <- fig_map_5a +
  annotation_custom(
    grob = bar_joint_grob,
    xmin = x_range[2] + 0.02 * x_width,
    xmax = x_range[2] + 0.49 * x_width,
    ymin = y_range[1] + 0.0 * y_height,
    ymax = y_range[1] + 0.6 * y_height
  )



# join all ----

fig_5 <- ggarrange(fig_5a,
                   fig_5bc,
                   nrow = 2,
                   heights = c(0.925, 1.3),
                   labels = c('a', ''))

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.png"
  ),
  plot = fig_5,
  width = figure_widths,
  height = 1.4*20,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.pdf"
  ),
  plot = fig_5,
  width = figure_widths,
  height = 1.3*20,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

