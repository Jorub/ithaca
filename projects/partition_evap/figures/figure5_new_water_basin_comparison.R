# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(rnaturalearth)
library(dplyr)

## data ----
fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])

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

qs <- quantile(
  agreement_summary[ma_basin != "Mean", joint_da_higher],
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)

qs_percent <- ceiling(qs*100)
qs_percent[4] <- qs_percent[4]-1

# Analysis ----
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
data_merged[ma_basin > 26 & ma_basin < 33, continent := 'Europe']
data_merged[ma_basin > 32 & ma_basin < 47, continent := 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent := 'Africa']
data_merged[ma_basin > 51, continent := 'Oceania']

data_merged[ma_basin < 21, continent_abr := 'NAm']
data_merged[ma_basin > 20 & ma_basin < 27, continent_abr := 'SAm']
data_merged[ma_basin > 26 & ma_basin < 33, continent_abr := 'Eur']
data_merged[ma_basin > 32 & ma_basin < 47, continent_abr:= 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent_abr := 'Afr']
data_merged[ma_basin > 51, continent_abr := 'Oce']

data_merged[, ma_basin := as.factor(ma_basin)]

## calculate means and compare to water balance ----

data_mean <- data_merged[,.(mean_evap = mean(evap_mean), 
                            dataset_type = first(dataset_type), 
                            continent = first(continent),
                            continent_abr = first(continent_abr)), 
                         .(dataset, ma_basin)]

data_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

data_stats <- data_mean[, .(
  Q25_evap = quantile(mean_evap, 0.25),
  Q75_evap = quantile(mean_evap, 0.75),
  ens_mean = mean(mean_evap),
  continent = first(continent)),
  .(ma_basin)]

data_stats[, IQR := Q75_evap - Q25_evap, .(ma_basin)]
data_stats[, sIQR := IQR/ens_mean]
data_stats <- merge(data_stats, agreement_summary, by = "ma_basin")

not_basin <- c(20, 12, 19, 13, 34, 43, 26, 27, 53, 54, 40,
               4)

summary(lm(sIQR ~ rel_da_higher, data = data_stats[!(ma_basin %in% not_basin)]))

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


## prepare penal a map data ----
fill_min <- min(plot_dt$joint_high_pct, na.rm = TRUE)
fill_max <- max(plot_dt$joint_high_pct, na.rm = TRUE)

basin_sum <- plot_dt[, .(
  ratio_min = min(ratio_ET, na.rm = TRUE),
  ratio_q25 = quantile(ratio_ET, 0.25, na.rm = TRUE),
  ratio_med = median(ratio_ET, na.rm = TRUE),
  ratio_mean = mean(ratio_ET, na.rm = TRUE),
  ratio_q75 = quantile(ratio_ET, 0.75, na.rm = TRUE),
  ratio_max = max(ratio_ET, na.rm = TRUE),
  ratio_q25_log = exp(quantile(log(ratio_ET), 0.25, na.rm = TRUE)),
  ratio_mean_log = exp(mean(log(ratio_ET), na.rm = TRUE)),
  ratio_q75_log = exp(quantile(log(ratio_ET), 0.75, na.rm = TRUE)),
  joint_high_pct = first(joint_high_pct)
), by = .(ma_basin)]

basin_sum[, ratio_et := fifelse(
  ratio_q75 < 1, "Below WB",
  fifelse(ratio_q25 > 1, "Above WB",
          "Crosses WB")
)]

ma_sf <- ma_sf %>%
  mutate(BasinID = as.character(BasinID)) %>%
  left_join(
    basin_sum,
    by = c("BasinID" = "ma_basin")
  ) %>%
  mutate(
    ratio_et = factor(
      ratio_et,
      levels = names(color_ref)
    )
  )

ma_sf_dt <- data.table(st_drop_geometry(ma_sf))


## prepare panel b ----

## prepare panel c ----

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


## prepare panel d ----
### heatplot data ----
plot_dt_heat <- merge(
  data_mean,
  basin_sum,
  by = "ma_basin",
  all.x = TRUE
)

basin_order <- plot_dt_heat[, .(
  joint_high_pct = first(joint_high_pct)
), by = ma_basin][order(joint_high_pct, ma_basin)]

plot_dt_heat[, ma_basin_ord := factor(
  ma_basin,
  levels = basin_order$ma_basin, order = T
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
    ratio_ET = exp(mean(log(ratio_ET), na.rm = TRUE)),
    joint_high_pct = first(joint_high_pct)
  ),
  by = .(ma_basin, ma_basin_ord)
]

ensemble_basin_ratio[
  ,
  `:=`(
    dataset = "mean",
    dataset_type = "Ensemble",
    group = "Basin"
  )
]


### Ensemble-min row for each basin ----

plot_dt_melt <- melt(plot_dt_heat[,.(ma_basin, dataset,
                                     dataset_type, continent_abr,
                                     ratio_ET, ratio_min, ratio_q25,
                                     ratio_mean, ratio_q75, ratio_max,
                                     joint_high_pct, ratio_et, ma_basin_ord,
                                     group)], 
                     value.name = "ratio_ET", 
                     measure.vars = c("ratio_ET", "ratio_min", "ratio_q25", 
                                 "ratio_mean", "ratio_q75", "ratio_max"))

plot_dt_melt[variable != "ratio_ET", dataset_type := "Ensemble"]

plot_dt_melt[variable == "ratio_min", dataset := "minimum"]
plot_dt_melt[variable == "ratio_max", dataset := "maximum"]
plot_dt_melt[variable == "ratio_q25", dataset := "Q25"]
plot_dt_melt[variable == "ratio_q75", dataset := "Q75"]
plot_dt_melt[variable == "ratio_mean", dataset := "mean"]

plot_dt_melt <- unique(plot_dt_melt)

not_dataset <- c("minimum", "maximum", "Q25", "Q75", "mean")

plot_dt_melt[!(dataset %in% not_dataset) & dataset_type == "Ensemble", dataset := "mean"]
plot_dt_melt <- unique(plot_dt_melt)

ens_stat_means <- plot_dt_melt[(dataset %in% not_dataset),
             .(ratio_ET = exp(mean(log(ratio_ET)))), .(dataset)]


ens_stat_means[
  ,
  `:=`(
    dataset_type = "Ensemble",
    group = "Mean",
    ma_basin = "Mean",
    ma_basin_ord = "Mean"
  )
]

plot_dt_heat_merge <- merge(plot_dt_heat, dataset_ratio_summary, 
                            by = c("dataset", "ratio_ET", "group", 
                                   "dataset_type", "ma_basin", "ma_basin_ord"),
                            all = T)

plot_dt_heat_merge_summary <- rbindlist(
  list(
    plot_dt_melt,
    dataset_ratio_summary,
    ens_stat_means
    
  ),
  fill = TRUE
)

plot_dt_heat_merge_summary <- unique(plot_dt_heat_merge_summary)
plot_dt_heat_merge_summary[, log2_ratio := log2(ratio_ET)]

ma_sf_dt[, BasinID_fac := as.factor(BasinID)]

### basin label ----
basin_label_dt <- merge(plot_dt_heat_merge_summary[,.(continent_abr, ma_basin, group)], 
                        ma_sf_dt[,.(BasinID_fac, Name)], 
                        by.x = "ma_basin", by.y = "BasinID_fac", all.x = T)

basin_label_filt <- unique(basin_label_dt)

basin_label_filt[, basin_label := paste(
  continent_abr,
  Name,
  ma_basin,
  sep = " - "
)]

basin_label_filt[ma_basin == "Mean", `:=`(
  basin_label = "Mean",
  Name = "Mean",
  continent_abr = "Mean"
)]

basin_label_filt <- basin_label_filt[complete.cases(basin_label_filt),]

basin_label_filt[, ma_basin_ord := c(as.numeric(basin_order$ma_basin), 57)]

### Order product rows and put ensemble mean at top ----

dataset_level_summary <- c(dataset_level,"minimum","Q25","mean","Q75","maximum")

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

# figure 5 b ----
### label ----

##
plot_dt[, joint_high_pct_fac := cut(joint_high_pct, 
                                    breaks = c(20, 40, 60, 100))]

qs_dt <- data.table(qs_percent)
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
    data = qs_dt[2:4,],
    aes(x = qs_percent, y = 2.45),
    label = c("Q25", "Q50", "Q75"),
    color = "grey45"
  ) +
  scale_color_manual(values = color_ref) +
  scale_fill_manual(values = color_ref) +
  labs(
    x = "Area fraction of high or above-average agreement overlap [%]",
    y = expression(ET[Product] / ET[WB]),
    color = "Central ensemble range",
    fill = "Central ensemble range"
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


# Figure 5 d ----

basin_axis_labels <- setNames(
  basin_label_filt$basin_label,
  as.character(basin_label_filt$ma_basin)
)

fig_5d <- ggplot() +
  geom_tile(data = plot_dt_heat_merge_summary,
            aes(x = ma_basin_ord, y = dataset, fill = log2_ratio),
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
    midpoint = 0,
    limits = c(-1, 1),
    breaks = log2(c(0.5, 0.75, 1, 1.5, 2)),
    labels = c("0.5", "0.75", "1", "1.5", "2"),
    name = expression(ET[product] / ET[WB]),
    oob = scales::squish
  ) +
  
  labs(
    x = "Basin",
    y = NULL
  ) +
geom_vline(xintercept = c(15.5, 28.5, 42.5),
           linetype = "dotted")+
  scale_x_discrete(labels = basin_axis_labels)+
  theme_fig5 +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 9
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

fig_5d

# map data ----


# Figure 5 c ----
fig_5c <- ggplot(
  basin_count,
  aes(x = n, y = ratio_et_label, fill = ratio_et)
) +
  geom_col(width = 0.25) +
  geom_text(
    aes(label = n),
    hjust = -0.4,
    size = 3
  ) +
  scale_fill_manual(
    values = color_ref,
    name = "Central ensemble range",
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
    title = "Central\nensemble range"
  ) +
  theme_fig5 +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(4, 3, 4, 2)
  )

# Figure 5 a plot basin ----

fig_map_5a <- 
  ggplot() +
  geom_sf(data = world_no_antarctica, fill = "light gray", color = "light gray") +
  geom_sf(data = ma_sf, aes(fill = joint_high_pct) ,color = "gray35", alpha = 1) +
  geom_label_repel(
    data = ma_sf,
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
  scale_fill_gradientn(
    colours = c("#EEF1F6", "#4D648D", "#5E9C76"),
    values = rescale(c(fill_min, qs_percent[4], fill_max)),
    limits = c(fill_min, fill_max),
    name = "Agreement \noverlap [%]",
  )+
  labs(x = NULL, y = NULL, fill = "Central product range") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_fig5+theme(legend.position = "right")


# join all ----

fig_5_top_right <- ggarrange(fig_5b, fig_5c,
                       nrow = 1, widths = c(1, 0.5),
                       labels = c('b', 'c', ''), align = "h")
fig_5_top_right <- ggarrange(fig_5_top_right, "",
                             widths = c(1, 0.01), nrow = 1)
fig_5_top <- ggarrange(fig_map_5a, fig_5_top_right,
                     nrow = 1, widths = c(1.5, 1.5),
                     labels = c('a', '', ''))


fig_5 <- ggarrange(fig_5_top,
                   fig_5d,
                   nrow = 2,
                   heights = c(0.925, 1.8),
                   labels = c('', 'd'))

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


# statistical tests ----

library(lme4)
library(lmerTest)
plot_dt[, joint_high_pct_10 := joint_high_pct/10]

## absolute difference ----
m_dev_model <- lmer(abs(log2(ratio_ET)) ~ joint_high_pct_10 + dataset_type + (1 | dataset) +(1 | ma_basin),
                    data = plot_dt, REML = FALSE)
summary(m_dev_model)

m_dev_model <- lmer(abs(log2(ratio_ET)) ~ joint_high_pct_10 + dataset_type + continent + (1 | dataset) +(1 | ma_basin),
                    data = plot_dt, REML = FALSE)
summary(m_dev_model)

## absolute difference ----

basin_sum[, joint_high_pct_10 := joint_high_pct/10]
lm_model <- lm(abs(log2(ratio_med)) ~ joint_high_pct_10, data = basin_sum)
summary(lm_model)

lm_model <- lm(log2(ratio_q75)-log2(ratio_q25) ~ joint_high_pct_10, data = basin_sum)
summary(lm_model)

plot_dt_sel <- plot_dt[, .(
  ratio_Q25 = quantile(ratio_ET, 0.25),
  ratio_Q75 = quantile(ratio_ET, 0.75),
  rel_da_higher = first(rel_da_higher)),
  .(ma_basin)
  ]

model_test <- lm(log2(ratio_Q75/ratio_Q25)~rel_da_higher, data = plot_dt_sel)
summary(model_test)
