# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')

ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basin_data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"))

## map ----

fig_map_5a <- 
  ggplot() +
  geom_sf(data = world_no_antarctica, fill = "light gray", color = "light gray") +
  geom_sf(data = ma_sf, aes(fill = mean_RE_basin), color = "gray35", alpha = 1) +
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
  ) + scale_fill_manual(
    values = map_wb_cols,
    name = "Central product range"
  )+
  labs(x = NULL, y = NULL, fill = "Central product range") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map_fig5+theme(legend.position = "right")

## mean error dependent on area fraction ----

fig_5b <- ggplot() +
  geom_point(
    data = basin_data,
    aes(x = joint_high_pct, y = mean_RE),
    color = "grey70",
    alpha = 0.35,
    size = 1.0
  ) +
  geom_linerange(
    data = ma_sf,
    aes(x = joint_high_pct, ymin = mean_RE_min, ymax = mean_RE_max),
    color = "grey70",
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_linerange(
    data = ma_sf,
    aes(
      x = joint_high_pct,
      ymin = mean_RE_q25,
      ymax = mean_RE_q75,
      color = mean_RE_basin
    ),
    linewidth = 1.15
  ) +
  geom_point(
    data = ma_sf,
    aes(x = joint_high_pct, y = mean_RE_med, fill = mean_RE_basin),
    shape = 21,
    color = "grey15",
    size = 2.3,
    stroke = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey15",
    linewidth = 0.4
  ) +
  scale_color_manual(values = map_wb_cols) +
  scale_fill_manual(values = map_wb_cols) +
  labs(
    x = "Area fraction of higher agreement [%]",
    y = expression((ET[Product] - ET[WB])/ET[WB]),
    color = "Central product range",
    fill = "Central product range"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_fig5 +
  theme(
    plot.margin = margin(8, 8, 10, 18),
    legend.position = "none"
  )

fig_5b

## mean error dependent on basin sIQR ----

fig_5c <- ggplot() +
  geom_point(
    data = basin_data,
    aes(x = sIQR, y = mean_RE),
    color = "grey70",
    alpha = 0.35,
    size = 1.0
  ) +
  geom_linerange(
    data = ma_sf,
    aes(x = sIQR, ymin = mean_RE_min, ymax = mean_RE_max),
    color = "grey70",
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_linerange(
    data = ma_sf,
    aes(
      x = sIQR,
      ymin = mean_RE_q25,
      ymax = mean_RE_q75,
      color = mean_RE_basin
    ),
    linewidth = 1.15
  ) +
  geom_point(
    data = ma_sf,
    aes(x = sIQR, y = mean_RE_med, fill = mean_RE_basin),
    shape = 21,
    color = "grey15",
    size = 2.3,
    stroke = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey15",
    linewidth = 0.4
  ) +
  scale_color_manual(values = map_wb_cols) +
  scale_fill_manual(values = map_wb_cols) +
  labs(
    x = "Basin sIQR [-]",
    y = expression((ET[Product] - ET[WB])/ET[WB]),
    color = "Central product range",
    fill = "Central product range"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_fig5 +
  theme(
    plot.margin = margin(8, 8, 10, 18),
    legend.position = "none"
  )

fig_5c

fig_5_b_c <- ggarrange(fig_5b, fig_5c, nrow = 2, labels = c("b", "c"), legend = "none")

fig_5_a_b_c <- ggarrange(fig_map_5a, fig_5_b_c, ncol = 2, 
                         common.legend = T, labels = c("a", ""), widths = c(1.5,1),
                         align = "v")

## heatmap ----

basin_order <- basin_data[, .(
  mean_RE_mean = mean(mean_RE)
), by = ma_basin][order(mean_RE_mean, ma_basin)]

basin_data[, ma_basin_ord := factor(
  ma_basin,
  levels = basin_order$ma_basin, order = T
)]

ma_sf_dt <- data.table(st_drop_geometry(ma_sf))

basin_label_dt <- merge(basin_data[,.(continent_abr, ma_basin)], 
                        ma_sf_dt[,.(BasinID, Name)], 
                        by.x = "ma_basin", by.y = "BasinID", all.x = T)

basin_label_filt <- unique(basin_label_dt)

basin_label_filt[, basin_label := paste(
  continent_abr,
  Name,
  ma_basin,
  sep = " - "
)]

basin_axis_labels <- setNames(
  basin_label_filt$basin_label,
  as.character(basin_label_filt$ma_basin)
)

fig_heat <- ggplot() +
  geom_tile(data = basin_data,
            aes(x = ma_basin_ord, y = dataset, fill = mean_RE),
            color = "white",
            linewidth = 0.25
  ) +
  facet_grid(
    rows = vars(dataset_type), 
    scales = "free",
    space = "free",
    switch = "y"
  ) +
  scale_fill_gradient2(
    low = fig5_col_low,
    mid = fig5_col_mid,
    high = fig5_col_high,
    midpoint = 0,
    limits = c(-0.5, 0.5),
    name = expression((ET[product] - ET[WB])/ET[WB]),
    oob = scales::squish
  ) +
  
  labs(
    x = "Basin",
    y = NULL
  ) +
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

fig5 <- ggarrange(fig_5_a_b_c, fig_heat, nrow = 2, labels = c("", "d"))

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig5_basin_comparison.pdf"
  ),
  plot = fig5,
  width = figure_widths,
  height = 1.3*20,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)
