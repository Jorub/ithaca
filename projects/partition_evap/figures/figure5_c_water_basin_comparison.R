# Figure 5: basin agreement and water-balance accuracy ----
library(data.table)
library(sf)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

# Configuration ----
agreement_years <- 2000:2019
wb_tolerance <- 0.1
excluded_datasets <- character(0)
wb_levels <- c("Below -10%", "Within 10%", "Above +10%")
wb_cols <- c("Below -10%" = fig5_col_low,
             "Within 10%" = "#4DAF4A",
             "Above +10%" = fig5_col_high)

# Data ----
# Figure 5a preparation supplies product means and water balance for 2000-2016.
ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basin_data <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"
)))
basin_annual <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basin.rds"
)))
basin_distribution <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_ma_basin.rds"
)))

basin_data[, ma_basin := as.character(ma_basin)]
basin_annual[, ma_basin := as.character(ma_basin)]
basin_distribution[, ma_basin := as.character(ma_basin)]
basin_annual[, year := as.integer(as.character(year))]

basin_data <- basin_data[
  dataset != "Ma" & !dataset %in% excluded_datasets &
    is.finite(mean_evap) & is.finite(water_balance) & water_balance > 0
]
stopifnot(
  !anyDuplicated(basin_data[, .(ma_basin, dataset)]),
  !anyDuplicated(basin_distribution[, .(ma_basin)])
)

# Full-period product agreement ----
# These metrics describe agreement among products, independent of water balance.
basin_annual <- basin_annual[
  year %in% agreement_years & dataset != "Ma" &
    !dataset %in% excluded_datasets & is.finite(evap_mean)
]
stopifnot(!anyDuplicated(basin_annual[, .(ma_basin, dataset, year)]))

basin_product_means <- basin_annual[, .(
  agreement_mean_et = mean(evap_mean)
), .(ma_basin, dataset)]
basin_magnitude <- basin_product_means[, .(
  sIQR = IQR(agreement_mean_et) / median(agreement_mean_et)
), .(ma_basin)]

# Use the existing basin match ratios, including genuine zeros if present.
basin_distribution <- basin_distribution[, .(
  ma_basin, basin_match_ratio = index
)]
stopifnot(all(
  is.na(basin_distribution$basin_match_ratio) |
    between(basin_distribution$basin_match_ratio, 0, 1)
))

# Water-balance comparison ----
basin_data[, mean_RE := (mean_evap - water_balance) / water_balance]
basin_data[, absolute_RE := abs(mean_RE)]

# Summarise the ET products only. The reference is not an ensemble member.
basin_summary <- basin_data[, .(
  ensemble_median_et = median(mean_evap),
  water_balance = first(water_balance),
  N_products = .N,
  rel_da_higher = first(rel_da_higher),
  dist_da_higher = first(dist_da_higher),
  joint_da_higher = first(joint_da_higher),
  continent_abr = first(continent_abr)
), .(ma_basin)]
basin_summary[, median_RE :=
                (ensemble_median_et - water_balance) / water_balance]
basin_summary[, absolute_median_RE := abs(median_RE)]
basin_summary[, median_wb_class := factor(fcase(
  median_RE < -wb_tolerance, "Below -10%",
  median_RE > wb_tolerance, "Above +10%",
  default = "Within 10%"
), levels = wb_levels)]

basin_summary <- merge(basin_summary, basin_magnitude,
                       by = "ma_basin", all.x = TRUE)
basin_summary <- merge(basin_summary, basin_distribution,
                       by = "ma_basin", all.x = TRUE)
stopifnot(!anyDuplicated(basin_summary[, .(ma_basin)]))

# Replace any older basin sIQR values with the full-period metric.
basin_data[, sIQR := NULL]
basin_data <- merge(
  basin_data,
  basin_summary[, .(ma_basin, sIQR, basin_match_ratio, median_wb_class)],
  by = "ma_basin", all.x = TRUE
)

# Panel a: ensemble-median error with 10% tolerance ----
ma_sf$BasinID <- as.character(ma_sf$BasinID)
map_data <- merge(
  ma_sf[, c("BasinID", "Name")], basin_summary,
  by.x = "BasinID", by.y = "ma_basin", all.x = TRUE
)

fig_map_5a <- ggplot() +
  geom_sf(data = world_no_antarctica,
          fill = "gray95", color = "gray80", linewidth = 0.2) +
  geom_sf(data = map_data, aes(fill = median_wb_class),
          color = "gray35", linewidth = 0.25) +
  geom_label_repel(
    data = map_data,
    aes(label = BasinID, geometry = geometry),
    stat = "sf_coordinates", min.segment.length = 0,
    box.padding = 0.5, point.padding = 0.1,
    size = 2, segment.color = "gray35", max.overlaps = Inf
  ) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE,
                    na.value = "gray85", na.translate = FALSE,
                    name = "Ensemble-median error") +
  labs(x = NULL, y = NULL, title = "a") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  theme_map_fig5 +
  theme(legend.position = "none")

# Plot data ----
metric_columns <- c(
  "rel_high_pct", "sIQR", "dist_high_pct", "basin_match_pct"
)
basin_data[, rel_high_pct := rel_da_higher * 100]
basin_data[, dist_high_pct := dist_da_higher * 100]
basin_data[, basin_match_pct := basin_match_ratio * 100]
basin_summary[, rel_high_pct := rel_da_higher * 100]
basin_summary[, dist_high_pct := dist_da_higher * 100]
basin_summary[, joint_high_pct := joint_da_higher * 100]
basin_summary[, basin_match_pct := basin_match_ratio * 100]

median_plot_data <- melt(
  basin_summary[, .(
    ma_basin, median_RE, median_wb_class,
    joint_high_pct, sIQR, basin_match_pct
  )],
  id.vars = c("ma_basin", "median_RE", "median_wb_class"),
  measure.vars = c("joint_high_pct", "sIQR", "basin_match_pct"),
  variable.name = "agreement_metric", value.name = "agreement_value"
)

absolute_plot_data <- melt(
  basin_data[, c("ma_basin", "dataset", "absolute_RE", metric_columns),
             with = FALSE],
  id.vars = c("ma_basin", "dataset", "absolute_RE"),
  measure.vars = metric_columns,
  variable.name = "agreement_metric", value.name = "agreement_value"
)
median_absolute_data <- melt(
  basin_summary[, c("ma_basin", "absolute_median_RE", "median_wb_class",
                    metric_columns), with = FALSE],
  id.vars = c("ma_basin", "absolute_median_RE", "median_wb_class"),
  measure.vars = metric_columns,
  variable.name = "agreement_metric", value.name = "agreement_value"
)

scatter_theme <- theme_fig5 + theme(
  legend.position = "none",
  axis.title = element_text(size = 9),
  axis.text = element_text(size = 9),
  plot.title = element_text(size = 11, face = "bold", hjust = 0),
  plot.title.position = "plot",
  plot.margin = margin(5, 8, 8, 8)
)

# Panels b-d: signed error of the ensemble median ----
fig_median_joint <- ggplot(
  median_plot_data[agreement_metric == "joint_high_pct"],
  aes(x = agreement_value, y = median_RE)
) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -wb_tolerance, ymax = wb_tolerance,
           fill = "gray90", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray45", linewidth = 0.3) +
  geom_hline(yintercept = c(-wb_tolerance, wb_tolerance),
             color = "gray60", linewidth = 0.25) +
  geom_point(aes(fill = median_wb_class), shape = 21,
             color = "gray20", size = 2.3, stroke = 0.25) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "b", x = "Area fraction of higher joint agreement (%)",
       y = NULL) +
  scatter_theme

fig_median_sIQR <- ggplot(
  median_plot_data[agreement_metric == "sIQR"],
  aes(x = agreement_value, y = median_RE)
) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -wb_tolerance, ymax = wb_tolerance,
           fill = "gray90", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray45", linewidth = 0.3) +
  geom_hline(yintercept = c(-wb_tolerance, wb_tolerance),
             color = "gray60", linewidth = 0.25) +
  geom_point(aes(fill = median_wb_class), shape = 21,
             color = "gray20", size = 2.3, stroke = 0.25) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "c", x = "Basin sIQR",
       y = NULL) +
  scatter_theme

fig_median_distribution <- ggplot(
  median_plot_data[agreement_metric == "basin_match_pct"],
  aes(x = agreement_value, y = median_RE)
) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = -wb_tolerance, ymax = wb_tolerance,
           fill = "gray90", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray45", linewidth = 0.3) +
  geom_hline(yintercept = c(-wb_tolerance, wb_tolerance),
             color = "gray60", linewidth = 0.25) +
  geom_point(aes(fill = median_wb_class), shape = 21,
             color = "gray20", size = 2.3, stroke = 0.25) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "d", x = "Basin distribution match ratio (%)",
       y = NULL) +
  scatter_theme

fig_median <- ggarrange(
  fig_median_joint, fig_median_sIQR, fig_median_distribution,
  nrow = 3, align = "v", legend = "none"
)
fig_median <- annotate_figure(
  fig_median,
  left = text_grob("Ensemble-median relative error (%)", rot = 90, size = 9)
)

# Panels e-h: absolute errors of products and the ensemble median ----
fig_absolute_magnitude_area <- ggplot() +
  geom_hline(yintercept = wb_tolerance, linetype = "dashed",
             color = "gray40", linewidth = 0.35) +
  geom_point(
    data = absolute_plot_data[agreement_metric == "rel_high_pct"],
    aes(x = agreement_value, y = absolute_RE),
    color = "gray65", alpha = 0.45, size = 1.1
  ) +
  geom_point(
    data = median_absolute_data[agreement_metric == "rel_high_pct"],
    aes(x = agreement_value, y = absolute_median_RE,
        fill = median_wb_class),
    shape = 21, color = "gray20", size = 2.1, stroke = 0.25
  ) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "e", x = "Area fraction of higher magnitude agreement (%)",
       y = "Absolute relative error (%)") +
  scatter_theme

fig_absolute_sIQR <- ggplot() +
  geom_hline(yintercept = wb_tolerance, linetype = "dashed",
             color = "gray40", linewidth = 0.35) +
  geom_point(
    data = absolute_plot_data[agreement_metric == "sIQR"],
    aes(x = agreement_value, y = absolute_RE),
    color = "gray65", alpha = 0.45, size = 1.1
  ) +
  geom_point(
    data = median_absolute_data[agreement_metric == "sIQR"],
    aes(x = agreement_value, y = absolute_median_RE,
        fill = median_wb_class),
    shape = 21, color = "gray20", size = 2.1, stroke = 0.25
  ) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "f", x = "Basin sIQR",
       y = "Absolute relative error (%)") +
  scatter_theme

fig_absolute_distribution_area <- ggplot() +
  geom_hline(yintercept = wb_tolerance, linetype = "dashed",
             color = "gray40", linewidth = 0.35) +
  geom_point(
    data = absolute_plot_data[agreement_metric == "dist_high_pct"],
    aes(x = agreement_value, y = absolute_RE),
    color = "gray65", alpha = 0.45, size = 1.1
  ) +
  geom_point(
    data = median_absolute_data[agreement_metric == "dist_high_pct"],
    aes(x = agreement_value, y = absolute_median_RE,
        fill = median_wb_class),
    shape = 21, color = "gray20", size = 2.1, stroke = 0.25
  ) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "g", x = "Area fraction of higher distribution agreement (%)",
       y = "Absolute relative error (%)") +
  scatter_theme

fig_absolute_distribution_basin <- ggplot() +
  geom_hline(yintercept = wb_tolerance, linetype = "dashed",
             color = "gray40", linewidth = 0.35) +
  geom_point(
    data = absolute_plot_data[agreement_metric == "basin_match_pct"],
    aes(x = agreement_value, y = absolute_RE),
    color = "gray65", alpha = 0.45, size = 1.1
  ) +
  geom_point(
    data = median_absolute_data[agreement_metric == "basin_match_pct"],
    aes(x = agreement_value, y = absolute_median_RE,
        fill = median_wb_class),
    shape = 21, color = "gray20", size = 2.1, stroke = 0.25
  ) +
  scale_fill_manual(values = wb_cols, limits = wb_levels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "h", x = "Basin distribution match ratio (%)",
       y = "Absolute relative error (%)") +
  scatter_theme

fig_absolute <- ggarrange(
  fig_absolute_magnitude_area, fig_absolute_sIQR,
  fig_absolute_distribution_area, fig_absolute_distribution_basin,
  nrow = 2, ncol = 2, align = "hv", legend = "none"
)

# Shared key for the map and all scatterplots ----
legend_data <- data.table(
  median_wb_class = factor(wb_levels, levels = wb_levels),
  x = seq_along(wb_levels), y = 1
)
legend_plot <- ggplot(
  legend_data, aes(x = x, y = y, fill = median_wb_class)
) +
  geom_point(shape = 21, color = "gray20", size = 3, show.legend = TRUE) +
  scale_fill_manual(
    values = wb_cols, limits = wb_levels, drop = FALSE,
    name = "Ensemble-median relative error (map and points, a-h)"
  ) +
  guides(
    fill = guide_legend(
      nrow = 1, title.position = "top", order = 1,
      override.aes = list(shape = 21, color = "gray20")
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.box.spacing = unit(0.3, "cm"),
    legend.margin = margin(4, 8, 4, 8)
  )

# Select the bottom guide explicitly, including on older ggplot versions.
legend_table <- ggplotGrob(legend_plot)
legend_index <- which(legend_table$layout$name == "guide-box-bottom")
if (length(legend_index) == 0) {
  legend_index <- which(legend_table$layout$name == "guide-box")
}
stopifnot(length(legend_index) == 1)
common_basin_legend <- legend_table$grobs[[legend_index]]

# Panel i: product-specific signed errors ----
basin_order <- basin_summary[order(median_RE, ma_basin), ma_basin]
basin_data[, ma_basin_ord := factor(ma_basin, levels = basin_order)]
dataset_order <- basin_data[, .(mean_error = mean(mean_RE)),
                            by = dataset][order(mean_error), dataset]
basin_data[, dataset := factor(dataset, levels = dataset_order)]

basin_names <- as.data.table(st_drop_geometry(map_data))[
  , .(ma_basin = BasinID, Name)
]
basin_labels <- merge(
  basin_summary[, .(ma_basin, continent_abr)], basin_names,
  by = "ma_basin", all.x = TRUE
)
basin_labels[, basin_label := paste(continent_abr, Name, ma_basin, sep = " - ")]
basin_axis_labels <- setNames(basin_labels$basin_label, basin_labels$ma_basin)

fig_heat <- ggplot(
  basin_data, aes(x = ma_basin_ord, y = dataset, fill = mean_RE)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  facet_grid(rows = vars(dataset_type), scales = "free_y",
             space = "free_y", switch = "y",
             labeller = labeller(dataset_type = c(
               "Composite" = "Composite",
               "Hydr./LSM model" = "Hydr./LSM\nmodel",
               "Reanalysis" = "Reanalysis",
               "Remote sensing" = "Remote\nsensing"
             ))) +
  scale_fill_gradient2(
    low = fig5_col_low, mid = fig5_col_mid, high = fig5_col_high,
    midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish,
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = scales::label_percent(accuracy = 1),
    name = "Product relative error (%)",
    guide = guide_colorbar(
      barwidth = unit(10, "cm"), barheight = unit(0.45, "cm"),
      title.position = "top", title.hjust = 0.5
    )
  ) +
  scale_x_discrete(labels = basin_axis_labels, drop = FALSE) +
  labs(x = "Basin", y = NULL, title = "i") +
  theme_fig5 +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0, hjust = 1, size = 9, face = "bold", colour = fig5_col_dark
    ),
    panel.spacing.y = unit(0.08, "lines"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.margin = margin(10, 8, 10, 8),
    legend.box.margin = margin(8, 0, 6, 0),
    plot.title = element_text(size = 11, face = "bold", hjust = 0),
    plot.margin = margin(6, 12, 12, 18)
  )

# Assemble and save ----
fig_overview <- ggarrange(
  fig_map_5a, fig_median, ncol = 2, widths = c(1.6, 1)
)
fig5 <- ggarrange(
  fig_overview, cowplot::ggdraw(common_basin_legend), fig_absolute,
  nrow = 3, heights = c(1.35, 0.18, 1.15)
)

saveRDS(basin_summary, paste0(
  PATH_SAVE_PARTITION_EVAP, "ma_basin_agreement_accuracy_summary.rds"
))
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig5_basin_comparison.pdf"),
  plot = fig5, width = figure_widths, height = 27,
  units = "cm", dpi = 300, device = cairo_pdf, bg = "white"
)
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig5_basin_comparison.png"),
  plot = fig5, width = figure_widths, height = 27,
  units = "cm", dpi = 300, bg = "white"
)
