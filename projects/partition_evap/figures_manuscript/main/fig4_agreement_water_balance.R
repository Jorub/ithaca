# Figure 4: agreement and water-balance accuracy ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
agreement_years <- 2000:2019
wb_tolerance <- 0.10
excluded_datasets <- character(0) # ETmonitor remains in the ensemble analysis.
wb_levels <- c("Below -10%", "Within 10%", "Above +10%")
wb_cols <- c("Below -10%" = fig5_col_low, "Within 10%" = "#4DAF4A",
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
# Figure layout ----
# Shared y-axis description for the three ensemble-median panels.
median_row <- ggarrange(fig_median_joint + labs(title = "a"),
                        fig_median_sIQR + labs(title = "b"),
                        fig_median_distribution + labs(title = "c"),
                        ncol = 3, align = "hv", legend = "none")
median_row <- annotate_figure(median_row,
  left = text_grob("Ensemble-median relative error (%)", rot = 90, size = 9))
absolute_rows <- ggarrange(
  fig_absolute_magnitude_area + labs(title = "d"),
  fig_absolute_sIQR + labs(title = "e", y = NULL),
  fig_absolute_distribution_area + labs(title = "f"),
  fig_absolute_distribution_basin + labs(title = "g", y = NULL),
  ncol = 2, nrow = 2, align = "hv", legend = "none")
fig4 <- ggarrange(median_row, absolute_rows, ncol = 1, heights = c(1, 1.8))
save_manuscript_figure(fig4, file.path(paths$main, "fig4_agreement_water_balance"),
                       c(figure_widths, 27))

# Descriptive counts, without retrospective best-product ranking or a new model.
basin_summary[, within_wb_tolerance := absolute_median_RE <= wb_tolerance]
product_counts <- basin_data[, .(
  N_below_tolerance = sum(mean_RE < -wb_tolerance),
  N_within_tolerance = sum(abs(mean_RE) <= wb_tolerance),
  N_above_tolerance = sum(mean_RE > wb_tolerance)
), ma_basin]
basin_summary <- merge(basin_summary, product_counts, by = "ma_basin", all.x = TRUE)
acceptance_counts <- basin_summary[, .(
  N_basins = .N, N_median_within_tolerance = sum(within_wb_tolerance),
  proportion_median_within_tolerance = mean(within_wb_tolerance)
)]
fwrite(basin_summary, file.path(paths$data, "fig4_basin_accuracy_summary.csv"))
fwrite(acceptance_counts, file.path(paths$data, "fig4_median_acceptance_counts.csv"))
saveRDS(basin_summary, file.path(paths$data, "fig4_basin_accuracy_summary.rds"))
