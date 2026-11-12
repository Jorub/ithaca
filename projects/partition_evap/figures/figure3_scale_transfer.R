source('source/partition_evap.R')
source('source/partition_evap_graphics.R')

# Configuration ----
sIQR_breaks <- c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.75)
match_ratio_breaks <- seq(0, 0.8, 0.1)
fraction_levels <- c(
  "0%", ">0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
  "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
)
share_levels <- c(
  "0%", ">0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"
)
share_cols <- c(
  "0%" = "gray95",
  ">0-10%" = "#DEEBF7",
  "10-20%" = "#C6DBEF",
  "20-40%" = "#9ECAE1",
  "40-60%" = "#6BAED6",
  "60-80%" = "#3182BD",
  "80-100%" = "#08519C"
)

agreement_stats_scale <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))

stopifnot(!anyDuplicated(agreement_stats_scale[, .(lon, lat)]))

# 8 degrees ----
agreement_stats_scale[, area_8deg := sum(area), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, N_8deg := .N, .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_sIQR_0_25_8deg := median(std_quant_range, na.rm = T), .(deg_8_lon, deg_8_lat)]
# Assign each fraction to every fine-grid row in the coarse cell.
agreement_stats_scale[, higher_agreement_frac_8deg :=
                        sum(area[rel_dataset_agreement %in% c("High", "Above average")]) /
                        area_8deg, .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, lower_agreement_frac_8deg :=
                        sum(area[rel_dataset_agreement %in% c("Low", "Below average")]) /
                        area_8deg, .(deg_8_lon, deg_8_lat)]

agreement_stats_scale[, higher_distribution_frac_8deg :=
                        sum(area[dist_dataset_agreement %in% c("High", "Above average")]) /
                        area_8deg, .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, lower_distribution_frac_8deg :=
                        sum(area[dist_dataset_agreement %in% c("Low", "Below average")]) /
                        area_8deg, .(deg_8_lon, deg_8_lat)]

# 4 degrees ----
agreement_stats_scale[, area_4deg := sum(area), .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, N_4deg := .N, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, higher_agreement_frac_4deg :=
                        sum(area[rel_dataset_agreement %in% c("High", "Above average")]) /
                        area_4deg, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, lower_agreement_frac_4deg :=
                        sum(area[rel_dataset_agreement %in% c("Low", "Below average")]) /
                        area_4deg, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, median_sIQR_0_25_4deg := median(std_quant_range, na.rm = T), .(deg_4_lon, deg_4_lat)]

### Comparison of 0.25 to larger grid cell ----

reduction_long <- melt(
  agreement_stats_scale,
  id.vars = c("lon", "lat", "area", "rel_dataset_agreement"),
  measure.vars = c(
    "reduction_1deg",
    "reduction_2deg",
    "reduction_4deg",
    "reduction_8deg"
  ),
  variable.name = "resolution",
  value.name = "sIQR_reduction"
)

reduction_long[
  ,
  resolution := factor(
    resolution,
    levels = c(
      "reduction_1deg",
      "reduction_2deg",
      "reduction_4deg",
      "reduction_8deg"
    ),
    labels = c("1°", "2°", "4°", "8°")
  )
]

sIQR_long <- melt(
  agreement_stats_scale,
  id.vars = c("lon", "lat", "area", "rel_dataset_agreement"),
  measure.vars = c(
    "sIQR_1deg",
    "sIQR_2deg",
    "sIQR_4deg",
    "sIQR_8deg"
  ),
  variable.name = "resolution",
  value.name = "sIQR"
)

sIQR_long[
  ,
  resolution := factor(
    resolution,
    levels = c(
      "sIQR_1deg",
      "sIQR_2deg",
      "sIQR_4deg",
      "sIQR_8deg"
    ),
    labels = c("1°", "2°", "4°", "8°")
  )
]


sIQR_long_merge <- merge(sIQR_long, reduction_long, by = c("lon", "lat", "area", "rel_dataset_agreement", "resolution"))

sIQR_long_merge[, sIQR_red_break := cut(sIQR_reduction, breaks = c(-Inf, -0.1, 0.1, Inf),
                                        labels = c(
                                          "sIQR increased by >0.1",
                                          "Change within ±0.1",
                                          "sIQR decreased by >0.1"
                                        ))]


pathway_proportions <- sIQR_long_merge[,
                                       .(affected_area = sum(area)),
                                       by = .(resolution,rel_dataset_agreement,sIQR_red_break)
][, proportion := affected_area / sum(affected_area),
  by = .(resolution,rel_dataset_agreement)
]

### plots ----
ggplot(
  sIQR_long_merge,
  aes(x = rel_dataset_agreement, y = sIQR_reduction)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~resolution)+
  coord_cartesian(ylim = c(-0.5, 0.5))+
  theme_bw()+
  labs(
    x = "Grid-scale (0.25 degrees) magnitude-agreement class",
    y = expression(sIQR[0.25]-sIQR[aggregated])
  )

### FIGURE agreement class dependence ----
fig_agreement_class <- ggplot(
  sIQR_long_merge,
  aes(x = rel_dataset_agreement, y = sIQR, color = sIQR_red_break)
) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~resolution)+
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  labs(
    x = "Grid-scale (0.25 degrees) magnitude-agreement class",
    y = expression(sIQR[aggregated]
    )
  )+
  scale_colour_manual(
    values = c(
      "sIQR increased by >0.1" = "#D55E00",
      "Change within ±0.1"     = "grey65",
      "sIQR decreased by >0.1" = "#0072B2"
    ),
    labels = c(
      "Higher sIQR (>0.1)",
      "Similar sIQR (±0.1)",
      "Lower sIQR (>0.1)"
    ),
    name = "Change after aggregation"
  )+ theme_scale_grid+
  theme(
    axis.text.x = element_text(angle= 40, hjust = 1),
    legend.position = "top"
  )

ggplot(
  pathway_proportions,
  aes(
    x = rel_dataset_agreement,
    y = proportion,
    fill = sIQR_red_break
  )
) +
  geom_col(width = 0.75) +
  facet_wrap(~ resolution, ncol = 2) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "sIQR increased by >0.1" = "#D55E00",
      "Change within ±0.1"     = "grey65",
      "sIQR decreased by >0.1" = "#0072B2"
    ),
    labels = c(
      "Higher sIQR (>0.1)",
      "Similar sIQR (±0.1)",
      "Lower sIQR (>0.1)"
    ),
  ) +
  labs(
    x = "Magnitude-agreement class at 0.25°",
    y = "Area affected",
    fill = "Change after aggregation"
  ) +
  theme_bw()

### does area fraction of higher agreement result in higher aggregated sIQR-----
agreement_stats_scale_deg_8 <- unique(agreement_stats_scale[,.(deg_8_lon, deg_8_lat,
                                                               higher_agreement_frac_8deg,
                                                               lower_agreement_frac_8deg,
                                                               higher_distribution_frac_8deg,
                                                               lower_distribution_frac_8deg,
                                                               sIQR_8deg,
                                                               area_8deg, N_8deg,
                                                               median_sIQR_0_25_8deg)])

stopifnot(!anyDuplicated(agreement_stats_scale_deg_8[, .(deg_8_lon, deg_8_lat)]))

agreement_stats_scale_deg_8[N_8deg >= 128 & area_8deg >= 1e+11, .N]
agreement_stats_scale_deg_8[higher_agreement_frac_8deg > 0, .N]

### filter ----
agreement_deg_8_filt <- agreement_stats_scale_deg_8[N_8deg >= 128 & area_8deg >= 1e+11]

agreement_stats_scale_deg_4 <- unique(agreement_stats_scale[,.(deg_4_lon, deg_4_lat,
                                                               higher_agreement_frac_4deg,
                                                               lower_agreement_frac_4deg,
                                                               sIQR_4deg,
                                                               area_4deg, N_4deg,
                                                               median_sIQR_0_25_4deg)])

stopifnot(!anyDuplicated(agreement_stats_scale_deg_4[, .(deg_4_lon, deg_4_lat)]))

agreement_stats_scale_deg_4[N_4deg >= 128/4 & area_4deg >= 1e+11, .N]
agreement_deg_4_filt <- agreement_stats_scale_deg_4[N_4deg >= 128/4 & area_4deg >= 1e+11]

ggplot(agreement_deg_4_filt)+
  geom_point(aes(x = higher_agreement_frac_4deg, y = sIQR_4deg), alpha = 0.5)+
  theme_bw()

### FIGURE  higher area fraction plot -----
higher_af <- ggplot()+
  geom_point(data = agreement_deg_4_filt,
             aes(x = higher_agreement_frac_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt,
             aes(x = higher_agreement_frac_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  theme_bw()+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Area fraction of 0.25° grid cells with higher agreement",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale_grid+
  theme(legend.position = "top")



ggplot()+
  geom_point(data = agreement_deg_4_filt[higher_agreement_frac_4deg == 0],
             aes(x = median_sIQR_0_25_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt[higher_agreement_frac_8deg == 0],
             aes(x = median_sIQR_0_25_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    linewidth = 0.7,
    colour = "grey35"
  ) +
  coord_equal(
    xlim = c(0, 1.8),
    ylim = c(0, 1.8)
  ) +
  labs(
    x = "Median sIQR of constituent 0.25° grid cells",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale+
  theme(legend.position = "top")


### FIGURE  lower area fraction plot -----
lower_af <- ggplot()+
  geom_point(data = agreement_deg_4_filt,
             aes(x = lower_agreement_frac_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt,
             aes(x = lower_agreement_frac_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  theme_bw()+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Area fraction of 0.25° grid cells with lower agreement",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale_grid+
  theme(legend.position = "top")

ggarrange(higher_af, lower_af, common.legend = T, nrow = 2)

# Distribution agreement at 8 degrees ----
# Use existing KS results rather than rerunning tests. Starting from the
# pairwise table retains coarse cells with valid tests but zero matches.
distribution_tests_8deg <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise_8deg.rds"
))
stopifnot(!anyDuplicated(distribution_tests_8deg[
  , .(deg_8_lon, deg_8_lat, dataset.x, dataset.y)
]))

distribution_8deg <- distribution_tests_8deg[, .(
  counts = sum(!is.na(KS_test_p)),
  matches = sum(KS_test_p > 0.05, na.rm = TRUE)
), .(deg_8_lon, deg_8_lat)]
distribution_8deg <- distribution_8deg[counts > 77]
distribution_8deg[, match_ratio := matches / counts]

distribution_8deg <- merge(
  agreement_stats_scale_deg_8[, .(
    deg_8_lon, deg_8_lat, area_8deg, N_8deg,
    higher_distribution_frac_8deg, lower_distribution_frac_8deg
  )],
  distribution_8deg, by = c("deg_8_lon", "deg_8_lat")
)
distribution_8deg <- distribution_8deg[
  N_8deg >= 128 & area_8deg >= 1e+11
]
stopifnot(!anyDuplicated(distribution_8deg[, .(deg_8_lon, deg_8_lat)]))

# Heatmap data ----
# Keep one observation per coarse cell, metric and fine-grid agreement group.
magnitude_heatmap_cells <- agreement_deg_8_filt[, .(
  deg_8_lon, deg_8_lat,
  metric = "Magnitude",
  aggregated_value = sIQR_8deg,
  Higher = higher_agreement_frac_8deg,
  Lower = lower_agreement_frac_8deg
)]
distribution_heatmap_cells <- distribution_8deg[, .(
  deg_8_lon, deg_8_lat,
  metric = "Distribution",
  aggregated_value = match_ratio,
  Higher = higher_distribution_frac_8deg,
  Lower = lower_distribution_frac_8deg
)]

heatmap_cells <- rbind(magnitude_heatmap_cells, distribution_heatmap_cells)
heatmap_cells <- heatmap_cells[is.finite(aggregated_value)]

# Display the requested ranges: sIQR up to 0.75 and match ratio up to 0.8.
heatmap_cells[metric == "Magnitude", coarse_bin := as.character(cut(
  aggregated_value, breaks = sIQR_breaks,
  include.lowest = TRUE, right = FALSE
))]
heatmap_cells[metric == "Distribution", coarse_bin := as.character(cut(
  aggregated_value, breaks = match_ratio_breaks,
  include.lowest = TRUE, right = TRUE
))]

magnitude_bin_levels <- levels(cut(
  numeric(0), breaks = sIQR_breaks,
  include.lowest = TRUE, right = FALSE
))
distribution_bin_levels <- levels(cut(
  numeric(0), breaks = match_ratio_breaks,
  include.lowest = TRUE, right = TRUE
))

heatmap_long <- melt(
  heatmap_cells,
  id.vars = c("deg_8_lon", "deg_8_lat", "metric",
              "aggregated_value", "coarse_bin"),
  measure.vars = c("Higher", "Lower"),
  variable.name = "agreement_group",
  value.name = "area_fraction"
)
heatmap_long <- heatmap_long[
  !is.na(coarse_bin) & is.finite(area_fraction)
]

heatmap_long[, fraction_bin := fcase(
  area_fraction == 0, "0%",
  area_fraction <= 0.1, ">0-10%",
  area_fraction <= 0.2, "10-20%",
  area_fraction <= 0.3, "20-30%",
  area_fraction <= 0.4, "30-40%",
  area_fraction <= 0.5, "40-50%",
  area_fraction <= 0.6, "50-60%",
  area_fraction <= 0.7, "60-70%",
  area_fraction <= 0.8, "70-80%",
  area_fraction <= 0.9, "80-90%",
  default = "90-100%"
)]

heatmap_counts <- heatmap_long[, .(N = .N),
                               .(metric, agreement_group, coarse_bin, fraction_bin)
]

# Complete the grid to distinguish zero counts from unsampled coarse bins.
magnitude_grid <- CJ(
  agreement_group = c("Higher", "Lower"),
  coarse_bin = magnitude_bin_levels,
  fraction_bin = fraction_levels
)
magnitude_grid[, metric := "Magnitude"]

distribution_grid <- CJ(
  agreement_group = c("Higher", "Lower"),
  coarse_bin = distribution_bin_levels,
  fraction_bin = fraction_levels
)
distribution_grid[, metric := "Distribution"]

heatmap_data <- merge(
  rbind(magnitude_grid, distribution_grid),
  heatmap_counts,
  by = c("metric", "agreement_group", "coarse_bin", "fraction_bin"),
  all.x = TRUE
)
heatmap_data[is.na(N), N := 0L]

# As in the original heatmap, each populated column sums to 100%.
# These are counts of coarse cells; fine-grid fractions remain area weighted.
heatmap_data[, N_bin := sum(N),
             .(metric, agreement_group, coarse_bin)]
heatmap_data[, proportion := fifelse(N_bin > 0, N / N_bin, NA_real_)]
heatmap_data[, fraction_bin := factor(fraction_bin,
                                      levels = fraction_levels)]

heatmap_data[, share_bin := fcase(
  is.na(proportion), NA_character_,
  proportion == 0, "0%",
  proportion <= 0.1, ">0-10%",
  proportion <= 0.2, "10-20%",
  proportion <= 0.4, "20-40%",
  proportion <= 0.6, "40-60%",
  proportion <= 0.8, "60-80%",
  default = "80-100%"
)]
heatmap_data[, share_bin := factor(share_bin, levels = share_levels)]

# Main figure: 8-degree scale transfer ----
heatmap_theme <- theme_scale + theme(
  panel.grid = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = 11, face = "bold"),
  legend.position = "bottom"
)

heatmap_fill <- scale_fill_manual(
  values = share_cols,
  limits = share_levels, breaks = share_levels, drop = FALSE,
  na.value = "grey85",
  na.translate = FALSE,
  name = "Share of coarse cells within aggregated bin",
  guide = guide_legend(nrow = 1, title.position = "top")
)

heatmap_magnitude_higher <- ggplot(
  heatmap_data[metric == "Magnitude" & agreement_group == "Higher"],
  aes(x = coarse_bin, y = fraction_bin, fill = share_bin)
) +
  geom_tile(colour = "white", linewidth = 0.3, show.legend = TRUE) +
  scale_x_discrete(limits = magnitude_bin_levels, drop = FALSE) +
  scale_y_discrete(limits = fraction_levels, drop = FALSE) +
  heatmap_fill +
  labs(
    title = "a  Higher magnitude agreement",
    x = "Aggregated sIQR at 8°",
    y = "Area fraction of 0.25° cells\nwith higher agreement"
  ) +
  heatmap_theme

heatmap_magnitude_lower <- ggplot(
  heatmap_data[metric == "Magnitude" & agreement_group == "Lower"],
  aes(x = coarse_bin, y = fraction_bin, fill = share_bin)
) +
  geom_tile(colour = "white", linewidth = 0.3, show.legend = TRUE) +
  scale_x_discrete(limits = magnitude_bin_levels, drop = FALSE) +
  scale_y_discrete(limits = fraction_levels, drop = FALSE) +
  heatmap_fill +
  labs(
    title = "b  Lower magnitude agreement",
    x = "Aggregated sIQR at 8°",
    y = "Area fraction of 0.25° cells\nwith lower agreement"
  ) +
  heatmap_theme

heatmap_distribution_higher <- ggplot(
  heatmap_data[metric == "Distribution" & agreement_group == "Higher"],
  aes(x = coarse_bin, y = fraction_bin, fill = share_bin)
) +
  geom_tile(colour = "white", linewidth = 0.3, show.legend = TRUE) +
  scale_x_discrete(limits = distribution_bin_levels, drop = FALSE) +
  scale_y_discrete(limits = fraction_levels, drop = FALSE) +
  heatmap_fill +
  labs(
    title = "c  Higher distribution agreement",
    x = "Aggregated distribution match ratio at 8°",
    y = "Area fraction of 0.25° cells\nwith higher agreement"
  ) +
  heatmap_theme

heatmap_distribution_lower <- ggplot(
  heatmap_data[metric == "Distribution" & agreement_group == "Lower"],
  aes(x = coarse_bin, y = fraction_bin, fill = share_bin)
) +
  geom_tile(colour = "white", linewidth = 0.3, show.legend = TRUE) +
  scale_x_discrete(limits = distribution_bin_levels, drop = FALSE) +
  scale_y_discrete(limits = fraction_levels, drop = FALSE) +
  heatmap_fill +
  labs(
    title = "d  Lower distribution agreement",
    x = "Aggregated distribution match ratio at 8°",
    y = "Area fraction of 0.25° cells\nwith lower agreement"
  ) +
  heatmap_theme

# The individual-cell boxplots and scatterplots above remain diagnostics.
# Extract the shared legend from panel c and display every share category.
legend_table <- ggplotGrob(
  heatmap_distribution_higher + theme(legend.position = "bottom")
)
legend_index <- which(legend_table$layout$name == "guide-box-bottom")
if (length(legend_index) == 0) {
  legend_index <- which(legend_table$layout$name == "guide-box")
}
stopifnot(length(legend_index) == 1)
common_heatmap_legend <- legend_table$grobs[[legend_index]]
stopifnot(inherits(common_heatmap_legend, "gtable"))

fig_scale_transfer <- ggarrange(
  heatmap_magnitude_higher, heatmap_magnitude_lower,
  heatmap_distribution_higher, heatmap_distribution_lower,
  nrow = 2, ncol = 2, align = "hv",
  legend = "none"
)
# Add the legend separately to avoid ggarrange's legend unit conversion.
fig_scale_transfer <- cowplot::plot_grid(
  fig_scale_transfer, cowplot::ggdraw(common_heatmap_legend),
  ncol = 1, rel_heights = c(1, 0.1)
)
# Save data ----
saveRDS(heatmap_data, paste0(
  PATH_SAVE_PARTITION_EVAP, "agreement_scale_transfer_heatmap_8deg.rds"
))

# Save figure ----
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig3_scale_transfer.pdf"),
  plot = fig_scale_transfer,
  width = figure_widths,
  height = 23,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig3_scale_transfer.png"),
  plot = fig_scale_transfer,
  width = figure_widths,
  height = 23,
  units = "cm",
  dpi = 300,
  bg = "white"
)
