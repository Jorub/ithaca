# Figure 6: water-balance filtering and spatial disagreement ----
library(data.table)
library(sf)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

# Configuration ----
wb_tolerance <- 0.10
high_grid_difference <- 0.20

# Data ----
basin_comparison <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"
)))
agreement_grid <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"
)))
diff_pp <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"
)))
ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))

if (!identical(attr(diff_pp, "sym_rel_diff_definition"), "cellwise_pair_mean")) {
  stop("Regenerate product_pair_difference_gridwise.rds with the corrected 07f script.")
}

basin_comparison[, ma_basin := as.character(ma_basin)]
agreement_grid[, ma_basin := as.character(ma_basin)]
basin_comparison <- basin_comparison[
  dataset != "Ma" & is.finite(mean_evap) &
    is.finite(water_balance) & water_balance > 0
]
basin_comparison[, mean_RE := (mean_evap - water_balance) / water_balance]
basin_comparison[, retained := abs(mean_RE) <= wb_tolerance]

stopifnot(
  !anyDuplicated(basin_comparison[, .(ma_basin, dataset)]),
  !anyDuplicated(agreement_grid[, .(lon, lat)]),
  !anyDuplicated(diff_pp[, .(lon, lat, dataset.x, dataset.y)])
)

grid_lookup <- agreement_grid[
  !is.na(ma_basin) & is.finite(area) & area > 0,
  .(lon, lat, ma_basin, area)
]

basin_summary <- basin_comparison[, .(
  N_total = .N, N_retained = sum(retained)
), by = ma_basin]
basin_summary[, retained_fraction := N_retained / N_total]
basin_summary <- merge(
  basin_summary,
  grid_lookup[, .(basin_area = sum(area)), by = ma_basin],
  by = "ma_basin", all.x = TRUE
)

# Pairwise differences before and after filtering ----
diff_basin <- merge(
  diff_pp[, .(lon, lat, dataset.x, dataset.y,
              sym_rel_diff, pair_mean_et, et_delta, abs_et_delta)],
  grid_lookup, by = c("lon", "lat")
)
retention_lookup <- basin_comparison[, .(ma_basin, dataset, retained)]
diff_basin <- merge(
  diff_basin, retention_lookup,
  by.x = c("ma_basin", "dataset.x"), by.y = c("ma_basin", "dataset")
)
setnames(diff_basin, "retained", "retained_x")
diff_basin <- merge(
  diff_basin, retention_lookup,
  by.x = c("ma_basin", "dataset.y"), by.y = c("ma_basin", "dataset")
)
setnames(diff_basin, "retained", "retained_y")
diff_basin <- diff_basin[is.finite(sym_rel_diff)]

grid_full <- diff_basin[, .(full_difference = median(sym_rel_diff)),
                        by = .(ma_basin, lon, lat, area)]
grid_kept <- diff_basin[retained_x & retained_y,
                        .(retained_difference = median(sym_rel_diff)),
                        by = .(ma_basin, lon, lat, area)
]
grid_comparison <- merge(
  grid_full, grid_kept, by = c("ma_basin", "lon", "lat", "area"),
  all.x = TRUE
)
grid_comparison[, difference_change := retained_difference - full_difference]

# Use the same valid cells for full and retained ensemble summaries.
basin_differences <- grid_comparison[is.finite(retained_difference), .(
  D_full = weighted_median(full_difference, area),
  D_retained = weighted_median(retained_difference, area),
  common_area = sum(area),
  high_difference_fraction = sum(area[retained_difference > high_grid_difference]) /
    sum(area)
), by = ma_basin]
basin_differences[, delta_D := D_retained - D_full]
basin_summary <- merge(basin_summary, basin_differences,
                       by = "ma_basin", all.x = TRUE)
basin_summary[, common_area_fraction := common_area / basin_area]

# Pairwise compensation for the subsequent case studies ----
pair_compensation <- diff_basin[
  retained_x & retained_y & is.finite(et_delta) & is.finite(abs_et_delta) &
    is.finite(pair_mean_et),
  .(
    gross_difference = weighted.mean(abs_et_delta, area),
    net_difference = abs(weighted.mean(et_delta, area)),
    pair_mean_et = weighted.mean(pair_mean_et, area),
    pair_area = sum(area)
  ), by = .(ma_basin, dataset.x, dataset.y)
]
pair_compensation[, compensation := fifelse(
  gross_difference > 0, 1 - net_difference / gross_difference, NA_real_
)]
pair_compensation[, gross_relative_difference := gross_difference / pair_mean_et]

# Map and heatmap data ----
ma_sf$BasinID <- as.character(ma_sf$BasinID)
map_data <- merge(
  ma_sf[, c("BasinID", "Name")], basin_summary,
  by.x = "BasinID", by.y = "ma_basin", all.x = TRUE
)

basin_names <- as.data.table(st_drop_geometry(ma_sf))[
  , .(ma_basin = as.character(BasinID), basin_name = Name)
]
basin_summary <- merge(basin_summary, basin_names,
                       by = "ma_basin", all.x = TRUE)

# Order heatmap by retained-product count.
basin_order <- basin_summary[order(-N_retained, ma_basin), ma_basin]
basin_comparison[, basin_plot := factor(ma_basin, levels = basin_order)]
basin_labels <- setNames(basin_summary$basin_name, basin_summary$ma_basin)
dataset_order <- basin_comparison[, .(N_retained = sum(retained)),
                                  by = dataset][order(-N_retained, dataset), dataset]
basin_comparison[, dataset_plot := factor(dataset, levels = rev(dataset_order))]

# Three global maps ----
map_base <- function(data, title) {
  ggplot() +
    geom_sf(data = world_no_antarctica,
            fill = "gray95", colour = "gray80", linewidth = 0.2) +
    geom_sf(data = data, aes(fill = value),
            colour = "gray45", linewidth = 0.25) +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    coord_sf(ylim = c(-70, 90), expand = FALSE) +
    labs(x = NULL, y = NULL, title = title) +
    theme_map_fig5 +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}

# a: Number of retained products.
map_data$value <- map_data$N_retained
fig_retained <- map_base(map_data, "a") +
  scale_fill_viridis_c(
    option = "C", direction = -1, breaks = pretty(map_data$N_retained),
    name = "Products retained", na.value = "gray85"
  )

# b: Percentage of basin area with >20% median pairwise difference.
area_breaks <- seq(0, 100, 20)
area_labels <- paste0(head(area_breaks, -1), "–", tail(area_breaks, -1), "%")
map_data$value <- cut(
  100 * map_data$high_difference_fraction,
  breaks = area_breaks, labels = area_labels, include.lowest = TRUE
)
fig_residual <- map_base(map_data, "b") +
  scale_fill_manual(
    values = setNames(viridisLite::viridis(5, option = "C", direction = -1),
                      area_labels),
    limits = area_labels, drop = FALSE,
    name = "Area with >20% difference", na.value = "gray85",
    guide = guide_legend(nrow = 2, byrow = TRUE)
  )

# c: Change in basin median pairwise difference after filtering.
change_breaks <- c(-Inf, -0.1, -0.05, 0.05, 0.1, Inf)
change_labels <- c("< -10 pp", "-10 to -5 pp", "-5 to +5 pp",
                   "+5 to +10 pp", "> +10 pp")
map_data$value <- cut(map_data$delta_D, breaks = change_breaks,
                      labels = change_labels, include.lowest = TRUE)
fig_change <- map_base(map_data, "c") +
  scale_fill_manual(
    values = setNames(colorRampPalette(c(fig5_col_low, fig5_col_mid,
                                         fig5_col_high))(5), change_labels),
    limits = change_labels, drop = FALSE,
    name = "Change after filtering", na.value = "gray85",
    guide = guide_legend(nrow = 2, byrow = TRUE)
  )

# d: Product-specific signed errors and retained-product outlines ----
fig_datasets <- ggplot(basin_comparison) +
  geom_tile(aes(x = basin_plot, y = dataset_plot, fill = mean_RE),
            colour = "white", linewidth = 0.2) +
  geom_tile(
    data = basin_comparison[retained == TRUE],
    aes(x = basin_plot, y = dataset_plot,
        colour = "Within 10% of water balance"),
    fill = NA, linewidth = 0.65
  ) +
  facet_grid(dataset_type ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = basin_labels, drop = FALSE) +
  scale_fill_gradient2(
    low = fig5_col_low, mid = fig5_col_mid, high = fig5_col_high,
    midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish,
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = scales::label_percent(accuracy = 1),
    name = "Product relative error (%)",
    guide = guide_colorbar(
      barwidth = unit(10, "cm"), barheight = unit(0.4, "cm"),
      title.position = "top", title.hjust = 0.5
    )
  ) +
  scale_colour_manual(
    values = c("Within 10% of water balance" = "gray25"), name = NULL,
    guide = guide_legend(override.aes = list(fill = NA, linewidth = 0.65))
  ) +
  labs(x = "Basin", y = "Products", title = "d") +
  theme_fig5 +
  theme(
    panel.grid = element_blank(), strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom", legend.box = "horizontal",
    plot.title = element_text(size = 11, face = "bold")
  )

# Assemble and save ----
fig_maps <- ggarrange(fig_retained, fig_residual, fig_change,
                      ncol = 3, align = "hv")
fig6 <- ggarrange(fig_maps, fig_datasets, ncol = 1, heights = c(1.3, 1.8))

saveRDS(basin_summary, paste0(PATH_SAVE_PARTITION_EVAP,
                              "basin_filtering_summary.rds"))
saveRDS(pair_compensation, paste0(PATH_SAVE_PARTITION_EVAP,
                                  "basin_retained_pair_compensation.rds"))
saveRDS(grid_comparison, paste0(PATH_SAVE_PARTITION_EVAP,
                                "basin_filtering_grid_comparison.rds"))

ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig6_basin_filtering.pdf"),
  plot = fig6, width = figure_widths, height = 27,
  units = "cm", dpi = 300, device = cairo_pdf, bg = "white"
)
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig6_basin_filtering.png"),
  plot = fig6, width = figure_widths, height = 27,
  units = "cm", dpi = 300, bg = "white"
)
