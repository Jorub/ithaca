# Figure 4 SI 01: ensemble-median water-balance map ----
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
fig_map <- fig_map_5a + labs(title = NULL) + theme(legend.position = "bottom")
save_manuscript_figure(fig_map,
  file.path(paths$si, "fig4_SI_01_median_water_balance_error"), c(figure_widths, 16))
