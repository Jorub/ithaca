# Figure 4: agreement and water-balance accuracy ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R", local = TRUE)
source("source/partition_evap_graphics.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/basin_accuracy_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
agreement_years <- 2000:2019
wb_tolerance <- 0.10
excluded_datasets <- character(0) # ETmonitor remains in the ensemble analysis.
wb_levels <- c("Below -10%", "Within 10%", "Above +10%")
wb_cols <- c("Below -10%" = fig5_col_low, "Within 10%" = "#4DAF4A",
             "Above +10%" = fig5_col_high)

panels <- build_basin_accuracy_panels()
# Shared y-axis description for the three ensemble-median panels.
median_row <- ggarrange(panels$median_joint + labs(title = "a"),
                        panels$median_siqr + labs(title = "b"),
                        panels$median_distribution + labs(title = "c"),
                        ncol = 3, align = "hv", legend = "none")
median_row <- annotate_figure(median_row,
  left = text_grob("Ensemble-median relative error (%)", rot = 90, size = 9))
absolute_rows <- ggarrange(
  panels$absolute_magnitude_area + labs(title = "d"),
  panels$absolute_siqr + labs(title = "e", y = NULL),
  panels$absolute_distribution_area + labs(title = "f"),
  panels$absolute_distribution_basin + labs(title = "g", y = NULL),
  ncol = 2, nrow = 2, align = "hv", legend = "none")
fig4 <- ggarrange(median_row, absolute_rows, ncol = 1, heights = c(1, 1.8))
save_manuscript_figure(fig4, file.path(paths$main, "fig4_agreement_water_balance"),
                       c(figure_widths, 27))

# Descriptive counts, without retrospective best-product ranking or a new model.
basin_summary <- panels$summary
basin_summary[, within_wb_tolerance := absolute_median_RE <= wb_tolerance]
product_counts <- panels$products[, .(
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
