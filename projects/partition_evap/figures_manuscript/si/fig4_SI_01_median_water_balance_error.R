# Figure 4 SI 01: ensemble-median water-balance map ----
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
fig_map <- panels$map + labs(title = NULL) + theme(legend.position = "bottom")
save_manuscript_figure(fig_map,
  file.path(paths$si, "fig4_SI_01_median_water_balance_error"), c(figure_widths, 16))
