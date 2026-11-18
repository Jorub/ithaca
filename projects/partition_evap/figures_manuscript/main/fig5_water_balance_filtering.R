# Figure 5: product retention by the basin water-balance filter ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/basin_retention_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
wb_tolerance <- .10

retention <- read_basin_retention(wb_tolerance)
fig5 <- ggarrange(basin_retained_map(retention), basin_retention_heatmap(retention),
                  ncol = 1, heights = c(1, 1.5))
save_manuscript_figure(fig5, file.path(paths$main, "fig5_water_balance_filtering"),
                       c(figure_widths, 27))
fwrite(retention$summary, file.path(paths$data, "fig5_product_retention_summary.csv"))
fwrite(retention$products[, .(ma_basin, dataset, mean_RE, retained)],
       file.path(paths$data, "fig5_product_retention_status.csv"))
