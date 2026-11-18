# Figure 3 SI 03: fine-to-coarse agreement heatmaps ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/scale_transfer_heatmaps.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
panels <- build_scale_transfer_heatmaps()
save_manuscript_figure(panels$figure,
  file.path(paths$si, "fig3_SI_03_scale_transfer_heatmaps"), c(figure_widths, 23))
saveRDS(panels$data, file.path(paths$data, "fig3_scale_transfer_heatmap_8deg.rds"))
