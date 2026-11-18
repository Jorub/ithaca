# Figure 2 SI 07: environmental reference maps ----
library(data.table)
library(sf)
library(stars)
library(raster)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R", local = TRUE)
source("source/partition_evap_graphics.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/environment_reference_panels.R", local = TRUE)


source("source/geo_functions.R", local = TRUE)
source("source/graphics.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
panels <- build_environment_reference_panels()
fig_environments <- ggarrange(plotlist = panels, ncol = 2, nrow = 3,
                              labels = letters[1:6], align = "hv")
save_manuscript_figure(fig_environments, file.path(paths$si, "fig2_SI_07_environment_maps"),
                       c(figure_widths, 30))
