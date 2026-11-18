# Figure 2: agreement maps and environmental signatures ----
library(data.table)
library(sf)
library(stars)
library(raster)
library(tibble)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R", local = TRUE)
source("source/partition_evap_graphics.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/environmental_fingerprints.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/agreement_environment_panels.R", local = TRUE)


source("source/geo_functions.R", local = TRUE)
source("source/graphics.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
figure_height <- 36
joint_levels_short <- c("Both higher", "Both lower", "Distribution higher", "Magnitude higher")

fig2 <- build_agreement_environment_figure()
save_manuscript_figure(fig2, file.path(paths$main, "fig2_agreement_environments"),
                       c(figure_widths, figure_height))
