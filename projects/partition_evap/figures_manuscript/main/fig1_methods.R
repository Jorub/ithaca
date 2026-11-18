# Figure 1: agreement metrics ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/methods_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
figure_size <- c(figure_widths, 25)

fig1 <- build_methods_figure()
save_manuscript_figure(fig1, file.path(paths$main, "fig1_methods"), figure_size)
