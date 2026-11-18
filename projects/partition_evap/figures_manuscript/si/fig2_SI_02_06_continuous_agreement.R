# Figure 2 SI 02-06: continuous agreement and ET context ----
library(data.table)
library(sf)
library(stars)
library(raster)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/continuous_agreement_panels.R", local = TRUE)


source("source/geo_functions.R", local = TRUE)
source("source/graphics.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
panels <- build_continuous_agreement_panels()
stems <- c(mean_et = "fig2_SI_02_mean_et", iqr = "fig2_SI_03_iqr",
           siqr = "fig2_SI_04_siqr", match_ratio = "fig2_SI_05_match_ratio",
           iqr_mean_relationship = "fig2_SI_06_iqr_mean_relationship")
for (name in names(stems)) {
  size <- if (name == "iqr_mean_relationship") c(figure_widths, figure_widths) else
    c(figure_widths / 2, 10)
  save_manuscript_figure(panels[[name]], file.path(paths$si, stems[[name]]), size)
}
