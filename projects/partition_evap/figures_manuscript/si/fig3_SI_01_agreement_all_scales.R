# Figure 3 SI 01: agreement all scales ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/scale_agreement_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
aggregation_scales <- c(1, 2, 4, 8)
higher_agreement_area_threshold <- .5

panels <- build_scale_agreement_panels(aggregation_scales, higher_agreement_area_threshold)
plots <- unlist(lapply(panels, function(p) p[c("magnitude", "distribution")]), recursive = FALSE)
figure <- assemble_scale_maps(plots)
save_manuscript_figure(figure, file.path(paths$si, "fig3_SI_01_agreement_all_scales"), c(figure_widths, 38))
