# Figure 3: agreement patterns and transitions at 8 degrees ----
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
aggregation_scale <- 8
higher_agreement_area_threshold <- .5

panels <- build_scale_agreement_panels(aggregation_scale, higher_agreement_area_threshold)[[1]]
pattern_row <- assemble_scale_maps(list(panels$magnitude, panels$distribution))
transition_row <- assemble_scale_maps(list(
  panels$magnitude_transition, panels$distribution_transition), labels = c("c", "d"))
fig3 <- ggarrange(pattern_row, transition_row, ncol = 1, heights = c(1, 1))
save_manuscript_figure(fig3, file.path(paths$main, "fig3_scale_persistence"),
                       c(figure_widths, 24))
saveRDS(panels[c("magnitude_data", "distribution_data")],
         file.path(paths$data, "fig3_scale_persistence_8deg.rds"))
