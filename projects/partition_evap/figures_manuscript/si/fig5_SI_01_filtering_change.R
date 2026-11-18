# Figure 5 SI 01: full versus filtered pairwise differences ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/filtering_overview_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
wb_tolerance <- .10
# Both panels take the median across per-pair summaries from 11g.

pairs <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds")))
products <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "basin_pair_product_status.rds")))
stopifnot(isTRUE(all.equal(attr(products, "wb_tolerance"), wb_tolerance)),
          isTRUE(all.equal(attr(pairs, "wb_tolerance"), wb_tolerance)))
summary <- summarise_filtering_changes(pairs, products)
basins <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basins$BasinID <- as.character(basins$BasinID)
map <- merge(basins, summary, by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
local <- filtering_change_map(map, "grid_change_pp", "a  Median grid-cell differences")
aggregate <- filtering_change_map(map, "aggregated_change_pp", "b  Basin-aggregated differences")
legend <- bottom_legend(local)
body <- ggarrange(local + theme(legend.position = "none"),
                   aggregate + theme(legend.position = "none"), ncol = 2, align = "hv")
figure <- cowplot::plot_grid(body, cowplot::ggdraw(legend), ncol = 1, rel_heights = c(1, .2))
save_manuscript_figure(figure, file.path(paths$si, "fig5_SI_01_filtering_change"),
                       c(figure_widths, 15))
saveRDS(summary, file.path(paths$data, "fig5_filtering_change_summary.rds"))
fwrite(summary, file.path(paths$data, "fig5_filtering_change_summary.csv"))
