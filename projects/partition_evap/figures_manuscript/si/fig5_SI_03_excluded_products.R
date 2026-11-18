# Figure 5 SI 03: direction of excluded product deviations ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
wb_tolerance <- .10

# Product retention ----
products <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                           "basin_pair_product_status.rds")))
stopifnot(isTRUE(all.equal(attr(products, "wb_tolerance"), wb_tolerance)))
products[, ma_basin := as.character(ma_basin)]
stopifnot(!anyDuplicated(products[, .(ma_basin, dataset)]),
          all(is.finite(products$mean_RE)), all(!is.na(products$retained)))
products[, retained := abs(mean_RE) <= wb_tolerance]
basins <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basins$BasinID <- as.character(basins$BasinID)
stopifnot(!anyDuplicated(basins$BasinID))
summary <- products[, .(
  N_products = .N, N_retained = sum(retained),
  N_below = sum(mean_RE < -wb_tolerance),
  N_above = sum(mean_RE > wb_tolerance)
), ma_basin]
summary[, N_excluded := N_below + N_above]
names <- as.data.table(st_drop_geometry(basins))[, .(ma_basin = BasinID, basin_name = Name)]
summary <- merge(names, summary, by = "ma_basin", all.x = TRUE)
order <- summary[order(is.na(N_retained), -N_retained, as.integer(ma_basin)), ma_basin]
products[, basin_plot := factor(ma_basin, levels = order)]
summary[, basin_plot := factor(ma_basin, levels = order)]
basin_labels <- setNames(paste0(summary$basin_name, " (", summary$ma_basin, ")"), summary$ma_basin)
dataset_order <- products[, .(N_retained = sum(retained)), dataset][
  order(-N_retained, dataset), dataset]
products[, dataset_plot := factor(dataset, levels = rev(dataset_order))]
# The denominator contains excluded products only. A bar spans 100 percentage
# points: the negative share below zero and the positive share above zero.
bars <- melt(summary, id.vars = c("ma_basin", "basin_plot", "N_excluded"),
              measure.vars = c("N_below", "N_above"),
              variable.name = "direction", value.name = "N_products")
bars[, share := fifelse(N_excluded > 0, N_products / N_excluded, NA_real_)]
bars[direction == "N_below", share := -share]
bars[, direction := factor(direction, levels = c("N_below", "N_above"),
                            labels = c("Below -10%", "Above +10%"))]
fig_excluded <- ggplot(bars[is.finite(share)], aes(basin_plot, share, fill = direction)) +
  geom_col(width = .8) + geom_hline(yintercept = 0, colour = "gray25", linewidth = .3) +
  geom_text(data = summary[N_excluded == 0], aes(basin_plot, 0, label = "0"),
             inherit.aes = FALSE, colour = "gray45", size = 2.5) +
  scale_fill_manual(values = c("Below -10%" = fig5_col_low, "Above +10%" = fig5_col_high),
                     drop = FALSE, name = NULL) +
  scale_x_discrete(labels = basin_labels, drop = FALSE) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25),
    labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  labs(x = "Basin", y = "Share of excluded products",
       title = "Direction of water-balance deviations among excluded products") +
  theme_fig5 + theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
                     axis.text.x = element_text(angle = 60, hjust = 1, size = 7))
save_manuscript_figure(fig_excluded, file.path(paths$si, "fig5_SI_03_excluded_products"),
                       c(figure_widths, 17))
fwrite(summary, file.path(paths$data, "fig5_excluded_product_counts.csv"))
