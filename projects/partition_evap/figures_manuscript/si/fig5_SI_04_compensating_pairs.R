# Figure 5 SI 04: frequency of exception pairs ----
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
gap_threshold_pp <- 10
compensation_threshold <- .50

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
pairs <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds")))
stopifnot(isTRUE(all.equal(attr(pairs, "wb_tolerance"), wb_tolerance)))
pairs[, ma_basin := as.character(ma_basin)]
pairs <- pairs[both_retained %in% TRUE]
pairs[, aggregated_difference := fifelse(is.finite(net_difference) &
  is.finite(pair_mean_et) & pair_mean_et > 0, net_difference / pair_mean_et, NA_real_)]
pairs[, gap_pp := 100 * (median_sym_rel_diff - aggregated_difference)]
pairs[, assessable := is.finite(gap_pp) & is.finite(compensation)]
pairs[, exception := assessable & gap_pp >= gap_threshold_pp - 100 * .Machine$double.eps &
                      compensation > compensation_threshold]
counts <- pairs[, .(N_assessable = sum(assessable), N_exceptions = sum(exception)), ma_basin]
counts[, proportion := fifelse(N_assessable > 0, N_exceptions / N_assessable, NA_real_)]
summary <- merge(summary, counts, by = "ma_basin", all.x = TRUE)
map <- merge(basins, summary, by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
labels <- c("0%", ">0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%", "Not assessable")
map$class <- as.character(cut(map$proportion, c(-Inf, 0, .2, .4, .6, .8, 1),
                               labels = labels[1:6]))
map$class[is.na(map$class)] <- "Not assessable"
map$class <- factor(map$class, levels = labels)
colours <- setNames(c("gray95", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#6A51A3", "gray70"), labels)
figure <- ggplot() +
  geom_sf(data = world_no_antarctica, fill = "gray95", colour = "gray80", linewidth = .2) +
  geom_sf(data = map, aes(fill = class), colour = "gray45", linewidth = .25) +
  scale_fill_manual(values = colours, limits = labels, drop = FALSE,
    name = "Retained pairs with\nlarge gap and compensation", guide = guide_legend(nrow = 2)) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) + labs(x = NULL, y = NULL) +
  theme_map_fig5 + theme(legend.position = "bottom")
save_manuscript_figure(figure, file.path(paths$si, "fig5_SI_04_compensating_pairs"),
                       c(figure_widths, 16))
fwrite(summary, file.path(paths$data, "fig5_compensating_pair_counts.csv"))
