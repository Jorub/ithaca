# Figure 5 SI 04: frequency of exception pairs ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/basin_retention_panels.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
wb_tolerance <- .10
gap_threshold_pp <- 10
compensation_threshold <- .50

retention <- read_basin_retention(wb_tolerance)
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
summary <- merge(retention$summary, counts, by = "ma_basin", all.x = TRUE)
map <- merge(retention$basins, summary, by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
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
