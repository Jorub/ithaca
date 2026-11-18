# Figure 5 SI 01: full versus filtered pairwise differences ----
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
# Both panels take the median across per-pair summaries from 11g.

pairs <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds")))
products <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "basin_pair_product_status.rds")))
stopifnot(isTRUE(all.equal(attr(products, "wb_tolerance"), wb_tolerance)),
          isTRUE(all.equal(attr(pairs, "wb_tolerance"), wb_tolerance)))
# Summarise full and retained pairs ----
pairs <- copy(pairs)
pairs[, ma_basin := as.character(ma_basin)]
products <- copy(products)
products[, ma_basin := as.character(ma_basin)]
stopifnot(!anyDuplicated(products[, .(ma_basin, dataset)]),
          !anyDuplicated(pairs[, .(ma_basin, dataset.x, dataset.y)]))
pairs[, aggregated_difference := fifelse(
  is.finite(net_difference) & is.finite(pair_mean_et) & pair_mean_et > 0,
  net_difference / pair_mean_et, NA_real_)]
summary <- products[, .(N_products = .N, N_retained = sum(retained)), ma_basin]
aggregate <- pairs[is.finite(aggregated_difference), .(
  aggregated_full = median(aggregated_difference),
  aggregated_retained = if (any(both_retained %in% TRUE)) {
    median(aggregated_difference[both_retained %in% TRUE])
  } else NA_real_
), ma_basin]
local <- pairs[is.finite(median_sym_rel_diff), .(
  grid_full = median(median_sym_rel_diff),
  grid_retained = if (any(both_retained %in% TRUE)) {
    median(median_sym_rel_diff[both_retained %in% TRUE])
  } else NA_real_
), ma_basin]
definition <- "median across per-pair area-weighted grid-cell medians"
summary <- merge(summary, local, by = "ma_basin", all.x = TRUE)
summary <- merge(summary, aggregate, by = "ma_basin", all.x = TRUE)
summary[N_retained < 2, c("grid_full", "grid_retained", "aggregated_full",
                          "aggregated_retained") := NA_real_]
summary[, `:=`(grid_change_pp = 100 * (grid_retained - grid_full),
                aggregated_change_pp = 100 * (aggregated_retained - aggregated_full))]
attr(summary, "grid_summary_definition") <- definition
attr(summary, "aggregated_summary_definition") <-
  "median across pairwise basin-aggregated symmetric relative differences"
attr(summary, "change_definition") <- "filtered minus full, percentage points"
basins <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basins$BasinID <- as.character(basins$BasinID)
map <- merge(basins, summary, by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
# Map panels ----
map_titles <- c(grid_change_pp = "a  Median grid-cell differences",
                aggregated_change_pp = "b  Basin-aggregated differences")
plots <- list()
for (metric in names(map_titles)) {
  title <- map_titles[[metric]]
  labels <- c("Decrease >=10 pp", "Decrease 5-10 pp", "Within +/-5 pp",
               "Increase 5-10 pp", "Increase >10 pp", "One retained product", "Unavailable")
  map$class <- as.character(cut(map[[metric]], c(-Inf, -10, -5, 5, 10, Inf),
                                labels = labels[1:5], include.lowest = TRUE))
  map$class[is.na(map$class)] <- "Unavailable"
  map$class[!is.na(map$N_retained) & map$N_retained == 1] <- "One retained product"
  map$class <- factor(map$class, levels = labels)
  colours <- setNames(c("#2166AC", "#67A9CF", "#E5E5E5", "#EF8A62", "#B2182B",
                         "gray35", "gray85"), labels)
  plots[[metric]] <- ggplot() +
    geom_sf(data = world_no_antarctica, fill = "gray95", colour = "gray80", linewidth = .2) +
    geom_sf(data = map, aes(fill = class), colour = "gray45", linewidth = .25) +
    scale_fill_manual(values = colours, limits = labels, drop = FALSE,
      name = "Filtering change\nRelative difference (pp)",
      guide = guide_legend(nrow = 3, byrow = TRUE)) +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    coord_sf(ylim = c(-70, 90), expand = FALSE) +
    labs(x = NULL, y = NULL, title = title) + theme_map_fig5 +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}
local <- plots$grid_change_pp
aggregate <- plots$aggregated_change_pp
legend <- bottom_legend(local)
body <- ggarrange(local + theme(legend.position = "none"),
                   aggregate + theme(legend.position = "none"), ncol = 2, align = "hv")
figure <- cowplot::plot_grid(body, cowplot::ggdraw(legend), ncol = 1, rel_heights = c(1, .2))
save_manuscript_figure(figure, file.path(paths$si, "fig5_SI_01_filtering_change"),
                       c(figure_widths, 15))
saveRDS(summary, file.path(paths$data, "fig5_filtering_change_summary.rds"))
fwrite(summary, file.path(paths$data, "fig5_filtering_change_summary.csv"))
