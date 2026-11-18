# Figure 5: product retention by the basin water-balance filter ----
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
# Retained-product map ----
map <- merge(basins, summary, by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
levels <- c("0", "1", "2-3", "4-5", "6-7", "8+")
colours <- setNames(c("gray95", "#DADAEB", "#BCBDDC", "#9E9AC8", "#756BB1", "#54278F"), levels)
map$class <- cut(map$N_retained, c(-Inf, 0, 1, 3, 5, 7, Inf), labels = levels)
fig_retained_map <- ggplot() +
  geom_sf(data = world_no_antarctica, fill = "gray95", colour = "gray80", linewidth = .2) +
  geom_sf(data = map, aes(fill = class), colour = "gray45", linewidth = .25) +
  scale_fill_manual(values = colours, limits = levels, drop = FALSE, na.value = "gray85",
    name = "Water-balance agreement\nProducts within +/-10%",
    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  labs(x = NULL, y = NULL, title = "a") + theme_map_fig5 +
  theme(legend.position = "left", plot.title = element_text(face = "bold"))

# Product-error heatmap ----
fig_retention_heatmap <- ggplot(products) +
  geom_tile(aes(basin_plot, dataset_plot, fill = mean_RE), colour = "white", linewidth = .2) +
  geom_tile(data = products[retained == TRUE],
    aes(basin_plot, dataset_plot, colour = "Within 10% of water balance"),
    fill = NA, linewidth = .65) +
  facet_grid(dataset_type ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = basin_labels, drop = FALSE) +
  scale_fill_gradient2(low = fig5_col_low, mid = fig5_col_mid, high = fig5_col_high,
    midpoint = 0, limits = c(-.5, .5), oob = scales::squish,
    breaks = c(-.5, -.25, 0, .25, .5), labels = scales::label_percent(accuracy = 1),
    name = "Product relative water-balance error (%)",
    guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(.4, "cm"),
                          title.position = "top", title.hjust = .5)) +
  scale_colour_manual(values = c("Within 10% of water balance" = "gray25"), name = NULL,
    guide = guide_legend(override.aes = list(fill = NA, linewidth = .65))) +
  labs(x = "Basin", y = "Products", title = "b") + theme_fig5 +
  theme(panel.grid = element_blank(), strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom", legend.box = "vertical",
    legend.margin = margin(8, 6, 8, 6), plot.title = element_text(face = "bold"))

# Figure layout ----
fig5 <- ggarrange(fig_retained_map, fig_retention_heatmap,
                  ncol = 1, heights = c(1, 1.5))
save_manuscript_figure(fig5, file.path(paths$main, "fig5_water_balance_filtering"),
                       c(figure_widths, 27))
fwrite(summary, file.path(paths$data, "fig5_product_retention_summary.csv"))
fwrite(products[, .(ma_basin, dataset, mean_RE, retained)],
       file.path(paths$data, "fig5_product_retention_status.csv"))
