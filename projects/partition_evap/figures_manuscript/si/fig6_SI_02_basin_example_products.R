# Figure 6 SI 02: relative differences and mean ET for the selected basin pairs ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
example_file <- "projects/partition_evap/figures_manuscript/basin_examples.csv"
excluded_example_products <- "etmonitor"
cell_size <- 0.25
map_padding <- 0.5
relative_breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, Inf)
relative_labels <- c("0-10%", ">10-20%", ">20-30%", ">30-40%", ">40-50%", ">50%")
relative_cols <- setNames(c("#F2F2F2", "#BDD7E7", "#6BAED6",
                            "#3182BD", "#08519C", "#08306B"), relative_labels)

examples <- fread(example_file, colClasses = c(ma_basin = "character"))
stopifnot(!any(examples$product_a %in% excluded_example_products),
          !any(examples$product_b %in% excluded_example_products))
pair_summary <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                                "basin_product_pair_summary.rds")))
# Select the requested product pairs ----
examples <- copy(examples)
pair_summary <- copy(pair_summary)
examples[, ma_basin := as.character(ma_basin)]
pair_summary[, ma_basin := as.character(ma_basin)]
examples[, `:=`(product_1 = pmin(product_a, product_b), product_2 = pmax(product_a, product_b))]
pair_summary[, `:=`(product_1 = pmin(as.character(dataset.x), as.character(dataset.y)),
              product_2 = pmax(as.character(dataset.x), as.character(dataset.y)))]
stopifnot(!anyDuplicated(pair_summary[, .(ma_basin, product_1, product_2)]),
          !anyDuplicated(examples[, .(ma_basin, product_1, product_2)]),
          all(examples$product_a != examples$product_b))
selected <- merge(examples, pair_summary, by = c("ma_basin", "product_1", "product_2"), all.x = TRUE)
stopifnot(nrow(selected) == nrow(examples), all(selected$both_retained %in% TRUE),
          all(!is.na(selected$dataset.x)), all(!is.na(selected$dataset.y)))
selected[, aggregated_sym_rel_diff := fifelse(
  is.finite(net_difference) & is.finite(pair_mean_et) & pair_mean_et > 0,
  net_difference / pair_mean_et, NA_real_)]
setorder(selected, panel_order)
selected_pairs <- selected
# Grid data ----
# Load the global pairwise file once, selecting only the required cells/pairs.
agreement_grid <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"
)))
agreement_grid[, ma_basin := as.character(ma_basin)]
grid_lookup <- agreement_grid[
  ma_basin %in% selected_pairs$ma_basin & is.finite(area) & area > 0,
  .(lon, lat, ma_basin)
]
stopifnot(!anyDuplicated(grid_lookup[, .(lon, lat)]))
query_grid <- merge(
  grid_lookup, selected_pairs[, .(ma_basin, dataset.x, dataset.y)],
  by = "ma_basin", allow.cartesian = TRUE
)
diff_pp <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"
)))
if (!identical(attr(diff_pp, "sym_rel_diff_definition"), "cellwise_pair_mean") ||
    !all(c("evap_mean.x", "evap_mean.y") %in% names(diff_pp))) {
  stop("Regenerate product_pair_difference_gridwise.rds with the current 07f script.")
}
pair_grid <- diff_pp[
  query_grid, on = .(lon, lat, dataset.x, dataset.y), nomatch = 0,
  .(ma_basin = i.ma_basin, lon, lat, dataset.x, dataset.y,
    evap_mean.x, evap_mean.y, et_delta, abs_et_delta, pair_mean_et, sym_rel_diff)
]
rm(diff_pp, query_grid, agreement_grid)
pair_grid <- pair_grid[
  is.finite(evap_mean.x) & is.finite(evap_mean.y) &
    is.finite(et_delta) & is.finite(abs_et_delta) & is.finite(pair_mean_et)
]
stopifnot(!anyDuplicated(pair_grid[
  , .(ma_basin, lon, lat, dataset.x, dataset.y)
]))
mapped_pairs <- unique(pair_grid[, .(ma_basin, dataset.x, dataset.y)])
stopifnot(nrow(mapped_pairs) == nrow(selected_pairs))

basins <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basins <- st_transform(basins, 4326)
basins$BasinID <- as.character(basins$BasinID)
stopifnot(!anyDuplicated(basins$BasinID), all(selected_pairs$ma_basin %in% basins$BasinID))
# Six basins in columns; relative difference, product A and product B in rows.
# Mean ET uses one colour scale across both products and every basin.
et_limits <- c(0, ceiling(max(pair_grid$evap_mean.x, pair_grid$evap_mean.y) / 100) * 100)
stopifnot(et_limits[2] > 0)
plots <- list()
legends <- list()
for (metric in c("relative", "product_a", "product_b")) {
  for (i in seq_len(nrow(selected_pairs))) {
    pair_info <- selected_pairs[i]
    pair <- copy(pair_grid[ma_basin == pair_info$ma_basin &
      dataset.x == pair_info$dataset.x & dataset.y == pair_info$dataset.y])
    stopifnot(nrow(pair) > 0)
    # A/B follows the requested pair, even when storage has the reverse order.
    a_is_x <- as.character(pair_info$product_a) == as.character(pair_info$dataset.x)
    if (metric == "relative") {
      pair[, value := cut(sym_rel_diff, breaks = relative_breaks,
                          labels = relative_labels, include.lowest = TRUE)]
      title <- paste0(pair_info$basin_name, " (", pair_info$ma_basin, ")")
      subtitle <- "Symmetric relative difference"
    } else if (metric == "product_a") {
      pair[, value := if (a_is_x) evap_mean.x else evap_mean.y]
      title <- NULL
      subtitle <- paste0("A: ", pair_info$product_a)
    } else {
      pair[, value := if (a_is_x) evap_mean.y else evap_mean.x]
      title <- NULL
      subtitle <- paste0("B: ", pair_info$product_b)
    }
    basin <- basins[basins$BasinID == pair_info$ma_basin, ]
    bounds <- st_bbox(basin)
    x_limits <- c(bounds[["xmin"]], bounds[["xmax"]]) + c(-1, 1) * map_padding
    y_limits <- c(bounds[["ymin"]], bounds[["ymax"]]) + c(-1, 1) * map_padding
    plot <- ggplot() +
      geom_tile(data = pair, aes(lon, lat, fill = value),
        width = cell_size, height = cell_size) +
      geom_sf(data = basin, fill = NA, colour = "gray20", linewidth = .4) +
      coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
      theme_map_fig5 + theme(legend.position = "bottom",
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9), axis.text = element_text(size = 7),
        legend.title = element_text(size = 10), legend.text = element_text(size = 9))
    if (metric == "relative") {
      plot <- plot + scale_fill_manual(values = relative_cols, limits = relative_labels,
        drop = FALSE, na.translate = FALSE, na.value = "gray85",
        name = "Symmetric relative difference",
        guide = guide_legend(nrow = 1, title.position = "top", title.hjust = .5))
    } else {
      plot <- plot + scale_fill_viridis_c(limits = et_limits, na.value = "gray85",
        name = "Mean ET (mm/year)",
        guide = guide_colorbar(barwidth = unit(12, "cm"), barheight = unit(.4, "cm"),
          title.position = "top", title.hjust = .5))
    }
    if (i == 1 && metric != "product_b") legends[[metric]] <- bottom_legend(plot)
    plots[[length(plots) + 1L]] <- plot + theme(legend.position = "none")
  }
}
body <- ggarrange(plotlist = plots, ncol = nrow(selected_pairs), nrow = 3,
                  labels = c(letters[seq_len(nrow(selected_pairs))],
                             rep("", 2L * nrow(selected_pairs))), align = "hv")
figure <- cowplot::plot_grid(body, cowplot::ggdraw(legends$relative),
  cowplot::ggdraw(legends$product_a), ncol = 1, rel_heights = c(1, .05, .05))
save_manuscript_figure(figure, file.path(paths$si, "fig6_SI_02_basin_example_products"), c(48, 30))
