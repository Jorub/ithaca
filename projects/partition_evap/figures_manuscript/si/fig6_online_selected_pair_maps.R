# Figure 6 online series: individual exception-pair maps ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
case_basin_ids <- character(0)
excluded_example_products <- "etmonitor"

# Map settings ----
cell_size <- 0.25
map_padding <- 0.5
et_breaks <- c(0, 10, 50, 100, 200, 500, Inf)
et_absolute_labels <- c("0-10", ">10-50", ">50-100", ">100-200",
                         ">200-500", ">500")
et_signed_labels <- c("<-500", "-500 to <-200", "-200 to <-100",
                       "-100 to <-50", "-50 to <-10", "-10 to 10",
                       ">10-50", ">50-100", ">100-200", ">200-500", ">500")
et_signed_cols <- setNames(c(
  "#7F2704", "#A63603", "#D94801", "#F16913", "#FDAE6B", "#F2F2F2",
  "#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#08306B"
), et_signed_labels)
et_absolute_cols <- setNames(c(
  "#F2F2F2", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#08306B"
), et_absolute_labels)
relative_breaks <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, Inf)
relative_labels <- c("0-10%", ">10-20%", ">20-30%", ">30-40%",
                      ">40-50%", ">50%")
relative_cols <- setNames(c(
  "#F2F2F2", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#08306B"
), relative_labels)

selected_pairs <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                                   "basin_compensating_pairs_by_gap.rds")))
selected_pairs[, ma_basin := as.character(ma_basin)]
selected_pairs <- selected_pairs[!dataset.x %in% excluded_example_products &
                                  !dataset.y %in% excluded_example_products]
if (length(case_basin_ids)) selected_pairs <- selected_pairs[ma_basin %in% case_basin_ids]
selected_pairs[, `:=`(product_a = as.character(dataset.x), product_b = as.character(dataset.y))]
stopifnot(all(selected_pairs$both_retained %in% TRUE))
if (!nrow(selected_pairs)) {
  message("No selected product pairs to plot.")
} else {
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
  for (i in seq_len(nrow(selected_pairs))) {
    pair_info <- selected_pairs[i]
    plots <- list()
    for (metric in c("signed", "relative")) {
      stopifnot(metric %in% c("signed", "relative"), nrow(pair_info) == 1)
      pair <- copy(pair_grid[ma_basin == pair_info$ma_basin &
                                dataset.x == pair_info$dataset.x & dataset.y == pair_info$dataset.y])
      stopifnot(nrow(pair) > 0)
      # Selection is unordered; the requested product A fixes the subtraction sign.
      if (as.character(pair_info$product_a) != as.character(pair_info$dataset.x)) {
        pair[, et_delta := -et_delta]
      }
      magnitude_class <- cut(abs(pair$et_delta), breaks = et_breaks,
                              labels = FALSE, include.lowest = TRUE)
      pair[, signed_class := factor(6L + sign(et_delta) * (magnitude_class - 1L),
        levels = seq_along(et_signed_labels), labels = et_signed_labels)]
      pair[, relative_class := cut(sym_rel_diff, breaks = relative_breaks,
                                    labels = relative_labels, include.lowest = TRUE)]
      basin <- basins[basins$BasinID == pair_info$ma_basin, ]
      bounds <- st_bbox(basin)
      x_limits <- c(bounds[["xmin"]], bounds[["xmax"]]) + c(-1, 1) * map_padding
      y_limits <- c(bounds[["ymin"]], bounds[["ymax"]]) + c(-1, 1) * map_padding
      if (metric == "signed") {
        pair[, value := signed_class]
        colours <- et_signed_cols
        legend_title <- "ET difference A - B (mm/year)"
      } else {
        pair[, value := relative_class]
        colours <- relative_cols
        legend_title <- "Symmetric relative difference"
      }
      plot <- ggplot() +
        geom_tile(data = pair, aes(lon, lat, fill = value),
          width = cell_size, height = cell_size) +
        geom_sf(data = basin, fill = NA, colour = "gray20", linewidth = .4) +
        coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
        scale_fill_manual(values = colours, drop = TRUE, na.translate = FALSE,
          na.value = "gray85", name = legend_title,
          guide = guide_legend(title.position = "top", title.hjust = .5, ncol = 3, byrow = TRUE)) +
        labs(x = NULL, y = NULL,
          title = paste0(pair_info$basin_name, " (", pair_info$ma_basin, ")"),
          subtitle = paste0("A: ", pair_info$product_a, "    B: ", pair_info$product_b),
          caption = sprintf("Basin %.1f%% | Grid median %.1f%% | Area >20%% %.0f%% | C %.0f%%",
            100 * pair_info$aggregated_sym_rel_diff, 100 * pair_info$median_sym_rel_diff,
            100 * pair_info$large_difference_fraction, 100 * pair_info$compensation)) +
        theme_map_fig5 + theme(legend.position = "bottom",
          plot.title = element_text(size = 10, face = "bold"),
          plot.subtitle = element_text(size = 8), plot.caption = element_text(size = 7),
          legend.title = element_text(size = 9), legend.text = element_text(size = 8))
      plots[[metric]] <- plot
    }
    figure <- ggarrange(plots$signed, plots$relative, ncol = 2, align = "hv")
    stem <- paste0("fig6_online_basin_", pair_info$ma_basin, "_", pair_info$product_a,
                    "_vs_", pair_info$product_b, "_differences")
    stem <- gsub("[^A-Za-z0-9_-]", "_", stem)
    save_manuscript_figure(figure, file.path(paths$online, stem), c(figure_widths, 18))
  }
}
