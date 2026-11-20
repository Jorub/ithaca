# Figure 6: signed ET differences for six selected basin pairs ----
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
excluded_example_products <- "etmonitor" # Selection only; no global ensemble change.

# Map settings ----
cell_size <- 0.25
map_padding <- 0.5
et_breaks <- c(0, 10, 50, 100, 200, 500, Inf)
et_signed_labels <- c("<-500", "-500 to <-200", "-200 to <-100",
                       "-100 to <-50", "-50 to <-10", "-10 to 10",
                       ">10-50", ">50-100", ">100-200", ">200-500", ">500")
et_signed_cols <- setNames(c(
  "#7F2704", "#A63603", "#D94801", "#F16913", "#FDAE6B", "#F2F2F2",
  "#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#08306B"
), et_signed_labels)
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
# Signed difference maps ----
metric <- "signed"
plots <- list()
for (i in seq_len(nrow(selected_pairs))) {
  pair_info <- selected_pairs[i]
  pair <- copy(pair_grid[ma_basin == pair_info$ma_basin &
    dataset.x == pair_info$dataset.x & dataset.y == pair_info$dataset.y])
  stopifnot(nrow(pair) > 0)
  # Selection is unordered; the requested product A fixes the subtraction sign.
  if (as.character(pair_info$product_a) != as.character(pair_info$dataset.x)) {
    pair[, et_delta := -et_delta]
  }
  magnitude_class <- cut(abs(pair$et_delta), breaks = et_breaks,
                          labels = FALSE, include.lowest = TRUE)
  pair[, value := factor(6L + sign(et_delta) * (magnitude_class - 1L),
    levels = seq_along(et_signed_labels), labels = et_signed_labels)]
  basin <- basins[basins$BasinID == pair_info$ma_basin, ]
  bounds <- st_bbox(basin)
  x_limits <- c(bounds[["xmin"]], bounds[["xmax"]]) + c(-1, 1) * map_padding
  y_limits <- c(bounds[["ymin"]], bounds[["ymax"]]) + c(-1, 1) * map_padding
  plots[[i]] <- ggplot() +
    geom_tile(data = pair, aes(lon, lat, fill = value),
      width = cell_size, height = cell_size) +
    geom_sf(data = basin, fill = NA, colour = "gray20", linewidth = .4) +
    coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    scale_fill_manual(values = et_signed_cols, drop = TRUE, na.translate = FALSE,
      na.value = "gray85", name = "ET difference A - B (mm/year)") +
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
}

# Common legend includes classes present in any selected basin.
present <- unique(unlist(lapply(plots, function(p) as.character(p$layers[[1]]$data$value))))
present <- names(et_signed_cols)[names(et_signed_cols) %in% present]
legend <- bottom_legend(plots[[1]] + scale_fill_manual(
  values = et_signed_cols, limits = present, drop = FALSE, na.translate = FALSE,
  name = "ET difference A - B (mm/year)",
  guide = guide_legend(nrow = 2, byrow = TRUE)))
body <- ggarrange(plotlist = lapply(plots, function(p) p + theme(legend.position = "none")),
                  ncol = 3, nrow = ceiling(length(plots) / 3),
                  labels = letters[seq_along(plots)], align = "hv")
figure <- cowplot::plot_grid(body, cowplot::ggdraw(legend), ncol = 1, rel_heights = c(1, .08))
save_manuscript_figure(figure, file.path(paths$main, "fig6_basin_examples"),
                       c(figure_widths, 26))
fwrite(selected_pairs, file.path(paths$data, "fig6_selected_basin_pairs.csv"))
