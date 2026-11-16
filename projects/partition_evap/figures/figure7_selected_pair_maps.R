# Spatial differences for every product pair selected by 11i ----
library(data.table)
library(sf)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

# Configuration ----
case_basin_ids <- character(0) # All selected basins; specify IDs for a subset.
et_difference_type <- "signed" # A minus B; use "absolute" for |A minus B|.
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
output_folder <- paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                        "exploratory/basin_pairs/selected_pair_maps/")
stopifnot(et_difference_type %in% c("absolute", "signed"))

# Selected pairs ----
# Run 11i first: both products retained, gap >=10 pp and compensation >50%.
# Selection thresholds remain in 11i, rather than being repeated here.
selected_pairs <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_compensating_pairs_by_gap.rds"
)))
selected_pairs[, ma_basin := as.character(ma_basin)]
if (length(case_basin_ids)) {
  selected_pairs <- selected_pairs[ma_basin %in% case_basin_ids]
}
stopifnot(
  !anyDuplicated(selected_pairs[, .(ma_basin, dataset.x, dataset.y)]),
  all(selected_pairs$both_retained == TRUE)
)

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
  pair_grid[, et_difference := if (et_difference_type == "signed") {
    et_delta
  } else abs_et_delta]
  
  # Apply the same discrete classes to every map. Exact thresholds belong
  # to the lower magnitude class, symmetrically for positive/negative values.
  pair_grid[, et_magnitude_class := cut(
    abs(et_difference), breaks = et_breaks, labels = FALSE, include.lowest = TRUE
  )]
  if (et_difference_type == "signed") {
    pair_grid[, et_class := factor(
      6L + sign(et_difference) * (et_magnitude_class - 1L),
      levels = seq_along(et_signed_labels), labels = et_signed_labels
    )]
    et_cols <- et_signed_cols
  } else {
    pair_grid[, et_class := factor(et_magnitude_class,
                                   levels = seq_along(et_absolute_labels), labels = et_absolute_labels)]
    et_cols <- et_absolute_cols
  }
  pair_grid[, relative_class := cut(sym_rel_diff, breaks = relative_breaks,
                                    labels = relative_labels, include.lowest = TRUE)]
  ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
  ma_sf <- st_transform(ma_sf, 4326)
  ma_sf$BasinID <- as.character(ma_sf$BasinID)
  stopifnot(all(selected_pairs$ma_basin %in% ma_sf$BasinID))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  map_theme <- theme_map_fig5 + theme(
    legend.position = "bottom", legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 11, face = "bold")
  )
  
  # One two-panel PDF and PNG per basin/product-pair combination ----
  for (pair_index in seq_len(nrow(selected_pairs))) {
    pair_info <- selected_pairs[pair_index]
    basin_id <- pair_info$ma_basin
    product_x <- pair_info$dataset.x
    product_y <- pair_info$dataset.y
    pair <- pair_grid[ma_basin == basin_id &
                        dataset.x == product_x & dataset.y == product_y]
    basin <- ma_sf[ma_sf$BasinID == basin_id, ]
    bounds <- st_bbox(basin)
    x_limits <- c(bounds[["xmin"]], bounds[["xmax"]]) +
      c(-map_padding, map_padding)
    y_limits <- c(bounds[["ymin"]], bounds[["ymax"]]) +
      c(-map_padding, map_padding)
    
    fig_et <- ggplot() +
      geom_tile(data = pair, aes(lon, lat, fill = et_class),
                width = cell_size, height = cell_size) +
      geom_sf(data = basin, fill = NA, colour = "gray20", linewidth = 0.5) +
      coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      labs(x = NULL, y = NULL, title = if (et_difference_type == "signed") {
        "a  Signed ET difference (A - B)"
      } else "a  Absolute ET difference |A - B|") +
      map_theme
    if (et_difference_type == "signed") {
      fig_et <- fig_et + scale_fill_manual(
        values = et_cols, drop = TRUE, na.translate = FALSE,
        na.value = "gray85",
        name = "ET difference (mm/year)",
        guide = guide_legend(title.position = "top", title.hjust = 0.5,
                             ncol = 3, byrow = TRUE)
      )
    } else {
      fig_et <- fig_et + scale_fill_manual(
        values = et_cols, drop = TRUE, na.translate = FALSE,
        na.value = "gray85",
        name = "Absolute ET difference (mm/year)",
        guide = guide_legend(title.position = "top", title.hjust = 0.5,
                             ncol = 3, byrow = TRUE)
      )
    }
    fig_relative <- ggplot() +
      geom_tile(data = pair[is.finite(sym_rel_diff)],
                aes(lon, lat, fill = relative_class),
                width = cell_size, height = cell_size) +
      geom_sf(data = basin, fill = NA, colour = "gray20", linewidth = 0.5) +
      scale_fill_manual(
        values = relative_cols, drop = TRUE, na.translate = FALSE,
        na.value = "gray85",
        name = "Symmetric relative difference",
        guide = guide_legend(title.position = "top", title.hjust = 0.5,
                             ncol = 3, byrow = TRUE)
      ) +
      coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
      labs(x = NULL, y = NULL, title = "b  Symmetric relative difference") +
      map_theme
    
    fig_pair <- ggarrange(fig_et, fig_relative, ncol = 2, align = "hv")
    fig_pair <- annotate_figure(
      fig_pair,
      top = text_grob(paste0(pair_info$basin_name, " (", basin_id, ")\n",
                             "A: ", product_x, "    B: ", product_y),
                      size = 12, face = "bold"),
      bottom = text_grob(sprintf(
        "Basin difference: %.1f%%; grid-cell median: %.1f%%; compensation: %.0f%%",
        100 * pair_info$aggregated_sym_rel_diff,
        100 * pair_info$median_sym_rel_diff, 100 * pair_info$compensation
      ), size = 9)
    )
    pair_name <- gsub("[^A-Za-z0-9_-]", "_", c(product_x, product_y))
    basename_out <- paste0("basin_", basin_id, "_", pair_name[1], "_vs_",
                           pair_name[2], "_", et_difference_type, "_maps")
    ggsave(file.path(output_folder, paste0(basename_out, ".pdf")), fig_pair,
           width = figure_widths, height = 18, units = "cm",
           device = cairo_pdf, bg = "white")
    ggsave(file.path(output_folder, paste0(basename_out, ".png")), fig_pair,
           width = figure_widths, height = 18, units = "cm", dpi = 300, bg = "white")
  }
}
