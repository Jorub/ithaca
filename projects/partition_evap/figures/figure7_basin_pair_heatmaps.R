# Exploratory basin heatmaps: aggregated and grid-cell pairwise differences ----
library(data.table)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

# Configuration ----
case_basin_ids <- character(0) # All prepared basins; specify IDs for a subset.
output_folder <- paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                        "exploratory/basin_pairs/")
show_values <- TRUE
large_difference_threshold <- 0.20

# Data ----
# First run 11g_basin_product_pair_comparison.R for the selected basins.
pair_summary <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds"
)))
product_status <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_pair_product_status.rds"
)))
pair_summary[, ma_basin := as.character(ma_basin)]
product_status[, ma_basin := as.character(ma_basin)]
if (!length(case_basin_ids)) case_basin_ids <- unique(product_status$ma_basin)
stopifnot(
  all(case_basin_ids %in% product_status$ma_basin),
  !anyDuplicated(product_status[, .(ma_basin, dataset)]),
  !anyDuplicated(pair_summary[, .(ma_basin, dataset.x, dataset.y)]),
  identical(attr(pair_summary, "wb_tolerance"),
            attr(product_status, "wb_tolerance"))
)
wb_tolerance <- attr(product_status, "wb_tolerance")
stopifnot(length(wb_tolerance) == 1, is.finite(wb_tolerance))
if (!"large_difference_fraction" %in% names(pair_summary) ||
    !isTRUE(all.equal(attr(pair_summary, "large_difference_threshold"),
                      large_difference_threshold))) {
  stop("Rerun 11g to calculate area with pairwise difference >20%.")
}
# Compare basin means on the same spatial support and period used for C.
pair_summary[, aggregated_sym_rel_diff := fifelse(
  is.finite(net_difference) & is.finite(pair_mean_et) & pair_mean_et > 0,
  net_difference / pair_mean_et, NA_real_
)]
# Use the same difference scale for both panels and across all plotted basins.
difference_limit <- max(c(0.20, pair_summary[
  ma_basin %in% case_basin_ids, c(aggregated_sym_rel_diff, median_sym_rel_diff)
]), na.rm = TRUE)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Four matching heatmaps for each basin ----
for (basin_id in case_basin_ids) {
  status <- copy(product_status[ma_basin == basin_id])
  setorder(status, -retained, dataset_type, dataset)
  dataset_order <- status$dataset
  retained_products <- status[retained == TRUE, dataset]
  dataset_labels <- setNames(
    paste0(status$dataset, fifelse(status$retained, " *", "")), status$dataset
  )
  pairs <- pair_summary[ma_basin == basin_id, .(
    dataset.x, dataset.y, aggregated_sym_rel_diff, median_sym_rel_diff,
    large_difference_fraction, compensation
  )]
  symmetric_pairs <- rbind(
    pairs,
    pairs[, .(dataset.x = dataset.y, dataset.y = dataset.x,
              aggregated_sym_rel_diff, median_sym_rel_diff,
              large_difference_fraction, compensation)]
  )
  stopifnot(!anyDuplicated(symmetric_pairs[, .(dataset.x, dataset.y)]))
  pair_matrix <- merge(
    CJ(dataset.x = dataset_order, dataset.y = dataset_order), symmetric_pairs,
    by = c("dataset.x", "dataset.y"), all.x = TRUE
  )
  pair_matrix[, both_retained := dataset.x %in% retained_products &
                dataset.y %in% retained_products & dataset.x != dataset.y]
  pair_matrix[, `:=`(
    aggregated_label = fifelse(is.finite(aggregated_sym_rel_diff),
                               scales::percent(aggregated_sym_rel_diff, accuracy = 0.1), ""),
    difference_label = fifelse(is.finite(median_sym_rel_diff),
                               scales::percent(median_sym_rel_diff, accuracy = 1), ""),
    area_label = fifelse(is.finite(large_difference_fraction),
                         scales::percent(large_difference_fraction, accuracy = 1), ""),
    compensation_label = fifelse(is.finite(compensation),
                                 scales::percent(compensation, accuracy = 1), "")
  )]
  pair_matrix[, dataset.x := factor(dataset.x, levels = dataset_order)]
  pair_matrix[, dataset.y := factor(dataset.y, levels = rev(dataset_order))]
  off_diagonal <- pair_matrix[as.character(dataset.x) != as.character(dataset.y)]
  
  pair_theme <- theme_fig5 + theme(
    panel.grid = element_blank(), legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 9), legend.text = element_text(size = 8),
    plot.title = element_text(size = 11, face = "bold")
  )
  
  fig_aggregated <- ggplot(off_diagonal, aes(dataset.x, dataset.y)) +
    geom_tile(aes(fill = aggregated_sym_rel_diff), colour = "white", linewidth = 0.25) +
    geom_tile(data = off_diagonal[both_retained == TRUE],
              fill = NA, colour = "gray20", linewidth = 0.7) +
    scale_fill_gradient(
      low = "#F7FBFF", high = "#2171B5", limits = c(0, difference_limit),
      labels = scales::label_percent(accuracy = 1), na.value = "gray85",
      name = "Basin-aggregated\npairwise difference",
      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                              barwidth = unit(5, "cm"), barheight = unit(0.3, "cm"))
    ) +
    scale_x_discrete(limits = dataset_order, labels = dataset_labels, drop = FALSE) +
    scale_y_discrete(limits = rev(dataset_order), labels = dataset_labels, drop = FALSE) +
    coord_equal() +
    labs(x = NULL, y = NULL, title = "a  Basin-aggregated differences") +
    pair_theme
  fig_difference <- ggplot(off_diagonal, aes(dataset.x, dataset.y)) +
    geom_tile(aes(fill = median_sym_rel_diff), colour = "white", linewidth = 0.25) +
    geom_tile(data = off_diagonal[both_retained == TRUE],
              fill = NA, colour = "gray20", linewidth = 0.7) +
    scale_fill_gradient(
      low = "#F7FBFF", high = "#2171B5", limits = c(0, difference_limit),
      labels = scales::label_percent(accuracy = 1), na.value = "gray85",
      name = "Median grid-cell\npairwise difference",
      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                              barwidth = unit(5, "cm"), barheight = unit(0.3, "cm"))
    ) +
    scale_x_discrete(limits = dataset_order, labels = dataset_labels, drop = FALSE) +
    scale_y_discrete(limits = rev(dataset_order), labels = dataset_labels, drop = FALSE) +
    coord_equal() +
    labs(x = NULL, y = NULL, title = "b  Median grid-cell differences") +
    pair_theme
  fig_area <- ggplot(off_diagonal, aes(dataset.x, dataset.y)) +
    geom_tile(aes(fill = large_difference_fraction), colour = "white", linewidth = 0.25) +
    geom_tile(data = off_diagonal[both_retained == TRUE],
              fill = NA, colour = "gray20", linewidth = 0.7) +
    scale_fill_gradient(
      low = "#FFF7EC", high = "#D95F0E", limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1), na.value = "gray85",
      name = "Area with pairwise\ndifference >20%",
      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                              barwidth = unit(5, "cm"), barheight = unit(0.3, "cm"))
    ) +
    scale_x_discrete(limits = dataset_order, labels = dataset_labels, drop = FALSE) +
    scale_y_discrete(limits = rev(dataset_order), labels = dataset_labels, drop = FALSE) +
    coord_equal() +
    labs(x = NULL, y = NULL, title = "c  Area with pairwise difference >20%") +
    pair_theme
  fig_compensation <- ggplot(off_diagonal, aes(dataset.x, dataset.y)) +
    geom_tile(aes(fill = compensation), colour = "white", linewidth = 0.25) +
    geom_tile(data = off_diagonal[both_retained == TRUE],
              fill = NA, colour = "gray20", linewidth = 0.7) +
    scale_fill_gradient(
      low = "#FCFBFD", high = "#756BB1", limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1), na.value = "gray85",
      name = "Spatial compensation\nCancelled gross difference",
      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                              barwidth = unit(5, "cm"), barheight = unit(0.3, "cm"))
    ) +
    scale_x_discrete(limits = dataset_order, labels = dataset_labels, drop = FALSE) +
    scale_y_discrete(limits = rev(dataset_order), labels = dataset_labels, drop = FALSE) +
    coord_equal() +
    labs(x = NULL, y = NULL, title = "d  Spatial compensation") +
    pair_theme
  if (show_values) {
    fig_aggregated <- fig_aggregated +
      geom_text(aes(label = aggregated_label), size = 2.2)
    fig_difference <- fig_difference +
      geom_text(aes(label = difference_label), size = 2.2)
    fig_area <- fig_area + geom_text(aes(label = area_label), size = 2.2)
    fig_compensation <- fig_compensation +
      geom_text(aes(label = compensation_label), size = 2.2)
  }
  fig_pairs <- ggarrange(fig_aggregated, fig_difference, fig_area, fig_compensation,
                         ncol = 2, nrow = 2, align = "hv")
  fig_pairs <- annotate_figure(
    fig_pairs,
    top = text_grob(paste0(first(status$basin_name), " (", basin_id, ") — ",
                           length(retained_products), " products retained"),
                    size = 12, face = "bold"),
    bottom = text_grob(paste0(
      "* Product within ±", 100 * wb_tolerance,
      "% of water balance. Outlines: both products retained. Grey: unavailable or undefined."
    ), size = 9)
  )
  
  # Save exploratory outputs; final main/SI placement is still to be decided.
  basename_out <- paste0("basin_", basin_id, "_pair_heatmaps")
  ggsave(file.path(output_folder, paste0(basename_out, ".pdf")), fig_pairs,
         width = figure_widths, height = 39, units = "cm",
         device = cairo_pdf, bg = "white")
  ggsave(file.path(output_folder, paste0(basename_out, ".png")), fig_pairs,
         width = figure_widths, height = 39, units = "cm", dpi = 300, bg = "white")
}
