# Figure 6 online series: individual exception-pair maps ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R", local = TRUE)
source("source/partition_evap_graphics.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)
source("projects/partition_evap/figures_manuscript/source/basin_pair_maps.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()
case_basin_ids <- character(0)
excluded_example_products <- "etmonitor"

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
  inputs <- load_pair_map_inputs(selected_pairs)
  for (i in seq_len(nrow(selected_pairs))) {
    pair <- selected_pairs[i]
    signed <- build_basin_pair_map(pair, inputs, "signed")
    relative <- build_basin_pair_map(pair, inputs, "relative")
    figure <- ggarrange(signed, relative, ncol = 2, align = "hv")
    stem <- paste0("fig6_online_basin_", pair$ma_basin, "_", pair$product_a,
                    "_vs_", pair$product_b, "_differences")
    stem <- gsub("[^A-Za-z0-9_-]", "_", stem)
    save_manuscript_figure(figure, file.path(paths$online, stem), c(figure_widths, 18))
  }
}
