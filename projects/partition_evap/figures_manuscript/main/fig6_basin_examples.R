# Figure 6: nine provisional basin-pair examples ----
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
example_file <- "projects/partition_evap/figures_manuscript/basin_examples.csv"
excluded_example_products <- "etmonitor" # Selection only; no global ensemble change.

examples <- fread(example_file, colClasses = c(ma_basin = "character"))
stopifnot(!any(examples$product_a %in% excluded_example_products),
          !any(examples$product_b %in% excluded_example_products))
pair_summary <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                                "basin_product_pair_summary.rds")))
selected_pairs <- select_example_pairs(examples, pair_summary)
inputs <- load_pair_map_inputs(selected_pairs) # Read the global grid file once.
for (metric in c("signed", "relative")) {
  plots <- lapply(seq_len(nrow(selected_pairs)), function(i) {
    build_basin_pair_map(selected_pairs[i], inputs, metric)
  })
  figure <- assemble_pair_map_board(plots, inputs, metric)
  save_manuscript_figure(figure, file.path(paths$main, paste0("fig6_basin_examples_", metric)),
                         c(figure_widths, 36))
}
fwrite(selected_pairs, file.path(paths$data, "fig6_selected_basin_pairs.csv"))
