# Offline validation: no production data or project configuration is sourced.
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(grid)

root <- Sys.getenv("MANUSCRIPT_FIGURE_ROOT", "projects/partition_evap/figures_manuscript")
repository <- Sys.getenv("ITHACA_REPO_ROOT", ".")
files <- list.files(root, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
stopifnot(length(files) >= 20)
invisible(lapply(files, parse))
for (file in files) {
  lines <- readLines(file, warn = FALSE)
  sources <- lines[grepl('^\\s*source\\("', lines)]
  paths <- sub('.*source\\("([^"]+)".*', '\\1', sources)
  for (path in paths) {
    actual <- if (startsWith(path, "projects/partition_evap/figures_manuscript/")) {
      file.path(root, sub("^projects/partition_evap/figures_manuscript/", "", path))
    } else file.path(repository, path)
    stopifnot(file.exists(actual))
  }
}
examples <- fread(file.path(root, "basin_examples.csv"), colClasses = c(ma_basin = "character"))
stopifnot(nrow(examples) == 9, !anyDuplicated(examples$ma_basin),
          !any(c(examples$product_a, examples$product_b) == "etmonitor"),
          sum(examples$product_a == "jra55" | examples$product_b == "jra55") == 1)

env <- new.env(parent = globalenv())
env$theme_map_fig5 <- env$theme_map_leg <- env$theme_fig5 <- theme_bw()
env$fig5_col_low <- "#D55E00"; env$fig5_col_mid <- "#F7F7F7"
env$fig5_col_high <- "#0072B2"
env$color_agreement <- c(Low = "#A63A3A", "Below average" = "#E38B75",
                         Average = "#F4CC70", "Above average" = "#97B8C2", High = "#4D648D")
# Evaluate only the existing weighted-median definition, not project setup.
definitions <- parse(file.path(repository, "source/partition_evap.R"))
for (definition in definitions) {
  if (is.call(definition) && identical(definition[[1]], as.name("<-")) &&
      identical(definition[[2]], as.name("weighted_median"))) eval(definition, env)
}
stopifnot(is.function(env$weighted_median))
helpers <- c("manuscript_io.R", "scale_agreement_panels.R", "basin_accuracy_panels.R",
              "basin_pair_maps.R", "basin_retention_panels.R", "filtering_overview_panels.R")
for (helper in helpers) sys.source(file.path(root, "source", helper), env)

# Exact majority boundary, missing values and both transition directions.
classes <- env$scale_transition_class(c("High", "High", "Low", "Low", NA),
                                      c(.5, .49, .5, .49, .8), .5)
stopifnot(identical(as.character(classes), c("Persistent", "Emergent", "Lost", "Other", NA)))

# Three synthetic basins: balanced members, shared high deviation and one retained.
ids <- c("20", "29", "56")
names <- c("California", "Danube", "Swan")
polygons <- lapply(seq_along(ids), function(i) {
  x <- 4 * (i - 1)
  st_polygon(list(matrix(c(x, 0, x + 2, 0, x + 2, 2, x, 2, x, 0), ncol = 2, byrow = TRUE)))
})
basins <- st_sf(BasinID = ids, Name = names, geometry = st_sfc(polygons, crs = 4326))
env$world_no_antarctica <- basins
products <- CJ(ma_basin = ids, dataset = c("a", "b", "c", "d"))
products[, mean_evap := c(80, 99, 101, 120, 100, 112, 116, 120, 70, 80, 100, 130)]
products[, `:=`(water_balance = 100, dataset_type = "Reanalysis", continent_abr = "Test",
                 rel_da_higher = .2, dist_da_higher = .3, joint_da_higher = .1, sIQR = .2)]
products[, `:=`(mean_RE = (mean_evap - water_balance) / water_balance,
                 basin_name = names[match(ma_basin, ids)])]
products[, retained := abs(mean_RE) <= .1]
attr(products, "wb_tolerance") <- .1
pair_keys <- rbindlist(lapply(ids, function(id) {
  combinations <- t(combn(c("a", "b", "c", "d"), 2))
  data.table(ma_basin = id, dataset.x = combinations[, 1], dataset.y = combinations[, 2])
}))
pairs <- copy(pair_keys)
pairs[, `:=`(pair_mean_et = 100, net_difference = c(1, 2, 3, 4, 5, 6),
               median_sym_rel_diff = c(.15, .1, .3, .08, .2, .12),
               large_difference_fraction = c(.1, .2, .6, .1, .4, .2),
               compensation = c(.9, .6, .7, .2, .9, .6),
               basin_name = names[match(ma_basin, ids)]), by = ma_basin]
pairs[, both_retained := mapply(function(id, a, b) {
  all(products[ma_basin == id & dataset %in% c(a, b), retained])
}, ma_basin, dataset.x, dataset.y)]
attr(pairs, "wb_tolerance") <- .1
attr(pairs, "large_difference_threshold") <- .2
annual <- products[, .(year = 2000:2019, evap_mean = rep(mean_evap, 20)), .(ma_basin, dataset)]
distribution <- data.table(ma_basin = ids, index = c(0, .4, .8))
fixtures <- list(ma_basins_spatial.rds = basins, ma_basins_comparison.rds = products,
                 interannual_variance_ma_basin.rds = annual,
                 distribution_agreement_index_ma_basin.rds = distribution,
                 basin_pair_product_status.rds = products, basin_product_pair_summary.rds = pairs)
env$PATH_SAVE_PARTITION_EVAP <- "/synthetic/"
env$readRDS <- function(path) {
  value <- fixtures[[basename(path)]]
  if (is.null(value)) stop("Unexpected synthetic read: ", path)
  copy(value)
}
env$agreement_years <- 2000:2019; env$wb_tolerance <- .1
env$excluded_datasets <- character(0)
env$wb_levels <- c("Below -10%", "Within 10%", "Above +10%")
env$wb_cols <- setNames(c("orange", "green", "blue"), env$wb_levels)
device <- tempfile(fileext = ".pdf")
pdf(device)
accuracy <- env$build_basin_accuracy_panels()
stopifnot(accuracy$summary[ma_basin == "20", ensemble_median_et] == 100,
          accuracy$summary[ma_basin == "20", median_RE] == 0,
          accuracy$summary[ma_basin == "20", basin_match_ratio] == 0,
          nrow(accuracy$summary) == length(ids))
invisible(lapply(accuracy[1:8], ggplotGrob))

# Both panels use medians across pairs; one retained member is unassessable.
pair_changes <- env$summarise_filtering_changes(pairs, products)
stopifnot(isTRUE(all.equal(pair_changes[ma_basin == "20", grid_change_pp], -5.5)),
          isTRUE(all.equal(pair_changes[ma_basin == "20", aggregated_change_pp], .5)),
          all(is.na(pair_changes[N_retained == 1, grid_change_pp])),
          identical(attr(pair_changes, "grid_summary_definition"),
                    "median across per-pair area-weighted grid-cell medians"))
map <- merge(basins, pair_changes, by.x = "BasinID", by.y = "ma_basin")
change_plot <- env$filtering_change_map(map, "grid_change_pp", "Synthetic")
change_data <- change_plot$layers[[2]]$data
stopifnot(as.character(change_data$class[change_data$BasinID == "56"]) == "One retained product")
invisible(ggplotGrob(change_plot))

# Pair selection matches unordered keys; reversing A/B reverses only the sign.
selected_examples <- data.table(panel_order = 1, ma_basin = "20", product_a = "c", product_b = "b")
selected <- env$select_example_pairs(selected_examples, pairs)
stopifnot(selected$dataset.x == "b", selected$dataset.y == "c")
inputs <- list(basins = basins, settings = env$pair_map_settings(),
  grid = data.table(ma_basin = "20", dataset.x = "b", dataset.y = "c",
                    lon = c(.5, 1.5), lat = .5, et_delta = c(10, 50), sym_rel_diff = c(.1, .2)))
forward <- copy(selected); forward[, `:=`(product_a = "b", product_b = "c")]
forward_plot <- env$build_basin_pair_map(forward, inputs, "signed")
reverse_plot <- env$build_basin_pair_map(selected, inputs, "signed")
relative_forward <- env$build_basin_pair_map(forward, inputs, "relative")
relative_reverse <- env$build_basin_pair_map(selected, inputs, "relative")
stopifnot(identical(forward_plot$layers[[1]]$data$et_delta,
                    -reverse_plot$layers[[1]]$data$et_delta),
          identical(relative_forward$layers[[1]]$data$value,
                    relative_reverse$layers[[1]]$data$value),
          as.character(reverse_plot$layers[[1]]$data$value[1]) == "-10 to 10")
invisible(ggplotGrob(env$assemble_pair_map_board(list(forward_plot, reverse_plot), inputs, "signed")))

# One coarse row per cell, broadcast area fractions, and genuine zero match ratios.
fine <- data.table(lon = seq_len(8), lat = 0, deg_8_lon = rep(c(4, 12, 20, 28), each = 2),
                    deg_8_lat = 4, area = c(1, 3, 3, 1, 1, 1, 1, 1),
                    rel_dataset_agreement = c("High", "Low", "High", "Low", "High", "Low", "Low", "Low"),
                    dist_dataset_agreement = c("High", "Low", "High", "Low", "High", "Low", "Low", "Low"))
mag <- data.table(deg_8_lon = c(4, 12, 20, 28), deg_8_lat = 4,
                   sIQR_8deg = c(.1, .1, .4, .4),
                   rel_dataset_agreement = c("High", "High", "Low", "Low"))
dist <- data.table(deg_8_lon = c(4, 12, 20, 28), deg_8_lat = 4,
                    index = c(.8, 0, .9, 0),
                    dist_dataset_agreement = c("High", "Low", "High", "Low"))
fixtures[["magnitude_agreement_scale_transfer.rds"]] <- fine
fixtures[["magnitude_agreement_8deg.rds"]] <- mag
fixtures[["distribution_agreement_8deg.rds"]] <- dist
fixtures[["thresholds_sIQR.RDS"]] <- c(0, .1, .2, .3, .4)
fixtures[["thresholds_distribution_agreement.RDS"]] <- c(0, .1, .3, .7, .9)
env$min_8_deg_N <- 1
scale_panels <- env$build_scale_agreement_panels(8)[[1]]
stopifnot(nrow(scale_panels$magnitude_data) == 4,
          identical(as.character(scale_panels$magnitude_data$transition),
                    c("Emergent", "Persistent", "Lost", "Other")),
          sum(scale_panels$distribution_data$index == 0) == 2)
invisible(ggplotGrob(scale_panels$magnitude))
fixtures[["magnitude_agreement_8deg.rds"]] <- rbind(mag, mag[1])
duplicate_rejected <- tryCatch({ env$build_scale_agreement_panels(8); FALSE }, error = function(e) TRUE)
stopifnot(duplicate_rejected)
fixtures[["magnitude_agreement_8deg.rds"]] <- mag

# Evaluate the SI entrypoints against synthetic reads; skip project setup/exports.
temp_output <- tempfile("manuscript_figures_"); dir.create(temp_output)
env$PATH_SAVE_PARTITION_EVAP_FIGURES <- paste0(temp_output, "/")
env$figure_widths <- 25
env$file.exists <- function(path) basename(path) %in% names(fixtures) || base::file.exists(path)
env$source <- function(file, local = TRUE, ...) {
  if (startsWith(file, "projects/partition_evap/figures_manuscript/")) {
    actual <- file.path(root, sub("^projects/partition_evap/figures_manuscript/", "", file))
    sys.source(actual, env)
    if (basename(file) == "manuscript_io.R") {
      env$save_manuscript_figure <- function(plot, filename, dimensions) {
        invisible(ggplotGrob(plot))
      }
    }
  }
  invisible(NULL)
}
for (script in c("fig5_SI_01_filtering_change.R", "fig5_SI_02_retained_pair_consistency.R",
                  "fig5_SI_03_excluded_products.R", "fig5_SI_04_compensating_pairs.R")) {
  sys.source(file.path(root, "si", script), env)
}
valid_bars <- env$bars[is.finite(share), .(span = sum(abs(share))), ma_basin]
stopifnot(all(abs(valid_bars$span - 1) < 1e-12),
          !any(env$bars$direction %in% "Within 10%"))
for (script in c("fig4_agreement_water_balance.R", "fig5_water_balance_filtering.R")) {
  sys.source(file.path(root, "main", script), env)
}
stopifnot(nrow(env$acceptance_counts) == 1,
          env$acceptance_counts$N_basins == length(ids))
dev.off()
unlink(c(temp_output, device), recursive = TRUE)
cat("Parsed", length(files), "R files; sources and nine-pair selection verified.\n")
cat("Synthetic accuracy, transition, per-pair filtering medians, gray-basin, pair-orientation and SI checks passed.\n")
