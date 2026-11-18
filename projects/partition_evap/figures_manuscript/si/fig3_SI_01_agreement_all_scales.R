# Figure 3 SI 01: agreement all scales ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
aggregation_scales <- c(1, 2, 4, 8)
higher_agreement_area_threshold <- .5

# Data and maps ----
fine <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                        "magnitude_agreement_scale_transfer.rds")))
stopifnot(!anyDuplicated(fine[, .(lon, lat)]),
          length(higher_agreement_area_threshold) == 1, between(higher_agreement_area_threshold, 0, 1))
levels <- c("Low", "Below average", "Average", "Above average", "High")
transition_cols <- c(Other = "#D9D9D9", Persistent = "#56B4E9",
                      Emergent = "#006837", Lost = "#D55E00")
higher <- c("High", "Above average")
thresholds_siqr <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "thresholds_sIQR.RDS"))
thresholds_distribution <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                            "thresholds_distribution_agreement.RDS"))
panels <- list()
for (scale in aggregation_scales) {
  coords <- paste0("deg_", scale, c("_lon", "_lat"))
  siqr_col <- paste0("sIQR_", scale, "deg")
  min_cells <- get(paste0("min_", scale, "_deg_N"), inherits = TRUE)
  mag <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                         paste0("magnitude_agreement_", scale, "deg.rds"))))
  dist <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                         paste0("distribution_agreement_", scale, "deg.rds"))))
  stopifnot(!anyDuplicated(mag[, ..coords]), !anyDuplicated(dist[, ..coords]))
  mag <- mag[, c(coords, siqr_col, "rel_dataset_agreement"), with = FALSE]
  dist <- dist[, c(coords, "index", "dist_dataset_agreement"), with = FALSE]
  fractions <- fine[, .(
    N_fine = .N, area = sum(area, na.rm = TRUE),
    higher_magnitude_fraction = sum(area[rel_dataset_agreement %in% higher], na.rm = TRUE) /
      sum(area, na.rm = TRUE),
    higher_distribution_fraction = sum(area[dist_dataset_agreement %in% higher], na.rm = TRUE) /
      sum(area, na.rm = TRUE)
  ), by = coords]
  mag <- merge(mag, fractions, by = coords)[N_fine > min_cells & area > 0]
  dist <- merge(dist, fractions, by = coords)[N_fine > min_cells & area > 0]
  mag[, magnitude_class := factor(as.character(rel_dataset_agreement), levels = levels)]
  dist[, distribution_class := factor(as.character(dist_dataset_agreement), levels = levels)]
  mag[, transition := factor(fcase(
    is.na(magnitude_class) | !is.finite(higher_magnitude_fraction), NA_character_,
    magnitude_class %in% higher & higher_magnitude_fraction >= higher_agreement_area_threshold, "Persistent",
    magnitude_class %in% higher & higher_magnitude_fraction < higher_agreement_area_threshold, "Emergent",
    !magnitude_class %in% higher & higher_magnitude_fraction >= higher_agreement_area_threshold, "Lost",
    default = "Other"
  ), levels = names(transition_cols))]
  dist[, transition := factor(fcase(
    is.na(distribution_class) | !is.finite(higher_distribution_fraction), NA_character_,
    distribution_class %in% higher & higher_distribution_fraction >= higher_agreement_area_threshold, "Persistent",
    distribution_class %in% higher & higher_distribution_fraction < higher_agreement_area_threshold, "Emergent",
    !distribution_class %in% higher & higher_distribution_fraction >= higher_agreement_area_threshold, "Lost",
    default = "Other"
  ), levels = names(transition_cols))]
  # Preserve the alternative fixed fine-grid reference for supporting tables.
  mag[, fine_reference_class := factor(fcase(
    get(siqr_col) > thresholds_siqr[5], "Low",
    get(siqr_col) > thresholds_siqr[4], "Below average",
    get(siqr_col) > thresholds_siqr[3], "Average",
    get(siqr_col) > thresholds_siqr[2], "Above average",
    get(siqr_col) <= thresholds_siqr[2], "High", default = NA_character_
  ), levels = levels)]
  dist[, fine_reference_class := factor(fcase(
    index > thresholds_distribution[5], "High",
    index > thresholds_distribution[4], "Above average",
    index > thresholds_distribution[3], "Average",
    index > thresholds_distribution[2], "Below average",
    index <= thresholds_distribution[2], "Low", default = NA_character_
  ), levels = levels)]
  # The original pattern maps merge the two metrics on common coarse cells.
  mag_common <- mag[dist[, ..coords], on = coords, nomatch = 0]
  dist_common <- dist[mag[, ..coords], on = coords, nomatch = 0]
  mag_plot <- mag_common[, .(lon = get(coords[1]), lat = get(coords[2]), value = magnitude_class)]
  dist_plot <- dist_common[, .(lon = get(coords[1]), lat = get(coords[2]), value = distribution_class)]
  mag_change <- mag[, .(lon = get(coords[1]), lat = get(coords[2]), value = transition)]
  dist_change <- dist[, .(lon = get(coords[1]), lat = get(coords[2]), value = transition)]
  # Maps for this resolution ----
  plot_data <- list(magnitude = mag_plot, distribution = dist_plot,
                    magnitude_transition = mag_change, distribution_transition = dist_change)
  map_titles <- c(magnitude = "Magnitude agreement", distribution = "Distribution agreement",
                  magnitude_transition = "Magnitude transitions",
                  distribution_transition = "Distribution transitions")
  scale_plots <- list()
  for (metric in names(plot_data)) {
    data <- plot_data[[metric]]
    is_transition <- grepl("transition", metric)
    colours <- if (is_transition) transition_cols else color_agreement
    legend_title <- if (is_transition) "Higher agreement" else "Agreement"
    map_title <- paste0(map_titles[[metric]], ": ", scale, " degrees")
    scale_plots[[metric]] <- ggplot() +
      geom_tile(data = data, aes(lon, lat, fill = value), width = scale, height = scale) +
      geom_sf(data = world_no_antarctica, fill = NA, colour = "gray35", linewidth = .25) +
      scale_fill_manual(values = colours, limits = names(colours),
                        drop = FALSE, na.value = "transparent", na.translate = FALSE,
                        name = legend_title,
                        guide = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_y_continuous(breaks = seq(-60, 60, 30)) +
      coord_sf(ylim = c(-70, 90), expand = FALSE) +
      labs(x = NULL, y = NULL, title = map_title) +
      theme_map_leg + theme(legend.position = "bottom")
  }
  panels[[as.character(scale)]] <- c(scale_plots,
    list(magnitude_data = mag, distribution_data = dist))
}
plots <- unlist(lapply(panels, function(p) p[c("magnitude", "distribution")]), recursive = FALSE)
# Figure layout ----
legend <- bottom_legend(plots[[1]])
body <- ggarrange(plotlist = lapply(plots, function(p) p + theme(legend.position = "none")),
                  ncol = 2, nrow = ceiling(length(plots) / 2),
                  labels = letters[seq_along(plots)], align = "hv")
figure <- cowplot::plot_grid(body, cowplot::ggdraw(legend), ncol = 1,
                             rel_heights = c(1, .09))
save_manuscript_figure(figure, file.path(paths$si, "fig3_SI_01_agreement_all_scales"), c(figure_widths, 38))
