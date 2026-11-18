# Adapted from figure3_b_maps_scale_transfer.R and figure3_c_maps_scale_transfer.R.
# Stored coarse classes use recalculated scale-specific thresholds. Fine-grid
# reference classes remain separate and do not drive transition colours.
scale_transition_class <- function(coarse_class, fine_fraction, threshold) {
  higher <- c("High", "Above average")
  factor(fcase(
    is.na(coarse_class) | !is.finite(fine_fraction), NA_character_,
    coarse_class %in% higher & fine_fraction >= threshold, "Persistent",
    coarse_class %in% higher & fine_fraction < threshold, "Emergent",
    !coarse_class %in% higher & fine_fraction >= threshold, "Lost",
    default = "Other"
  ), levels = c("Other", "Persistent", "Emergent", "Lost"))
}

coarse_agreement_map <- function(data, scale, settings) {
  ggplot() +
    geom_tile(data = data, aes(lon, lat, fill = value), width = scale, height = scale) +
    geom_sf(data = world_no_antarctica, fill = NA, colour = "gray35", linewidth = .25) +
    scale_fill_manual(values = settings$colours, limits = names(settings$colours),
                      drop = FALSE, na.value = "transparent", na.translate = FALSE,
                      name = settings$legend,
                      guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    coord_sf(ylim = c(-70, 90), expand = FALSE) +
    labs(x = NULL, y = NULL, title = settings$title) +
    theme_map_leg + theme(legend.position = "bottom")
}

build_scale_agreement_panels <- function(scales, area_threshold = .5) {
  fine <- as.data.table(readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                          "magnitude_agreement_scale_transfer.rds")))
  stopifnot(!anyDuplicated(fine[, .(lon, lat)]),
            length(area_threshold) == 1, between(area_threshold, 0, 1))
  levels <- c("Low", "Below average", "Average", "Above average", "High")
  transition_cols <- c(Other = "#D9D9D9", Persistent = "#56B4E9",
                        Emergent = "#006837", Lost = "#D55E00")
  higher <- c("High", "Above average")
  thresholds_siqr <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP, "thresholds_sIQR.RDS"))
  thresholds_distribution <- readRDS(file.path(PATH_SAVE_PARTITION_EVAP,
                                              "thresholds_distribution_agreement.RDS"))
  panels <- list()
  for (scale in scales) {
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
    mag[, transition := scale_transition_class(magnitude_class,
                                                higher_magnitude_fraction, area_threshold)]
    dist[, transition := scale_transition_class(distribution_class,
                                                 higher_distribution_fraction, area_threshold)]
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
    panels[[as.character(scale)]] <- list(
      magnitude = coarse_agreement_map(mag_plot, scale, list(colours = color_agreement,
        legend = "Agreement", title = paste0("Magnitude agreement: ", scale, " degrees"))),
      distribution = coarse_agreement_map(dist_plot, scale, list(colours = color_agreement,
        legend = "Agreement", title = paste0("Distribution agreement: ", scale, " degrees"))),
      magnitude_transition = coarse_agreement_map(mag_change, scale,
        list(colours = transition_cols, legend = "Higher agreement",
             title = paste0("Magnitude transitions: ", scale, " degrees"))),
      distribution_transition = coarse_agreement_map(dist_change, scale,
        list(colours = transition_cols, legend = "Higher agreement",
             title = paste0("Distribution transitions: ", scale, " degrees"))),
      magnitude_data = mag, distribution_data = dist)
  }
  panels
}

assemble_scale_maps <- function(plots, columns = 2, labels = letters[seq_along(plots)]) {
  legend <- bottom_legend(plots[[1]])
  body <- ggarrange(plotlist = lapply(plots, function(p) p + theme(legend.position = "none")),
                    ncol = columns, labels = labels, align = "hv")
  cowplot::plot_grid(body, cowplot::ggdraw(legend), ncol = 1, rel_heights = c(1, .09))
}
