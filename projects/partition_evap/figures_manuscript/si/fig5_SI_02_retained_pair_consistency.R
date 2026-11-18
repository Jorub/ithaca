# Figure 5 SI 02: retained-pair consistency overview ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

# Configuration ----
wb_tolerance <- 0.10
gap_threshold_pp <- 10
large_difference_threshold <- 0.20
joint_area_limit <- 0.20
area_limits <- 0.20
consistency_levels <- c("Both criteria met", "Gap criterion only",
                         "Area criterion only", "Neither criterion met")
consistency_cols <- setNames(
  c("#08519C", "#9ECAE1", "#E2A374", "#D9B3B3"), consistency_levels
)
stopifnot(joint_area_limit %in% area_limits)
paths <- manuscript_paths()
output_folder <- paths$si
# Only absorb machine-scale arithmetic error at strict/inclusive boundaries.
gap_tolerance_pp <- 100 * .Machine$double.eps
area_tolerance <- 10 * .Machine$double.eps

# Compact inputs from 11g; no global grid data or 11i selection ----
pair_summary <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds"
)))
product_status <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_pair_product_status.rds"
)))
ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
stopifnot(
  isTRUE(all.equal(attr(pair_summary, "wb_tolerance"), wb_tolerance)),
  isTRUE(all.equal(attr(product_status, "wb_tolerance"), wb_tolerance))
)
if (!"large_difference_fraction" %in% names(pair_summary) ||
    !isTRUE(all.equal(attr(pair_summary, "large_difference_threshold"),
                     large_difference_threshold))) {
  stop("Rerun 11g to calculate area with pairwise difference >20%.")
}
pair_summary[, ma_basin := as.character(ma_basin)]
product_status[, ma_basin := as.character(ma_basin)]
ma_sf$BasinID <- as.character(ma_sf$BasinID)
stopifnot(
  !anyDuplicated(product_status[, .(ma_basin, dataset)]),
  !anyDuplicated(pair_summary[, .(
    ma_basin, product_1 = pmin(as.character(dataset.x), as.character(dataset.y)),
    product_2 = pmax(as.character(dataset.x), as.character(dataset.y))
  )]),
  all(pair_summary$dataset.x != pair_summary$dataset.y),
  !anyDuplicated(ma_sf$BasinID),
  all(product_status$ma_basin %in% ma_sf$BasinID)
)

# Every retained combination counts once, with equal weight across pairs.
retained_pairs <- copy(pair_summary[both_retained %in% TRUE])
retention_x <- product_status[, .(ma_basin, dataset.x = dataset,
                                  retained_x_check = retained)]
retention_y <- product_status[, .(ma_basin, dataset.y = dataset,
                                  retained_y_check = retained)]
retained_pairs <- merge(retained_pairs, retention_x,
                          by = c("ma_basin", "dataset.x"), all.x = TRUE)
retained_pairs <- merge(retained_pairs, retention_y,
                          by = c("ma_basin", "dataset.y"), all.x = TRUE)
stopifnot(all(retained_pairs$retained_x_check %in% TRUE),
          all(retained_pairs$retained_y_check %in% TRUE))
retained_pairs[, c("retained_x_check", "retained_y_check") := NULL]
retained_pairs[, aggregated_sym_rel_diff := fifelse(
  is.finite(net_difference) & net_difference >= 0 &
    is.finite(pair_mean_et) & pair_mean_et > 0,
  net_difference / pair_mean_et, NA_real_
)]
retained_pairs[, gap_pp :=
                 100 * abs(median_sym_rel_diff - aggregated_sym_rel_diff)]
# Use one common assessable-pair denominator for the map and both criteria.
# Missing metrics are unavailable, not failed criteria; retain their counts.
retained_pairs[, assessable :=
  is.finite(gap_pp) & is.finite(median_sym_rel_diff) &
    median_sym_rel_diff >= 0 & is.finite(large_difference_fraction) &
    large_difference_fraction >= 0 & large_difference_fraction <= 1]
retained_pairs[, gap_pass :=
  assessable & gap_pp < gap_threshold_pp - gap_tolerance_pp]
retained_pairs[, area_pass :=
  assessable & large_difference_fraction <= joint_area_limit + area_tolerance]
retained_pairs[, joint_pass := gap_pass & area_pass]
retained_pairs[, consistency_class := fcase(
  !assessable, NA_character_,
  joint_pass, "Both criteria met",
  gap_pass, "Gap criterion only",
  area_pass, "Area criterion only",
  default = "Neither criterion met"
)]
pair_counts <- retained_pairs[, .(
  N_retained_pairs = .N,
  N_assessable_pairs = sum(assessable),
  N_gap_pass = sum(gap_pass, na.rm = TRUE),
  N_joint_pass = sum(joint_pass, na.rm = TRUE)
), .(ma_basin)]
for (area_limit in area_limits) {
  count_column <- paste0("N_area_", 100 * area_limit)
  area_counts <- retained_pairs[, .(N_pass = sum(
    assessable & large_difference_fraction <= area_limit + area_tolerance,
    na.rm = TRUE
  )), .(ma_basin)]
  pair_counts[area_counts, on = "ma_basin", (count_column) := i.N_pass]
}

# Include all basins, including those without an assessable retained pair.
basin_names <- as.data.table(st_drop_geometry(ma_sf))[
  , .(ma_basin = BasinID, basin_name = Name)
]
retained_counts <- product_status[, .(
  N_products = .N, N_retained = sum(retained)
), .(ma_basin)]
basin_summary <- merge(basin_names, retained_counts, by = "ma_basin", all.x = TRUE)
basin_summary <- merge(basin_summary, pair_counts, by = "ma_basin", all.x = TRUE)
count_columns <- c("N_retained_pairs", "N_assessable_pairs", "N_gap_pass",
                    "N_joint_pass", paste0("N_area_", 100 * area_limits))
for (count_column in count_columns) {
  set(basin_summary, which(is.na(basin_summary[[count_column]])), count_column, 0L)
}
basin_summary[, `:=`(
  N_possible_pairs = N_retained * (N_retained - 1) / 2,
  N_unassessable_pairs = N_retained * (N_retained - 1) / 2 - N_assessable_pairs,
  proportion_gap = fifelse(N_assessable_pairs > 0,
                            N_gap_pass / N_assessable_pairs, NA_real_),
  proportion_joint = fifelse(N_assessable_pairs > 0,
                              N_joint_pass / N_assessable_pairs, NA_real_)
)]
stopifnot(all(basin_summary$N_retained_pairs <= basin_summary$N_possible_pairs,
              na.rm = TRUE))
area_proportion_columns <- paste0("proportion_area_", 100 * area_limits)
for (area_index in seq_along(area_limits)) {
  basin_summary[, (area_proportion_columns[area_index]) := fifelse(
    N_assessable_pairs > 0,
    get(paste0("N_area_", 100 * area_limits[area_index])) / N_assessable_pairs,
    NA_real_
  )]
}
setorder(basin_summary, ma_basin)

# Partition the same assessable pairs into four mutually exclusive classes.
area_count_column <- paste0("N_area_", 100 * joint_area_limit)
basin_summary[, `:=`(
  N_gap_only = N_gap_pass - N_joint_pass,
  N_area_only = get(area_count_column) - N_joint_pass,
  N_neither = N_assessable_pairs - N_gap_pass -
    get(area_count_column) + N_joint_pass
)]
class_count_columns <- c("N_joint_pass", "N_gap_only", "N_area_only", "N_neither")
stopifnot(
  all(as.matrix(basin_summary[, ..class_count_columns]) >= 0),
  all(rowSums(basin_summary[, ..class_count_columns]) ==
        basin_summary$N_assessable_pairs)
)

# Sort the stacked barplot by the proportion meeting both map criteria.
# Keep the gap proportion as a tie-breaker; gray basins go last.
basin_order <- basin_summary[order(
  is.na(proportion_joint), -proportion_joint, -proportion_gap,
  as.integer(ma_basin), na.last = TRUE
), ma_basin]
basin_summary[, basin_plot := factor(ma_basin, levels = basin_order)]
basin_labels <- setNames(
  paste0(basin_summary$basin_name, " (", basin_summary$ma_basin, ")"),
  basin_summary$ma_basin
)
consistency_bars <- melt(
  basin_summary, id.vars = c("ma_basin", "basin_plot", "N_assessable_pairs"),
  measure.vars = class_count_columns,
  variable.name = "consistency_class", value.name = "N_pairs"
)
consistency_bars[, proportion := fifelse(
  N_assessable_pairs > 0, N_pairs / N_assessable_pairs, NA_real_
)]
consistency_bars[, consistency_class := factor(consistency_class,
  levels = class_count_columns, labels = consistency_levels)]
unassessable <- basin_summary[N_assessable_pairs == 0]

# a: Joint criterion, expressed as a proportion of assessable retained pairs.
map_data <- merge(ma_sf[, c("BasinID", "Name")], basin_summary,
                    by.x = "BasinID", by.y = "ma_basin", all.x = TRUE)
map_levels <- c("0%", ">0-20%", ">20-40%", ">40-60%", ">60-80%",
                 ">80-100%", "Not assessable")
map_data$joint_class <- cut(
  map_data$proportion_joint, breaks = c(-Inf, 0, .2, .4, .6, .8, 1),
  labels = head(map_levels, -1)
)
map_data$joint_class <- factor(
  fifelse(is.na(map_data$joint_class), "Not assessable",
           as.character(map_data$joint_class)), levels = map_levels
)
map_cols <- setNames(c("#F2F2F2", "#DEEBF7", "#C6DBEF", "#9ECAE1",
                        "#4292C6", "#08519C", "gray70"), map_levels)
fig_map <- ggplot() +
  geom_sf(data = world_no_antarctica,
            fill = "gray95", colour = "gray80", linewidth = .2) +
  geom_sf(data = map_data, aes(fill = joint_class),
            colour = "gray45", linewidth = .25) +
  scale_fill_manual(values = map_cols, limits = map_levels, drop = FALSE,
                      name = "Retained pairs meeting\nboth criteria",
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = FALSE) +
  labs(x = NULL, y = NULL, title = "a  Basin and grid-cell consistency") +
  theme_map_fig5 + theme(legend.position = "bottom")

bar_theme <- theme_fig5 + theme(
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  legend.position = "bottom", plot.title = element_text(face = "bold", size = 11)
)
# Gray columns indicate unavailable comparisons, rather than zero passes.
fig_criteria <- ggplot(consistency_bars, aes(basin_plot)) +
  geom_col(data = unassessable, aes(y = 1), fill = "gray70", width = .8) +
  geom_col(data = consistency_bars[is.finite(proportion)],
             aes(y = proportion, fill = consistency_class),
             position = position_stack(reverse = TRUE), width = .8) +
  geom_hline(yintercept = c(.25, .5, .75), colour = "gray45",
               linetype = "dashed", linewidth = .3) +
  scale_fill_manual(values = consistency_cols, limits = consistency_levels,
                      drop = FALSE, name = NULL,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_discrete(labels = basin_labels, drop = FALSE) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25),
                      oob = scales::squish,
                      labels = scales::label_percent(accuracy = 1),
                      expand = expansion(mult = c(0, .03))) +
  labs(x = "Basin", y = "Proportion of\nretained pairs",
       title = "b  Consistency criteria") +
  bar_theme + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))
fig_overview <- ggarrange(fig_map, fig_criteria,
                           ncol = 1, heights = c(1.5, 1.1), align = "v")

# Save to the manuscript namespace, without overwriting exploratory summaries.
settings <- list(wb_tolerance = wb_tolerance, gap_threshold_pp = gap_threshold_pp,
                 large_difference_threshold = large_difference_threshold,
                 joint_area_limit = joint_area_limit,
                 denominator = "retained pairs with both differences and valid area fraction")
attr(basin_summary, "criteria") <- settings
attr(retained_pairs, "criteria") <- settings
saveRDS(basin_summary, file.path(paths$data, "fig5_retained_pair_consistency_summary.rds"))
saveRDS(retained_pairs, file.path(paths$data, "fig5_retained_pair_consistency_pairs.rds"))
fwrite(basin_summary, file.path(paths$data, "fig5_retained_pair_consistency_summary.csv"))
save_manuscript_figure(fig_overview,
  file.path(paths$si, "fig5_SI_02_retained_pair_consistency"), c(figure_widths, 28))
