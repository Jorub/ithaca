# Both panels take the median across per-pair summaries from 11g.
# Local differences are already area-weighted within each pair; pairs receive
# equal weight when summarised across the full or retained ensemble.
summarise_filtering_changes <- function(pairs, products) {
  pairs <- copy(pairs)
  pairs[, ma_basin := as.character(ma_basin)]
  products <- copy(products)
  products[, ma_basin := as.character(ma_basin)]
  stopifnot(!anyDuplicated(products[, .(ma_basin, dataset)]),
            !anyDuplicated(pairs[, .(ma_basin, dataset.x, dataset.y)]))
  pairs[, aggregated_difference := fifelse(
    is.finite(net_difference) & is.finite(pair_mean_et) & pair_mean_et > 0,
    net_difference / pair_mean_et, NA_real_)]
  summary <- products[, .(N_products = .N, N_retained = sum(retained)), ma_basin]
  aggregate <- pairs[is.finite(aggregated_difference), .(
    aggregated_full = median(aggregated_difference),
    aggregated_retained = if (any(both_retained %in% TRUE)) {
      median(aggregated_difference[both_retained %in% TRUE])
    } else NA_real_
  ), ma_basin]
  local <- pairs[is.finite(median_sym_rel_diff), .(
    grid_full = median(median_sym_rel_diff),
    grid_retained = if (any(both_retained %in% TRUE)) {
      median(median_sym_rel_diff[both_retained %in% TRUE])
    } else NA_real_
  ), ma_basin]
  definition <- "median across per-pair area-weighted grid-cell medians"
  summary <- merge(summary, local, by = "ma_basin", all.x = TRUE)
  summary <- merge(summary, aggregate, by = "ma_basin", all.x = TRUE)
  summary[N_retained < 2, c("grid_full", "grid_retained", "aggregated_full",
                            "aggregated_retained") := NA_real_]
  summary[, `:=`(grid_change_pp = 100 * (grid_retained - grid_full),
                  aggregated_change_pp = 100 * (aggregated_retained - aggregated_full))]
  attr(summary, "grid_summary_definition") <- definition
  attr(summary, "aggregated_summary_definition") <-
    "median across pairwise basin-aggregated symmetric relative differences"
  attr(summary, "change_definition") <- "filtered minus full, percentage points"
  summary
}

filtering_change_map <- function(map, metric, title) {
  labels <- c("Decrease >=10 pp", "Decrease 5-10 pp", "Within +/-5 pp",
               "Increase 5-10 pp", "Increase >10 pp", "One retained product", "Unavailable")
  map$class <- as.character(cut(map[[metric]], c(-Inf, -10, -5, 5, 10, Inf),
                                labels = labels[1:5], include.lowest = TRUE))
  map$class[is.na(map$class)] <- "Unavailable"
  map$class[!is.na(map$N_retained) & map$N_retained == 1] <- "One retained product"
  map$class <- factor(map$class, levels = labels)
  colours <- setNames(c("#2166AC", "#67A9CF", "#E5E5E5", "#EF8A62", "#B2182B",
                         "gray35", "gray85"), labels)
  ggplot() +
    geom_sf(data = world_no_antarctica, fill = "gray95", colour = "gray80", linewidth = .2) +
    geom_sf(data = map, aes(fill = class), colour = "gray45", linewidth = .25) +
    scale_fill_manual(values = colours, limits = labels, drop = FALSE,
      name = "Filtering change\nRelative difference (pp)",
      guide = guide_legend(nrow = 3, byrow = TRUE)) +
    scale_y_continuous(breaks = seq(-60, 60, 30)) +
    coord_sf(ylim = c(-70, 90), expand = FALSE) +
    labs(x = NULL, y = NULL, title = title) + theme_map_fig5 +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}
