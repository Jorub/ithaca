# Which IPCC regions have similar topology? ----
source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

library(data.table)

## data ----
ipcc_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_regions_topology_rank_roles.rds"))
global_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))

----
ipcc_regions <- unique(ipcc_topo$IPCC_ref_region)

roles <- colnames(ipcc_topo[,3:8])

ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, pos_signal)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                     value.var = "pos_signal")
cor_pos <- cor(data_wide[, 2:45], method = "spearman")

ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, neg_signal)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                   value.var = "neg_signal")

cor_neg <- cor(data_wide[, 2:45], method = "spearman")

ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, signal_dampener)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                   value.var = "signal_dampener")

cor_signal_dampener <- cor(data_wide[, 2:45], method = "spearman")


ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, opposing_majority_trend)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                   value.var = "opposing_majority_trend")

cor_opposing_majority_trend <- cor(data_wide[, 2:45], method = "spearman")

ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, opposing_significance)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                   value.var = "opposing_significance")

cor_opposing_significance <- cor(data_wide[, 2:45], method = "spearman")

ipcc_topo_sel <- copy(ipcc_topo[,.(dataset, ipcc_regions, opposition_contributor)])
data_wide <- dcast(data = ipcc_topo_sel, dataset~ipcc_regions,
                   value.var = "opposition_contributor")

cor_opposition_contributor <- cor(data_wide[, 2:45], method = "spearman")

cor_matrices <- list(
  pos_signal = cor_pos,
  neg_signal = cor_neg,
  signal_dampener = cor_signal_dampener,
  opposing_majority_trend = cor_opposing_majority_trend,
  opposing_significance  = cor_opposing_significance,
  opposition_contributor = cor_opposition_contributor
)

cor_array <- simplify2array(cor_matrices)

mean_cor <- apply(
  cor_array,
  MARGIN = c(1, 2),
  FUN = mean,
  na.rm = TRUE
)

sd_cor <- apply(
  cor_array,
  MARGIN = c(1, 2),
  FUN = sd,
  na.rm = TRUE
)

regions <- rownames(cor_matrices[[1]])

rownames(mean_cor) <- regions
colnames(mean_cor) <- regions

regional_dissimilarity <- as.dist(1 - mean_cor)

regional_clustering_complete <- hclust(
  regional_dissimilarity,
  method = "complete"
)

plot(
  regional_clustering_complete,
  ylab = expression("Dissimilarity (1 - mean " * rho * ")"),
  xlab = "IPCC reference region",
  sub = ""
)

rho_threshold = 0.6 

region_clusters <- cutree(
  regional_clustering_complete,
  h = 1 - rho_threshold
)

region_clusters

table(region_clusters)

region_order <- regional_clustering_complete$labels[
  regional_clustering_complete$order
]


mean_cor_ordered <- mean_cor[
  region_order,
  region_order
]

mean_cor_long <- as.data.table(
  as.table(mean_cor_ordered)
)

setnames(
  mean_cor_long,
  c("region_1", "region_2", "mean_rho")
)

mean_cor_long[
  ,
  `:=`(
    region_1 = factor(region_1, levels = rev(region_order)),
    region_2 = factor(region_2, levels = region_order)
  )
]


rho_breaks <- seq(-0.4, 1, by = 0.2)

rho_labels <- c(
  "-0.4 to <-0.2",
  "-0.2 to <0.0",
  "0.0 to <0.2",
  "0.2 to <0.4",
  "0.4 to <0.6",
  "0.6 to <0.8",
  "0.8 to 1.0"
)

mean_cor_long[
  ,
  rho_class := cut(
    mean_rho,
    breaks = rho_breaks,
    labels = rho_labels,
    include.lowest = TRUE,
    right = FALSE
  )
]

rho_colors <- c(
  "-0.4 to <-0.2" = "#E8A090",
  "-0.2 to <0.0"  = "#FDDBC7",
  "0.0 to <0.2"   = "#D1E5F0",
  "0.2 to <0.4"   = "#92C5DE",
  "0.4 to <0.6"   = "#4393C3",
  "0.6 to <0.8"   = "#2166AC",
  "0.8 to 1.0"    = "#053061"
)

names(rho_colors) <- rho_labels

fig_cor_heatmap <- ggplot(
  mean_cor_long,
  aes(
    x = region_2,
    y = region_1,
    fill = rho_class
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_manual(
    values = rho_colors,
    limits = rho_labels,
    drop = FALSE,
    na.value = "grey80",
    name = expression("Mean " * rho),
    guide = guide_legend(reverse = TRUE)
  ) +
  coord_equal() +
  labs(
    x = "IPCC reference region",
    y = "IPCC reference region"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 6
    ),
    axis.text.y = element_text(size = 6),
    legend.key.height = unit(0.35, "cm")
  )


# Are some metrics more correlated than others?  ----
metric_correlations <- rbindlist(
  lapply(names(cor_matrices), function(metric_name) {
    
    current_matrix <- cor_matrices[[metric_name]]
    values <- current_matrix[upper.tri(current_matrix)]
    
    data.table(
      metric = metric_name,
      rho = values
    )
  })
)

metric_summary <- metric_correlations[
  ,
  .(
    mean_rho = mean(rho, na.rm = TRUE),
    minimum = min(rho, na.rm = TRUE),
    q25 = quantile(rho, 0.25, na.rm = TRUE),
    median_rho = median(rho, na.rm = TRUE),
    q75 = quantile(rho, 0.75, na.rm = TRUE),
    maximum = max(rho, na.rm = TRUE),
    fraction_above_04 = mean(rho >= 0.4, na.rm = TRUE),
    fraction_negative = mean(rho < 0, na.rm = TRUE)
  ),
  by = metric
]

rho_breaks_metric <- seq(-1, 1, by = 0.2)

rho_labels_metric <- c(
  "[-1.0,-0.8)",
  "[-0.8,-0.6)",
  "[-0.6,-0.4)",
  "[-0.4,-0.2)",
  "[-0.2,0.0)",
  "[0.0,0.2)",
  "[0.2,0.4)",
  "[0.4,0.6)",
  "[0.6,0.8)",
  "[0.8,1.0]"
)

rho_colors_metric <- c(
  "[-1.0,-0.8)" = "#9E4138",
  "[-0.8,-0.6)" = "#B95C50",
  "[-0.6,-0.4)" = "#D47868",
  "[-0.4,-0.2)" = "#E8A090",
  "[-0.2,0.0)"  = "#FDDBC7",
  "[0.0,0.2)"   = "#D1E5F0",
  "[0.2,0.4)"   = "#92C5DE",
  "[0.4,0.6)"   = "#4393C3",
  "[0.6,0.8)"   = "#2166AC",
  "[0.8,1.0]"    = "#053061"
)

metric_correlations[
  ,
  rho_class := cut(
    rho,
    breaks = rho_breaks_metric,
    labels = rho_labels_metric,
    include.lowest = TRUE,
    right = FALSE
  )
]

correlation_frequencies <- metric_correlations[
  ,
  .N,
  by = .(metric, rho_class)
][
  ,
  fraction := N / sum(N),
  by = metric
]

fig_metric_frequency <- ggplot(
  correlation_frequencies,
  aes(
    x = rho_class,
    y = fraction,
    fill = rho_class
  )
) +
  geom_col(
    width = 0.85,
    show.legend = FALSE
  ) +
  # Boundary of the moderate-similarity classes
  geom_vline(
    xintercept = 7.5,
    linetype = 2,
    color = "grey35"
  ) +
  facet_wrap(
    ~ metric,
    ncol = 2
  ) +
  scale_fill_manual(
    values = rho_colors_metric,
    drop = FALSE
  ) +
  scale_x_discrete(
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = expression("Regional rank correlation (" * rho * ")"),
    y = "Fraction of region pairs"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Is the overall similarity across roles evident across IPCC reference region pairs ?  ----

sd_ordered <- sd_cor[
  region_order,
  region_order
]

sd_long <- as.data.table(
  as.table(sd_cor_ordered)
)

setnames(
  sd_long,
  c("region_1", "region_2", "sd_rho")
)

sd_long[
  ,
  `:=`(
    region_1 = factor(region_1, levels = rev(region_order)),
    region_2 = factor(region_2, levels = region_order)
  )
]


sd_breaks <- c(0, 0.1,0.2, 0.3, 0.5)

sd_labels <- c(
  "[0.0,0.1)",
  "[0.1,0.2)",
  "[0.2,0.3)",
  "[0.3,0.5]"
)

sd_long[
  ,
  sd_class := cut(
    sd_rho,
    breaks = sd_breaks,
    labels = sd_labels,
    include.lowest = TRUE,
    right = FALSE
  )
]

sd_colors <- c(
  "[0.0,0.1)" = "#F7FBFF",
  "[0.1,0.2)" = "#C6DBEF",
  "[0.2,0.3)" = "#2171B5",
  "[0.3,0.5]" = "#08306B"
)

names(sd_colors) <- sd_labels

fig_sd_heatmap <- ggplot(
  sd_long,
  aes(
    x = region_2,
    y = region_1,
    fill = sd_class
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_manual(
    values = sd_colors,
    breaks = rev(sd_labels),
    drop = FALSE,
    na.value = "grey80",
    name = expression("SD of " * rho)
  )+
  coord_equal() +
  labs(
    x = "IPCC reference region",
    y = "IPCC reference region"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 6
    ),
    axis.text.y = element_text(size = 6),
    legend.key.height = unit(0.35, "cm")
  )+
  ggtitle("Across-metric variability in regional rank correlations")

# Which datasets have stable ranks? ----
ipcc_topo_merge <- global_topo[ipcc_topo, on = .(dataset)]
diff <- ipcc_topo
diff[, 3:8] <- ipcc_topo_merge[, 9:14]-ipcc_topo_merge[, 2:7]

diff_long <- melt(diff, id.vars = c("dataset", "IPCC_ref_region"))

diff_long_mean <- diff_long[, .(mean_rank_diff = mean(value), IQR = IQR(value)), .(variable, dataset)]


diff_long_mean[
  ,
  IQR_class := cut(
    IQR,
    breaks = c(-Inf, 2, 4, 6, Inf),
    labels = c(
      "0–2",
      "2–4",
      "4–6",
      "6–8"
    ),
    right = TRUE
  )
]

fill_rank_IQR <- c(
  "0–2" = "grey90",
  "2–4" = "#CBC9E2",
  "4–6" = "#9E9AC8",
  "6–8" = "#6A51A3"
)

fig_regional_stability <- ggplot(
  diff_long_mean,
  aes(
    x = variable,
    y = dataset,
    fill = IQR_class
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = fill_rank_IQR,
    limits = names(fill_rank_IQR),
    breaks = rev(names(fill_rank_IQR)),
    drop = FALSE,
    name = "Regional\nrank IQR"
  ) +
  scale_x_discrete(
    labels = function(x) {
      gsub("_", " ", x)
    }
  ) +
  labs(
    x = NULL,
    y = "Dataset"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "right"
  )


hc <- regional_clustering_complete

h_values <- seq(
  from = 0,
  to = max(hc$height),
  length.out = 1000
)

n_clusters <- vapply(
  h_values,
  function(h) length(unique(cutree(hc, h = h))),
  integer(1)
)

threshold <- 0.6
k_threshold <- length(unique(cutree(hc, h = threshold)))

plot(
  h_values,
  n_clusters,
  type = "s",
  lwd = 2,
  xlab = expression("Dissimilarity threshold (" * h * ")"),
  ylab = "Number of clusters",
  main = "Cluster count across dissimilarity thresholds"
)

abline(
  v = threshold,
  lty = 2,
  col = "red"
)

points(
  threshold,
  k_threshold,
  pch = 19,
  col = "red"
)

text(
  threshold,
  k_threshold,
  labels = paste0("  h = 0.6: ", k_threshold, " clusters"),
  pos = 4,
  col = "red"
)