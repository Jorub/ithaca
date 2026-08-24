# Which IPCC regions have similar topology? ----
source('source/evap_trend.R')

ipcc_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_regions_topology_rank_roles.rds"))
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

min_cor <- apply(
  cor_array,
  MARGIN = c(1, 2),
  FUN = min,
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

rho_threshold = 0.4 

region_clusters <- cutree(
  regional_clustering_complete,
  h = 1 - rho_threshold
)

region_clusters

region_order <- regional_clustering$labels[
  regional_clustering$order
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

names(rho_colors) <- rho_labels

rho_colors <- c(
  "-0.4 to <-0.2" = "#E8A090",
  "-0.2 to <0.0"  = "#FDDBC7",
  "0.0 to <0.2"   = "#D1E5F0",
  "0.2 to <0.4"   = "#92C5DE",
  "0.4 to <0.6"   = "#4393C3",
  "0.6 to <0.8"   = "#2166AC",
  "0.8 to 1.0"    = "#053061"
)

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


