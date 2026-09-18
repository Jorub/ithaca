# Which IPCC regions have similar topology? ----
source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

library(data.table)

## data ----
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


regions <- rownames(cor_matrices[[1]])

rownames(mean_cor) <- regions
colnames(mean_cor) <- regions

regional_dissimilarity <- as.dist(1 - mean_cor)

regional_clustering_complete <- hclust(
  regional_dissimilarity,
  method = "complete"
)

group_colors <- c(
  "#E58B35",# Group 1
           "#8666A0",  # Group 2
            "#A5483A",# Group 3
           "#3E875D", # Group 4
           "#D8B72B", # Group 5
           "#397594", # Group 6
           "#817239" # Group 7
)

png(
  filename = paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig_5_SI_regional_clustering_dendogram.png"),
  width = 2400,
  height = 1600,
  res = 300
)

plot(
  regional_clustering_complete,
  ylab = expression("Dissimilarity (1 - mean spearman correlation)"),
  xlab = "IPCC reference region",
  sub = ""
)

abline(
  h = 1 - 0.2,
  lty = 2,
  lwd = 1.5,
  col = "gray60"
)

abline(
  h = 1 - 0.6,
  lty = 2,
  lwd = 1.5,
  col = "gray60"
)

par(lwd = 2)
rect.hclust(
  regional_clustering_complete,
  k = 7,
  border = group_colors
)


dev.off()
