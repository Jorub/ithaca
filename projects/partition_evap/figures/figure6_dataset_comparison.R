# tile plots for over and underestimator and best
# heatplots
source('source/partition_evap.R')
source('source/graphics.R')

library(ggpubr)

evap_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "grid_performance_datasets.rds"))
cols_data_dt <- data.table(dataset = names(cols_data), colors = cols_data) 

evap_summary_over <- evap_summary[performance == "Under"]
evap_summary_over[, rank := rank(area_fraction)]
evap_summary_over <- merge(evap_summary_over, cols_data_dt, by = "dataset")  
evap_summary_over <- evap_summary_over[order(rank)]
evap_summary[, dataset := factor(dataset, levels = evap_summary_over$dataset)]
evap_summary[performance == "Over", performance := "Higher"]
evap_summary[performance == "Under", performance := "Lower"]

## global ----
performance_global <- ggplot(evap_summary)+
  geom_bar(aes(x = dataset, y = area_fraction, fill = performance), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("Closest"= "gold","Higher" = colset_RdBu_5[5], "Lower" = colset_RdBu_5[1]))+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        legend.spacing.x = unit(1.5, "cm"),
        legend.spacing.y = unit(1.5, "cm"),
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom")+
  labs(x = "Datasets", y = "Global area fraction [-]", 
       fill = "Deviation to\nensemble mean")+
  coord_flip()

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "partition_biome_datasets_for_plot.rds"))
data_count <- data[performance == "Over", .N, .(performance, dataset)]
data_count <- rbind(data_count, data.table(performance = "Over", dataset = "gldas-vic", N = 0))
data_count[, rank := rank(N)]

data_count <- merge(data_count, cols_data_dt, by = "dataset")  
data_count <- data_count[order(rank)]
data[, dataset := factor(dataset, levels = data_count$dataset)]
data[performance == "Over", performance := "Higher"]
data[performance == "Under", performance := "Lower"]


global <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products.rds"))

q10 <- quantile(global$area_fraction, c(0.1))
q30 <- quantile(global$area_fraction, c(0.3))
q70 <- quantile(global$area_fraction, c(0.7))
q90 <- quantile(global$area_fraction, c(0.9))

global[, fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                             labels = c("Low", "Below Average", "Average", "Above Average", "High"))]

dataset_order <- as.character(evap_summary_over$dataset)

global[, dataset.x := factor(dataset.x, levels = dataset_order)]
global[, dataset.y := factor(dataset.y, levels = rev(dataset_order))]

dataset_global <- 
  ggplot(global) +
  geom_tile(
    aes(x = dataset.x, y = dataset.y, fill = fraction_fac), 
    color = "white", lwd = 0.8, linetype = 1
  ) +
  geom_tile(
    aes(
      x = factor(dataset.y, levels = dataset_order),
      y = factor(dataset.x, levels = rev(dataset_order)),
      fill = fraction_fac
    ), 
    color = "white", lwd = 0.8, linetype = 1
  ) +
  scale_fill_manual(values = colset_RdBu_5) +
  scale_x_discrete(limits = dataset_order, drop = FALSE) +
  scale_y_discrete(limits = rev(dataset_order), drop = FALSE) +
  labs(fill = "Distribution\nagreement    ", x = "", y = "") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = "black"),
    legend.position = "bottom"
  ) +
  ggtitle(label = "Global")

top_row <- ggarrange(performance_global, dataset_global, 
                     labels = c("a", "b"), nrow = 1, 
                     align = "hv")

## biomes ----
biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
biome[, dataset.x := factor(dataset.x, levels = evap_summary_over$dataset, ordered = T)]
biome[, dataset.y := factor(dataset.y, levels = evap_summary_over$dataset, ordered = T)]

q10 <- quantile(biome$area_fraction, c(0.1))
q30 <- quantile(biome$area_fraction, c(0.3))
q70 <- quantile(biome$area_fraction, c(0.7))
q90 <- quantile(biome$area_fraction, c(0.9))

biome[, fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                            labels = c("Low", "Below average", "Average", "Above average", "High"))]


## biomes diff tog global----

biome_global <- merge(biome, global[,.(dataset.x, dataset.y, area_fraction)], 
                      by = c("dataset.x", "dataset.y"))

biome_global[, area_fraction_diff := area_fraction.y-area_fraction.x]
biome_global[area_fraction_diff > 0.05 , agreement := "Higher"]
biome_global[area_fraction_diff < -0.05 , agreement := "Lower"]
biome_global[is.na(agreement), agreement := "Same"]

## plot biome agreement ----
dataset_cols <- c("Low" =  colset_RdBu_5[1], "Below average" = colset_RdBu_5[2],
                  "Average" = colset_RdBu_5[3], "Above average" = colset_RdBu_5[4], 
                  "High" = colset_RdBu_5[5])

dataset_tundra <- ggplot(biome[biome_short_class %in% "Tundra"], aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Tundra")

dataset_desert <- ggplot(biome[biome_short_class %in% "Deserts"], 
                         aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Deserts")

dataset_trop_forest <- ggplot(biome[biome_short_class %in% "T/S Moist BL Forests"], 
                              aes(x = dataset.x, y = dataset.y, fill = fraction_fac))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_cols)+
  labs(fill = "Distribution\nagreement    ", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "T/S Moist BL Forests")


dataset_gg <- ggarrange(dataset_desert, dataset_tundra, dataset_trop_forest, 
                        nrow = 1, ncol = 3,
                        common.legend = T, legend = "bottom", align = "hv",
                        labels = c("c", "d", "e"))

## plot difference to global ----
## colors diff ----
dataset_diff_cols <- c("Lower" =  colset_RdBu_5[1], 
                  "Same" = colset_RdBu_5[3],
                  "Higher" = colset_RdBu_5[5])

dataset_tundra_diff <- ggplot(biome_global[biome_short_class %in% "Tundra"], 
                         aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols)+
  labs(fill = "Distribution agreement\ncompared to global", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Tundra")

dataset_desert_diff <- ggplot(biome_global[biome_short_class %in% "Deserts"], 
                         aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols)+
  labs(fill = "Distribution agreement\ncompared to global", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "Deserts")

dataset_trop_forest_diff <- ggplot(biome_global[biome_short_class %in% "T/S Moist BL Forests"], 
                              aes(x = dataset.x, y = dataset.y, 
                                  fill = agreement))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols)+
  labs(fill = "Distribution agreement\ncompared to global", x = "", y = "")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(strip.text = element_text(colour = 'black'), legend.position = "right")+
  ggtitle(label = "T/S Moist BL Forests")


dataset_diff_gg <- ggarrange(dataset_desert_diff, 
                        dataset_tundra_diff, 
                        dataset_trop_forest_diff, 
                        nrow = 1, ncol = 3,
                        common.legend = T, legend = "bottom", align = "hv",
                        labels = c("f", "g", "h"))

## composite figure ----

ggarrange(top_row, dataset_gg, dataset_diff_gg,
          nrow = 3, heights = c(0.9, 0.6, 0.6), common.legend = F)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig6_dataset_comparison.png"), 
       width = 8, height = 12)


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig6_dataset_comparison.pdf"), 
       width = 10, height = 13)
