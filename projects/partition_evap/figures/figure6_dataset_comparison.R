# tile plots for over and underestimator and best
# heatplots
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/graphics.R')

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
  scale_fill_manual(values = color_global_positioning)+
  theme_fig6 + theme(
        legend.spacing.x = unit(1.5, "cm"),
        legend.spacing.y = unit(1.5, "cm"),
        legend.position = "right")+
  labs(x = "Datasets", y = "Global area fraction [-]", 
       fill = "Relative position to\nensemble mean")+
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

q10 <- quantile(global$area_fraction, 0.1, na.rm = TRUE)
q30 <- quantile(global$area_fraction, 0.3, na.rm = TRUE)
q70 <- quantile(global$area_fraction, 0.7, na.rm = TRUE)
q90 <- quantile(global$area_fraction, 0.9, na.rm = TRUE)

agreement_labels <- c(
  "Low" = paste0("Low (\u2264", round(q10, 2), ")"),
  "Below average" = paste0("Below avg. (", round(q10, 2), "\u2013", round(q30, 2), ")"),
  "Average" = paste0("Average (", round(q30, 2), "\u2013", round(q70, 2), ")"),
  "Above average" = paste0("Above avg. (", round(q70, 2), "\u2013", round(q90, 2), ")"),
  "High" = paste0("High (>", round(q90, 2), ")")
)


global[, fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                             labels = c("Low", "Below average", "Average", "Above average", "High"))]

global_sym <- rbind(
  global[, .(
    dataset.x,
    dataset.y,
    area_fraction,
    fraction_fac
  )],
  global[, .(
    dataset.x = dataset.y,
    dataset.y = dataset.x,
    area_fraction,
    fraction_fac
  )]
)

dataset_order <- as.character(evap_summary_over$dataset)

global_sym[, dataset.x := factor(dataset.x, levels = rev(dataset_order))]
global_sym[, dataset.y := factor(dataset.y, levels = (dataset_order))]

dataset_cols <- c("Low" =  colset_RdBu_5[1], "Below average" = colset_RdBu_5[2],
                  "Average" = colset_RdBu_5[3], "Above average" = colset_RdBu_5[4], 
                  "High" = colset_RdBu_5[5])

 
dataset_global <-  ggplot(global_sym) +
  geom_tile(
    aes(x = dataset.x, y = dataset.y, fill = fraction_fac), 
    color = "white", lwd = 0.8, linetype = 1
  ) +
  scale_fill_manual(
    values = dataset_cols,
    labels = agreement_labels,
    drop = FALSE
  )+
  labs(fill = "Pairwise matching\narea fraction", x = "", y = "") +
  theme_fig6 +
  ggtitle(label = "Global")

top_row <- ggarrange(dataset_global, performance_global, 
                     labels = c("a", "b"), 
                     nrow = 1, 
                     align = "hv", 
                     widths = c(1, 1.3))

## biomes ----
biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))

biome[, fraction_fac := cut(area_fraction, breaks = c(-0.01, q10, q30, q70, q90, 1), 
                            labels = c("Low", "Below average", "Average", "Above average", "High"))]

biome_sym <- rbind(
  biome[, .(
    biome_short_class,
    dataset.x,
    dataset.y,
    area_fraction,
    fraction_fac
  )],
  biome[, .(
    biome_short_class,
    dataset.x = dataset.y,
    dataset.y = dataset.x,
    area_fraction,
    fraction_fac
  )]
)

biome_sym[, dataset.x := factor(dataset.x, levels = rev(dataset_order))]
biome_sym[, dataset.y := factor(dataset.y, levels = dataset_order)]

## biomes diff tog global----

biome_global <- merge(biome, global[,.(dataset.x, dataset.y, area_fraction)], 
                      by = c("dataset.x", "dataset.y"))

biome_global[, area_fraction_diff := area_fraction.y-area_fraction.x]
biome_global[area_fraction_diff > 0.05 , agreement := "Lower in biome"]
biome_global[area_fraction_diff < -0.05 , agreement := "Higher in biome"]
biome_global[is.na(agreement), agreement := "Similar"]

biome_global_sym <- rbind(
  biome_global[, .(
    biome_short_class,
    dataset.x,
    dataset.y,
    area_fraction.x,
    area_fraction.y,
    agreement
  )],
  biome_global[, .(
    biome_short_class,
    dataset.x = dataset.y,
    dataset.y = dataset.x,
    area_fraction.x,
    area_fraction.y,
    agreement
  )]
)

biome_global_sym[, dataset.x := factor(dataset.x, levels = rev(dataset_order))]
biome_global_sym[, dataset.y := factor(dataset.y, levels = dataset_order)]


## plot biome agreement ----

dataset_tundra <- 
  ggplot(biome_sym[biome_short_class %in% "Tundra"])+
  geom_tile(
    aes(x = dataset.x, 
        y = dataset.y, 
        fill = fraction_fac), 
    color = "white", lwd = 0.8, linetype = 1
  ) +
  scale_fill_manual(
    values = dataset_cols,
    labels = agreement_labels,
    drop = FALSE
  )+
  labs(fill = "Pairwise matching\narea fraction", x = "", y = "")+
  theme_fig6 + 
  theme(legend.position = "right")+
  ggtitle(label = "Tundra")

dataset_desert <- ggplot(biome_sym[biome_short_class %in% "Deserts"])+
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
  scale_fill_manual(
    values = dataset_cols,
    labels = agreement_labels,
    drop = FALSE
  )+
  labs(fill = "Pairwise matching\narea fraction", x = "", y = "")+
  theme_fig6 + 
  ggtitle(label = "Deserts")

dataset_trop_forest <- ggplot(biome_sym[biome_short_class %in% "T/S Moist BL Forests"], 
)+
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
  scale_fill_manual(
    values = dataset_cols,
    labels = agreement_labels,
    drop = FALSE
  )+
  labs(fill = "Pairwise matching\narea fraction", x = "", y = "") +
  theme_fig6 + theme(legend.position = "right") +
  ggtitle(label = "T/S Moist BL Forests")


dataset_gg <- ggarrange(dataset_desert, dataset_tundra, dataset_trop_forest, 
                        nrow = 1, ncol = 3,
                        common.legend = T, legend = "bottom", align = "hv",
                        labels = c("c", "d", "e"))

## plot difference to global ----
## colors diff ----


dataset_tundra_diff <- ggplot(biome_global_sym[biome_short_class %in% "Tundra"], 
                         aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols)+
  labs(fill = "Change in matching area fraction vs. global", x = "", y = "")+
  theme_fig6 +  
  ggtitle(label = "Tundra")

dataset_desert_diff <- ggplot(biome_global_sym[biome_short_class %in% "Deserts"], 
                         aes(x = dataset.x, y = dataset.y, fill = agreement))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols) +
  labs(fill = "Change in matching area fraction vs. global", x = "", y = "") +
  theme_fig6 +    
  ggtitle(label = "Deserts")

dataset_trop_forest_diff <- ggplot(biome_global_sym[biome_short_class %in% "T/S Moist BL Forests"], 
                              aes(x = dataset.x, y = dataset.y, 
                                  fill = agreement))+
  geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  scale_fill_manual(values = dataset_diff_cols)+
  labs(fill = "Change in matching area fraction vs. global", x = "", y = "") +
  theme_fig6 +  
  ggtitle(label = "T/S Moist BL Forests")


dataset_diff_gg <- ggarrange(dataset_desert_diff, 
                        dataset_tundra_diff, 
                        dataset_trop_forest_diff, 
                        nrow = 1, ncol = 3,
                        common.legend = T, legend = "bottom", align = "hv",
                        labels = c("f", "g", "h"))

## composite figure ----

fig_6 <- ggarrange(top_row, 
                   dataset_gg, 
          nrow = 2, 
          heights = c(0.56, 0.6), 
          common.legend = F,
          align = "hv")

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, 
              "main/fig6_dataset_comparison.png"), 
       plot = fig_6,
       width = figure_widths, 
       height = 25, 
       unit = "cm")


ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, 
              "main/fig6_dataset_comparison.pdf"), 
       plot = fig_6,
       width = figure_widths, 
       height = 25, 
       unit = "cm",
       device = cairo_pdf)

