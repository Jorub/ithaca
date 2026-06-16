source('source/partition_evap.R')
source('source/graphics.R')

library(ggpubr)
library(ggnewscale)
library(cowplot)

# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))

# plot ----
## theme ----
theme_fig3 <- theme_bw(base_size = 11) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )