source('source/partition_evap.R')
source('source/graphics.R')

library(ggpubr)
library(ggnewscale)
library(cowplot)
library(grid)
library(scales)


# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
dataset_agreement_grid_wise[, lat_classes := cut(lat, seq(-60, 90, 30))]

## labels ----
evap_quant_labels <- levels(dataset_agreement_grid_wise$evap_quant)
elev_labels <- levels(dataset_agreement_grid_wise$elev_class)
lat_labels <- levels(dataset_agreement_grid_wise$lat_classes)

## summarize evap quantiles ----
dataset_agreement_grid_wise[evap_quant %in% evap_quant_labels[c(1,2)], evap_quant_v2 := "0-0.2"]
dataset_agreement_grid_wise[evap_quant %in% evap_quant_labels[c(3,4)], evap_quant_v2 := "0.2-0.4"]
dataset_agreement_grid_wise[evap_quant %in% evap_quant_labels[c(5,6)], evap_quant_v2 := "0.4-0.6"]
dataset_agreement_grid_wise[evap_quant %in% evap_quant_labels[c(7,8)], evap_quant_v2 := "0.6-0.8"]
dataset_agreement_grid_wise[evap_quant %in% evap_quant_labels[c(9,10)], evap_quant_v2 := "0.8-1"]
dataset_agreement_grid_wise[, evap_quant_v2 := as.factor(evap_quant_v2)]
evap_quant_labels_v2 <- levels(dataset_agreement_grid_wise$evap_quant_v2)

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


## colors ----

color_quartile_agreement <- c(
  "Low" = "#6A51A3",
  "Below average" = "#B2ABD2",
  "Average" = "#F7F7F7",
  "Above average" = "#92C5DE",
  "High" = "#0571B0"
)

color_distribution_agreement <- c(
  "Low" = "#A6611A",
  "Below average" = "#DFC27D",
  "Average" = "#F7F7F7",
  "Above average" = "#80CDC1",
  "High" = "#018571"
)

color_joint <- c("Both higher" = "#4D648D", "Both lower" = "#A63A3A")

# panel a ----
## Calculate area fractions ----
q_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(evap_quant_v2, rel_dataset_agreement)]
q_agreement <- q_agreement[complete.cases(q_agreement)]
q_agreement[, area_fraction := area_agreement/sum(area_agreement), evap_quant_v2]

d_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(evap_quant_v2, dist_dataset_agreement)]
d_agreement <- d_agreement[complete.cases(d_agreement)]
d_agreement[, area_fraction := area_agreement/sum(area_agreement), evap_quant_v2]

joint_evap_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(evap_quant_v2, rel_dataset_agreement, dist_dataset_agreement)]
joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Both lower",]

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Both higher",]
joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(evap_quant_v2, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(evap_quant_v2)]

joint_agreement <- unique(joint_evap_agreement[,.(evap_quant_v2, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(evap_quant_v2)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base+0.18]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]

## top ----
a_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(evap_quant_v2)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Quartile agreement")+
  ggnewscale::new_scale_fill() +
  geom_col(data = d_agreement, aes(x = as.numeric(evap_quant_v2)+0.18, y = area_fraction, 
                                   fill = dist_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_distribution_agreement,
                    name = "Distribution agreement")+
  scale_x_continuous(
    breaks = 1:5,
    labels = evap_quant_labels_v2,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Evapotranspiration quantiles",
    y = "Area fraction [%]",
    title = "ET quantiles"
  ) +
  theme_fig3

## joint agreement ----

a_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint, name = "Joint agreement")+
  scale_x_continuous(
    breaks = 1:5,
    labels = evap_quant_labels_v2,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.5, 0.2),
    limits = c(0, 0.55),
    expand = c(0, 0)
  ) +
  labs(
    x = "Evapotranspiration quantiles",
    y = "Area fraction [%]",
  ) +
  theme_fig3

## ggarrange ----

panel_a <- ggarrange(a_top, a_bottom, nrow = 2,
                     align = "v",
                     heights = c(1.3, 1), legend = "none")

# panel b -----
## Calculate area fractions ----
q_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(elev_class, rel_dataset_agreement)]
q_agreement <- q_agreement[complete.cases(q_agreement)]
q_agreement[, area_fraction := area_agreement/sum(area_agreement), elev_class]

d_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(elev_class, dist_dataset_agreement)]
d_agreement <- d_agreement[complete.cases(d_agreement)]
d_agreement[, area_fraction := area_agreement/sum(area_agreement), elev_class]

joint_evap_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(elev_class, rel_dataset_agreement, dist_dataset_agreement)]
joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Both lower",]

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Both higher",]
joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(elev_class, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(elev_class)]

joint_agreement <- unique(joint_evap_agreement[,.(elev_class, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(elev_class)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base+0.18]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]

## top ----
b_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(elev_class)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Quartile agreement")+
  ggnewscale::new_scale_fill() +
  geom_col(data = d_agreement, aes(x = as.numeric(elev_class)+0.18, y = area_fraction, 
                                   fill = dist_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_distribution_agreement,
                    name = "Distribution agreement")+
  scale_x_continuous(
    breaks = 1:6,
    labels = elev_labels,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Elevation classes [m]",
    y = "Area fraction [%]",
    title = "Elevation"
  ) +
  theme_fig3 +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))

## joint agreement ----

b_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint, name = "Joint agreement")+
  scale_x_continuous(
    breaks = 1:6,
    labels = elev_labels,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.5, 0.2),
    limits = c(0, 0.55),
    expand = c(0, 0)
  ) +
  labs(
    x = "Elevation classes [m]",
    y = "Area fraction [%]",
  ) +
  theme_fig3 +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))


## ggarrange ----

panel_b <- ggarrange(b_top, b_bottom, nrow = 2,
                     align = "v",
                     heights = c(1.3, 1), legend = "none")

# panel c ----

q_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), 
                                           .(lat_classes, rel_dataset_agreement)]
q_agreement <- q_agreement[complete.cases(q_agreement)]
q_agreement[, area_fraction := area_agreement/sum(area_agreement), 
            lat_classes]

d_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(lat_classes, dist_dataset_agreement)]
d_agreement <- d_agreement[complete.cases(d_agreement)]
d_agreement[, area_fraction := area_agreement/sum(area_agreement), lat_classes]

joint_evap_agreement <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(lat_classes, rel_dataset_agreement, dist_dataset_agreement)]
joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Both lower",]

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Both higher",]
joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(lat_classes, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(lat_classes)]

joint_agreement <- unique(joint_evap_agreement[,.(lat_classes, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(lat_classes)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base+0.18]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]

## top ----
c_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(lat_classes)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Quartile agreement")+
  ggnewscale::new_scale_fill() +
  geom_col(data = d_agreement, aes(x = as.numeric(lat_classes)+0.18, y = area_fraction, 
                                   fill = dist_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_distribution_agreement,
                    name = "Distribution agreement")+
  scale_x_continuous(
    breaks = 1:5,
    labels = lat_labels,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Latitude classes [°]",
    y = "Area fraction [%]",
    title = "Latitude"
  ) +
  theme_fig3

## joint agreement ----

c_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint, name = "Joint agreement")+
  scale_x_continuous(
    breaks = 1:5,
    labels = lat_labels,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.5, 0.2),
    limits = c(0, 0.55),
    expand = c(0, 0)
  ) +
  labs(
    x = "Latitude classes [°]",
    y = "Area fraction [%]",
  ) +
  theme_fig3

## ggarrange ----

panel_c <- ggarrange(c_top, c_bottom, nrow = 2,
                     align = "v",
                     heights = c(1.3, 1), legend = "none")

# legend panel ----

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

joint_levels <- c(
  "Both higher",
  "Both lower"
)

dummy_quartile <- data.table(
  x = agreement_levels,
  y = 1,
  rel_dataset_agreement = factor(
    agreement_levels,
    levels = agreement_levels
  )
)

dummy_distribution <- data.table(
  x = agreement_levels,
  y = 1,
  dist_dataset_agreement = factor(
    agreement_levels,
    levels = agreement_levels
  )
)

dummy_joint <- data.table(
  x = joint_levels,
  y = 1,
  joint_agreement = factor(
    joint_levels,
    levels = joint_levels
  )
)

legend_theme <- theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.x = unit(0.10, "cm"),
    legend.margin = margin(t = 0, r = 2, b = 0, l = 2),
    legend.box.margin = margin(t = 0, r = 2, b = 0, l = 2)
  )


p_legend_quartile <- ggplot(
  dummy_quartile,
  aes(x = x, y = y, fill = rel_dataset_agreement)
) +
  geom_col() +
  scale_fill_manual(
    values = color_quartile_agreement,
    limits = agreement_levels,
    breaks = agreement_levels,
    drop = FALSE,
    name = "Quartile agreement",
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  legend_theme

p_legend_distribution <- ggplot(
  dummy_distribution,
  aes(x = x, y = y, fill = dist_dataset_agreement)
) +
  geom_col() +
  scale_fill_manual(
    values = color_distribution_agreement,
    limits = agreement_levels,
    breaks = agreement_levels,
    drop = FALSE,
    name = "Distribution agreement",
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  legend_theme

p_legend_joint <- ggplot(
  dummy_joint,
  aes(x = x, y = y, fill = joint_agreement)
) +
  geom_col() +
  scale_fill_manual(
    values = color_joint,
    limits = joint_levels,
    breaks = joint_levels,
    drop = FALSE,
    name = "Joint agreement",
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  legend_theme

get_bottom_legend <- function(p) {
  g <- ggplotGrob(p)
  
  guide_id <- which(g$layout$name == "guide-box-bottom")
  
  if (length(guide_id) == 0) {
    stop("No bottom legend found. Check that legend.position = 'bottom'.")
  }
  
  legend <- g$grobs[[guide_id]]
  
  return(legend)
}

legend_quartile <- get_bottom_legend(p_legend_quartile)
legend_distribution <- get_bottom_legend(p_legend_distribution)
legend_joint <- get_bottom_legend(p_legend_joint)


common_legend <- cowplot::plot_grid(
  cowplot::ggdraw(legend_quartile),
  cowplot::ggdraw(legend_distribution),
  cowplot::ggdraw(legend_joint),
  nrow = 1,
  rel_widths = c(1.35, 1.55, 0.75),
  align = "h"
)


common_legend_v2 <- cowplot::plot_grid(
  cowplot::ggdraw(legend_quartile),
  cowplot::ggdraw(legend_distribution),
  cowplot::ggdraw(legend_joint),
  nrow = 3,
  rel_widths = c(1.35, 1.55, 0.75),
  align = "hv"
)

common_legend_v3 <- cowplot::plot_grid(
  cowplot::ggdraw(legend_quartile),
  cowplot::ggdraw(legend_distribution),
  nrow = 2,
  align = "hv"
)

# plot all ----

panel_title_top <- ggdraw() +
  draw_label(
    "Quartile and distribution agreement across gradients",
    x = 0,
    hjust = 0,
    fontface = "bold",
    size = 11
  )

panel_title_bottom <- ggdraw() +
  draw_label(
    "Spatial overlap of agreement metrics across gradients",
    x = 0,
    hjust = 0,
    fontface = "bold",
    size = 11
  )

fig3_grid_top <- cowplot::plot_grid(
  a_top, b_top,  c_top, 
  ncol = 3,
  align = "hv",
  labels = c("a", "b", "c"),
  label_fontface = "bold",
  label_size = 11
)

fig3_grid_top_legend <- cowplot::plot_grid(
  fig3_grid_top,
  common_legend_v3,
  ncol = 2,
  rel_widths = c(1, 0.15)
)

fig3_grid_bottom <- cowplot::plot_grid(
  a_bottom, b_bottom,  c_bottom, 
  ncol = 3,
  align = "hv",
  labels = c("d", "e", "f"),
  label_fontface = "bold",
  label_size = 11
)

fig3_grid_bottom_legend <- cowplot::plot_grid(
  fig3_grid_bottom,
  cowplot::ggdraw(legend_joint),
  ncol = 2,
  rel_widths = c(1, 0.15)
)

fig3 <- cowplot::plot_grid(
  panel_title_top,
  fig3_grid_top_legend,
  panel_title_bottom,
  fig3_grid_bottom_legend,
  nrow = 4,
  rel_heights = c(0.1, 1, 0.1, 0.6)
)

fig3

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig3_agreement_gradients.png"
  ),
  plot = fig3,
  width = 1.5*20,
  height = 20,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig3_agreement_gradients.pdf"
  ),
  plot = fig3,
  width = 1.5*20,
  height = 20,
  units = "cm",
  dpi = 300
)

