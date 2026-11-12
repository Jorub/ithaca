source('source/partition_evap.R')
source('source/graphics.R')
source('source/partition_evap_graphics.R')

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

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Magnitude higher",]

joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Distribution higher",]

joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(evap_quant_v2, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(evap_quant_v2)]

joint_agreement <- unique(joint_evap_agreement[,.(evap_quant_v2, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(evap_quant_v2)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base-0.09]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]
joint_agreement[joint_agreement == "Magnitude higher", x_base := x_base+0.09]
joint_agreement[joint_agreement == "Distribution higher", x_base := x_base+0.18]

## top ----

a_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(evap_quant_v2)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Magnitude agreement")+
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
  scale_fill_manual(values = color_joint_agreement_short, name = "Joint agreement")+
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

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Magnitude higher",]

joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Distribution higher",]

joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(elev_class, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(elev_class)]

joint_agreement <- unique(joint_evap_agreement[,.(elev_class, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(elev_class)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base-0.09]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]
joint_agreement[joint_agreement == "Magnitude higher", x_base := x_base+0.09]
joint_agreement[joint_agreement == "Distribution higher", x_base := x_base+0.18]
## top ----
b_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(elev_class)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Magnitude agreement")+
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
  scale_fill_manual(values = color_joint_agreement_short, name = "Joint agreement")+
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

joint_evap_agreement[rel_dataset_agreement %in% c("High", "Above average")
                     & dist_dataset_agreement %in% c("Low", "Below average"), 
                     joint_agreement := "Magnitude higher",]

joint_evap_agreement[rel_dataset_agreement %in% c("Low", "Below average")
                     & dist_dataset_agreement %in% c("High", "Above average"), 
                     joint_agreement := "Distribution higher",]
joint_evap_agreement[, sum_area_agreement := sum(area_agreement), .(lat_classes, joint_agreement)]
joint_evap_agreement[, area_fraction := sum_area_agreement/sum(area_agreement), .(lat_classes)]

joint_agreement <- unique(joint_evap_agreement[,.(lat_classes, area_fraction, joint_agreement)])
joint_agreement <- joint_agreement[complete.cases(joint_agreement)]

joint_agreement[, x_base := as.numeric(lat_classes)]
joint_agreement[joint_agreement == "Both higher", x_base := x_base-0.09]
joint_agreement[joint_agreement == "Both lower", x_base := x_base-0.18]
joint_agreement[joint_agreement == "Magnitude higher", x_base := x_base+0.09]
joint_agreement[joint_agreement == "Distribution higher", x_base := x_base+0.18]
## top ----
c_top <- ggplot()+
  geom_col(data = q_agreement, aes(x = as.numeric(lat_classes)-0.18, y = area_fraction, 
                                   fill = rel_dataset_agreement),
           width = 0.3) +
  scale_fill_manual(values = color_quartile_agreement,
                    name = "Magnitude agreement")+
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
  scale_fill_manual(values = color_joint_agreement_short, name = "Joint agreement")+
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



fig3 <- ggarrange(a_bottom, b_bottom, c_bottom, nrow = 1, align = "hv", common.legend = T)

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig3_agreement_gradients.png"
  ),
  plot = fig3,
  width = figure_widths,
  height = 20/2,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig3_agreement_gradients.pdf"
  ),
  plot = fig3,
  width = figure_widths,
  height = 20/2,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

