# Figure 2: agreement maps ----
source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")

library(data.table)
library(sf)
library(stars)
library(raster)


# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

dataset_agreement_grid_wise[, rel_dataset_agreement := factor(
  rel_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]

dataset_agreement_grid_wise[, dist_dataset_agreement := factor(
  dist_dataset_agreement,
  levels = agreement_levels,
  ordered = TRUE
)]

dataset_agreement_grid_wise[rel_dataset_agreement %in% c("High", "Above average") & 
                              dist_dataset_agreement %in% c("High", "Above average"), 
                            joint_agreement:= "Both higher"]
dataset_agreement_grid_wise[rel_dataset_agreement %in% c("Low", "Below average") & 
                              dist_dataset_agreement %in% c("Low", "Below average"), 
                            joint_agreement:= "Both lower"]
dataset_agreement_grid_wise[rel_dataset_agreement %in% c("High", "Above average") & 
                              dist_dataset_agreement %in% c("Low", "Below average"), 
                            joint_agreement:= "Magnitude higher \nDistribution lower"]
dataset_agreement_grid_wise[rel_dataset_agreement %in% c("Low", "Below average") & 
                              dist_dataset_agreement %in% c("High", "Above average"), 
                            joint_agreement:= "Magnitude lower \nDistribution higher"]

dataset_agreement_grid_wise[is.na(joint_agreement), joint_agreement := "Average"]

dataset_agreement_grid_wise[, joint_agreement:= as.factor(joint_agreement)]

joint_levels <- levels(dataset_agreement_grid_wise$joint_agreement)




# Panel a: Magnitude agreement ----

## prep ----
to_plot_dt <- dataset_agreement_grid_wise[, .(lon, lat, rel_dataset_agreement,
    value = as.numeric(rel_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, rel_dataset_agreement)]
)

problem_to_val[, rel_dataset_agreement := factor(rel_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  rel_dataset_agreement
]

to_plot_stars$rel_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$rel_dataset_agreement <- factor(
  to_plot_stars$rel_dataset_agreement,
  levels = agreement_levels
)

levels(to_plot_stars$rel_dataset_agreement)

## plot ----
fig_a <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = rel_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Magnitude agreement") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map


# Panel b: distribution agreement ----
## prep ----
to_plot_dt <- dataset_agreement_grid_wise[, .(lon, lat, dist_dataset_agreement,
                                              value = as.numeric(dist_dataset_agreement))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, dist_dataset_agreement)]
)

problem_to_val[, dist_dataset_agreement := factor(dist_dataset_agreement,
                                                 levels = agreement_levels,
                                                 ordered = T)]


to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  dist_dataset_agreement
]

to_plot_stars$dist_dataset_agreement <- array(
  factor(
    rel_vals,
    levels = agreement_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$dist_dataset_agreement <- factor(
  to_plot_stars$dist_dataset_agreement,
  levels = agreement_levels
)

## plot ----
fig_b <- ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = dist_dataset_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_agreement, 
                    breaks = agreement_levels,
                    limits = agreement_levels,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  scale_color_manual(values = color_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Distribution agreement") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map


# Panel c: joint agreement map ----
## prep ----
to_plot_dt <- dataset_agreement_grid_wise[, .(lon, lat, joint_agreement,
                                              value = as.numeric(joint_agreement))
                                          ]

problem_to_val <- unique(
  to_plot_dt[, .(value, joint_agreement)]
)

problem_to_val[, joint_agreement := factor(joint_agreement,
                                                  levels = joint_agreement,
                                                  ordered = T)]


to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[data.table(value = vals),
                           on = "value",
                           joint_agreement
                           ]

to_plot_stars$joint_agreement <- array(
  factor(
    rel_vals,
    levels = joint_levels
  ),
  dim = dim(to_plot_stars$value)
)

to_plot_stars$joint_agreement <- factor(
  to_plot_stars$joint_agreement,
  levels = joint_levels
)


## plot ----
fig_c <-  ggplot() +
  geom_stars(data = to_plot_stars,
             aes(fill = joint_agreement)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_joint_agreement, 
                    breaks = joint_levels ,
                    limits = joint_levels ,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  labs(x = NULL, y = NULL, fill = "",
       title = "Spatial overlap of higher (high and above average) and lower (low and below average) agreement") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map


fig_c <- fig_c + theme(plot.margin = unit(c(1.1, 7, 0.1, 0.1), "cm"),
                       legend.position = "none")

# Panel c: area fraction inset ----
total_area <- dataset_agreement_grid_wise[,
  sum(area, na.rm = TRUE)
]

joint_area_stats <- dataset_agreement_grid_wise[,.(
    area_fraction = sum(area, na.rm = TRUE) / total_area
  ), by = joint_agreement]

### bar plot ----
bar_joint <- ggplot(
  joint_area_stats[!is.na(joint_agreement) & joint_agreement != "Average"], 
  aes(x = joint_agreement, y = area_fraction * 100)
  ) +
  geom_bar(
    aes(fill = joint_agreement),
    stat = "identity",
    width = 0.7
  ) +
  geom_hline(
    yintercept = seq(0, 15, 5),
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = round(area_fraction * 100, digits = 1)),
    hjust = -0.15,
    size = 3
  ) +
  scale_fill_manual(
    values = color_joint_agreement,
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Area fraction [%]"
  ) +
  coord_flip() +
  theme_fig1+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")
  )

# Panel c: merge map and inset ----
bar_joint_grob <- ggplotGrob(bar_joint)

build <- ggplot_build(fig_c)

x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width <- diff(x_range)
y_height <- diff(y_range)

fig_c_bar <- fig_c +
  annotation_custom(
    grob = bar_joint_grob,
    xmin = x_range[2] + 0.02 * x_width,
    xmax = x_range[2] + 0.3 * x_width,
    ymin = y_range[1] + 0.0 * y_height,
    ymax = y_range[1] + 0.5 * y_height
  )

# Figure assembly ----
common_legend <- get_legend(
  fig_a +
    theme(
      legend.position = "right",
      legend.box.margin = margin(t = 2, r = 12, b = 2, l = 2),
      legend.margin = margin(t = 2, r = 12, b = 2, l = 2)
    )
)


fig2_top <- ggarrange(
  fig_a,
  fig_b,
  labels = c("a", "b"),
  font.label = list(size = 14, face = "bold"),
  nrow = 1,
  ncol = 2,
  common.legend = TRUE,
  legend.grob = common_legend,
  legend = "right"
)

fig2 <- ggarrange(
  fig2_top,
  fig_c_bar,
  labels = c("", "c"),
  font.label = list(size = 14, face = "bold"),
  nrow = 2,
  ncol = 1,
  heights = c(1, 1.4)
)

fig2

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig2_agreement_maps.png"
  ),
  plot = fig2,
  width = figure_widths,
  height = 30,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig2_agreement_maps.pdf"
  ),
  plot = fig2,
  width = figure_widths,
  height = 23,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

