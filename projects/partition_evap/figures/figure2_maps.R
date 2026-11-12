# Figure 2: agreement maps, environmental fingerprints and gradients ----
library(data.table)
library(sf)
library(stars)
library(raster)
library(tibble)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("source/geo_functions.R")
source("source/graphics.R")
source("projects/partition_evap/figures/figure2_SI_fingerprints_function.R")

# Configuration ----
figure_height <- 36
joint_levels_short <- c("Both higher", "Both lower",
                        "Distribution higher", "Magnitude higher")

# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"
))
stopifnot(!anyDuplicated(dataset_agreement_grid_wise[, .(lon, lat)]))

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

dataset_agreement_grid_wise[, joint_agreement := NA_character_]

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
             aes(fill = rel_dataset_agreement), linewidth = 0) +
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
             aes(fill = dist_dataset_agreement), linewidth = 0) +
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
             aes(fill = joint_agreement), linewidth = 0) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = color_joint_agreement,
                    breaks = joint_levels ,
                    limits = joint_levels ,
                    drop = FALSE,
                    na.value = "transparent",
                    na.translate = FALSE) +
  labs(x = NULL, y = NULL, fill = "",
       title = "Joint magnitude and distribution agreement") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map


fig_c <- fig_c + theme(legend.position = "none")

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

# Panel d: environmental fingerprints ----
joint_land <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(land_cover_short_class, rel_dataset_agreement, dist_dataset_agreement)]
joint_land[, environment := land_cover_short_class]
joint_land[, group := "Landcover"]
joint_land[, land_cover_short_class := NULL]
joint_land <- joint_land[complete.cases(joint_land)]

joint_biome <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(biome_short_class, rel_dataset_agreement, dist_dataset_agreement)]
joint_biome[, environment := biome_short_class]
joint_biome[, group := "Biome"]
joint_biome[, biome_short_class := NULL]
joint_biome <- joint_biome[complete.cases(joint_biome)]

joint_KG <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(KG_beck, rel_dataset_agreement, dist_dataset_agreement)]
joint_KG[, environment := KG_beck]
joint_KG[, group := "Koeppen Geiger"]
joint_KG[, KG_beck := NULL]
joint_KG <- joint_KG[complete.cases(joint_KG)]

joint_IPCC <- dataset_agreement_grid_wise[, .(area_agreement = sum(area)), .(IPCC_ref_region, rel_dataset_agreement, dist_dataset_agreement)]
joint_IPCC[, environment := IPCC_ref_region]
joint_IPCC[, group := "IPCC regions"]
joint_IPCC[, IPCC_ref_region := NULL]
joint_IPCC <- joint_IPCC[complete.cases(joint_IPCC)]

joint_evap_agreement <- merge(joint_land, joint_biome, all = T,
                              by = c("group", "environment",
                                     "area_agreement",
                                     "rel_dataset_agreement",
                                     "dist_dataset_agreement"))

joint_evap_agreement <- merge(joint_evap_agreement, joint_KG, all = T,
                              by = c("group", "environment",
                                     "area_agreement",
                                     "rel_dataset_agreement",
                                     "dist_dataset_agreement"))

joint_evap_agreement <- merge(joint_evap_agreement, joint_IPCC, all = T,
                              by = c("group", "environment",
                                     "area_agreement",
                                     "rel_dataset_agreement",
                                     "dist_dataset_agreement"))

joint_evap_agreement[, area_fraction := area_agreement/sum(area_agreement), .(environment, group)]

lower <- c("Low", "Below average")
higher <- c("High", "Above average")

joint_evap_agreement_lower <- joint_evap_agreement[rel_dataset_agreement %in% lower &
                                                     dist_dataset_agreement %in% lower]

joint_evap_agreement_lower_summary <-
  joint_evap_agreement_lower[,.(area_fraction = sum(area_fraction),
                                area = sum(area_agreement)),
                             .(environment, group)]


joint_evap_agreement_higher <- joint_evap_agreement[rel_dataset_agreement %in% higher &
                                                      dist_dataset_agreement %in% higher]

joint_evap_agreement_higher_summary <-
  joint_evap_agreement_higher[, .(area_fraction = sum(area_fraction),
                                  area = sum(area_agreement)), .(environment, group)]


contrast_evap_agreement_d_higher <- joint_evap_agreement[rel_dataset_agreement %in% lower &
                                                           dist_dataset_agreement %in% higher]

contrast_evap_agreement_d_higher_summary <-
  contrast_evap_agreement_d_higher[, .(area_fraction = sum(area_fraction),
                                       area = sum(area_agreement)),
                                   .(environment, group)]

contrast_evap_agreement_q_higher <- joint_evap_agreement[rel_dataset_agreement %in% higher &
                                                           dist_dataset_agreement %in% lower]

contrast_evap_agreement_q_higher_summary <-
  contrast_evap_agreement_q_higher[, .(area_fraction = sum(area_fraction),
                                       area = sum(area_agreement)), .(environment, group)]


environments <- c(
  "BWh", "ARP", "Barren", "Deserts",
  "ET", "TIB", "Shrublands", "Tundra",
  "Af",  "NWS", "Forests", "T/S Forests",
  "BSh", "EAU", "Savannas", "Mediterranean"
)

group_names <- unique(joint_evap_agreement$group)

all_order <- make_environment_order(
  joint_evap_agreement,
  group_name = group_names
)

environments_order <- as.character(all_order[all_order %in% environments])

environments <- rev(environments_order)

environments_select <- joint_evap_agreement[environment %in% environments]

environments_select[, environment := factor(environment, levels = environments,
                                            ordered = T)]
environments_select[,unique(environment)]

environments_select_higher <- environments_select[rel_dataset_agreement %in% higher &
                                                    dist_dataset_agreement %in%  higher,
                                                  .(x = "Both higher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_higher <- merge(environments_select_higher,
                                    environments_select[,.(environment = unique(environment))],
                                    all = T, by = "environment")
environments_select_higher[is.na(area_fraction), area_fraction := 0]
environments_select_higher[is.na(x), x := "Both higher"]
environments_select_higher[, x_pos := 1]

environments_select_lower <- environments_select[rel_dataset_agreement %in% lower &
                                                   dist_dataset_agreement %in%  lower,
                                                 .(x = "Both lower", area_fraction = sum(area_fraction)), .(environment)]
environments_select_lower <- merge(environments_select_lower,
                                   environments_select[,.(environment = unique(environment))],
                                   all = T, by = "environment")
environments_select_lower[is.na(area_fraction), area_fraction := 0]
environments_select_lower[is.na(x), x := "Both lower"]
environments_select_lower[, x_pos := 2]

environments_select_q_higher <- environments_select[rel_dataset_agreement %in% higher &
                                                      dist_dataset_agreement %in% lower,
                                                    .(x = "Magnitude \nhigher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_q_higher <- merge(environments_select_q_higher,
                                      environments_select[,.(environment = unique(environment))],
                                      all = T, by = "environment")
environments_select_q_higher[is.na(area_fraction), area_fraction := 0]
environments_select_q_higher[is.na(x), x := "Magnitude \nhigher"]
environments_select_q_higher[, x_pos := 3]

environments_select_d_higher <- environments_select[rel_dataset_agreement %in% lower &
                                                      dist_dataset_agreement %in% higher,
                                                    .(x = "Distribution \nhigher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_d_higher <- merge(environments_select_d_higher,
                                      environments_select[,.(environment = unique(environment))],
                                      all = T, by = "environment")
environments_select_d_higher[is.na(area_fraction), area_fraction := 0]
environments_select_d_higher[is.na(x), x := "Distribution \nhigher"]
environments_select_d_higher[, x_pos := 4]



environments_joined <- rbind(environments_select_higher,
                             environments_select_lower,
                             environments_select_q_higher,
                             environments_select_d_higher
)

environments_level <- environments_joined[, unique(as.factor(x))]

environments_blue <- rbind(
  environments_select_higher
)

environments_red <- rbind(
  environments_select_lower
)

environments_purple <- rbind(environments_select_q_higher)

environments_teal <- rbind(environments_select_d_higher)

# Fingerprint matrix ----
## gg ----

fig_fingerprints <- ggplot()+
  geom_tile(data = environments_blue,
            aes(y = environment, x = x_pos, fill = area_fraction),
            width = 0.98, color = "gray80", linewidth = 0.25)+
  geom_text(
    data = environments_blue,
    aes(
      y = environment,
      x = x_pos,
      label = percent(area_fraction, accuracy = 1),
      colour = area_fraction > 0.55
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#4D648D",
    name = "High agreement",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1)
  )+
  ggnewscale::new_scale_fill() +
  geom_tile(data = environments_red,
            aes(y = environment, x = x_pos, fill = area_fraction),
            width = 0.98, color = "gray80", linewidth = 0.25)+
  geom_text(
    data = environments_red,
    aes(
      y = environment,
      x = x_pos,
      label = percent(area_fraction, accuracy = 1),
      colour = area_fraction > 0.55
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#A63A3A",
    name = "Low agreement",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1)
  )+
  ggnewscale::new_scale_fill() +
  geom_tile(data = environments_purple,
            aes(y = environment, x = x_pos, fill = area_fraction),
            width = 0.98, color = "gray80", linewidth = 0.25)+
  geom_text(
    data = environments_purple,
    aes(
      y = environment,
      x = x_pos,
      label = percent(area_fraction, accuracy = 1),
      colour = area_fraction > 0.55
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#6E5773",
    name = "Contrasting agreement",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1)
  )+
  ggnewscale::new_scale_fill() +
  geom_tile(data = environments_teal,
            aes(y = environment, x = x_pos, fill = area_fraction),
            width = 0.98, color = "gray80", linewidth = 0.25)+
  geom_text(
    data = environments_teal,
    aes(
      y = environment,
      x = x_pos,
      label = percent(area_fraction, accuracy = 1),
      colour = area_fraction > 0.65
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#E2A374",
    name = "Contrasting agreement",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1)
  )+
  scale_x_continuous(
    breaks = 1:4,
    labels = environments_level
  ) +
  scale_y_discrete(limits = environments, expand = c(0, 0)) +
  
  scale_colour_manual(
    values = c(`FALSE` = "black", `TRUE` = "white"),
    guide = "none"
  ) +
  theme_fig4+
  theme(legend.position = "none")+
  labs(y = "Environment", x = NULL,
       title = "Environmental fingerprints of product agreement")

# Panels e-g: joint agreement across gradients ----
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


## Evapotranspiration ----
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

a_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint_agreement_short,
                    limits = joint_levels_short,
                    breaks = joint_levels_short,
                    drop = FALSE, name = "Joint agreement")+
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

## Elevation ----
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

b_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint_agreement_short,
                    limits = joint_levels_short,
                    breaks = joint_levels_short,
                    drop = FALSE, name = "Joint agreement")+
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

## Latitude ----
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

c_bottom <- ggplot(joint_agreement)+
  geom_col(aes(x = x_base, y = area_fraction, fill = joint_agreement))+
  scale_fill_manual(values = color_joint_agreement_short,
                    limits = joint_levels_short,
                    breaks = joint_levels_short,
                    drop = FALSE, name = "Joint agreement")+
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

# Figure styling ----
panel_theme <- theme(
  plot.title = element_text(size = 11, face = "bold", hjust = 0),
  plot.title.position = "plot",
  plot.subtitle = element_text(size = 9, colour = "gray40"),
  plot.margin = margin(6, 8, 6, 8)
)

fig_a <- fig_a + labs(title = "a  Magnitude agreement") + panel_theme
fig_b <- fig_b + labs(title = "b  Distribution agreement") + panel_theme
fig_c <- fig_c +
  labs(title = "c  Joint magnitude and distribution agreement",
       subtitle = paste(
         "Higher: high and above average;",
         "lower: low and below average"
       )) +
  panel_theme

fig_fingerprints <- fig_fingerprints +
  labs(title = "d  Environmental fingerprints",
       subtitle = "Area within each environment (%)", y = NULL) +
  panel_theme +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, colour = "gray25"),
    axis.text.y = element_text(size = 9, colour = "gray25")
  )

gradient_theme <- panel_theme + theme(
  panel.border = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = "gray90", linewidth = 0.25),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "gray55", linewidth = 0.25),
  axis.text = element_text(size = 9, colour = "gray25"),
  axis.title = element_text(size = 9),
  legend.position = "none"
)

a_bottom <- a_bottom +
  labs(title = "e  Evapotranspiration", y = "Area fraction (%)") +
  gradient_theme
b_bottom <- b_bottom +
  labs(title = "f  Elevation", y = "Area fraction (%)") +
  gradient_theme
c_bottom <- c_bottom +
  labs(title = "g  Latitude", y = "Area fraction (%)") +
  gradient_theme

# Keep the joint-map key in the same order as the fingerprint columns.
joint_key_order <- c(
  "Both higher", "Both lower",
  "Magnitude higher \nDistribution lower",
  "Magnitude lower \nDistribution higher"
)

bar_joint <- bar_joint +
  scale_x_discrete(limits = rev(joint_key_order)) +
  scale_y_continuous(breaks = seq(0, 15, 5),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Global area (%)", caption = "Grey: other combinations") +
  panel_theme +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8, colour = "gray25"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, colour = "gray40", hjust = 0)
  )

# Figure assembly ----
common_legend <- get_legend(
  fig_a +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.height = unit(0.45, "cm"),
      legend.key.width = unit(0.35, "cm"),
      legend.margin = margin(0, 4, 0, 0)
    )
)

fig2_top <- ggarrange(
  fig_a, fig_b,
  nrow = 1, ncol = 2,
  common.legend = TRUE,
  legend.grob = common_legend,
  legend = "right"
)

fig2_joint <- ggarrange(
  fig_c,
  ggarrange(NULL, bar_joint, NULL,
            nrow = 3, heights = c(1, 1.25, 1)),
  ncol = 2, widths = c(4.3, 1)
)

# The joint-map key serves all four joint-agreement displays.
fig2_gradients <- ggarrange(
  a_bottom, b_bottom, c_bottom,
  nrow = 3, align = "v", legend = "none"
)

fig2_environments <- ggarrange(
  fig_fingerprints, fig2_gradients,
  ncol = 2, widths = c(1, 1.3), align = "h"
)

# The full-width map needs about twice the height of either top-row map.
fig2 <- ggarrange(
  fig2_top, fig2_joint, fig2_environments,
  nrow = 3, ncol = 1,
  heights = c(1, 1.9, 2.15)
)

# Save figure ----
ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig2_agreement_maps.png"),
  plot = fig2,
  width = figure_widths,
  height = figure_height,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
                    "main/fig2_agreement_maps.pdf"),
  plot = fig2,
  width = figure_widths,
  height = figure_height,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
  bg = "white"
)

