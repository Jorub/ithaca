source('source/partition_evap.R')
source('source/graphics.R')
source('source/partition_evap_graphics.R')
source('projects/partition_evap/figures/figure4_SI_fingerprints_function.R')

library(tibble)
library(data.table)
library(sf)
library(stars)
library(raster)

# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))

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

# plot ----
## label ----
top_row <- " "

block_labels <- tibble(
  x_pos = c(2.5),
  environment = top_row,
  label = c(
    "Spatial overlap between metrics"
  )
)

## gg ----

fig4 <- ggplot()+
  geom_tile(data = environments_blue, 
            aes(y = environment, x = x_pos, fill = area_fraction),
            width = 0.95, color = "gray25")+
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
            width = 0.95, color = "gray25")+
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
            width = 0.95, color = "gray25")+
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
            width = 0.95, color = "gray25")+
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
  geom_text(
    data = block_labels,
    aes(x = x_pos, y = environment, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 1:4,
    labels = environments_level
  ) +
  scale_y_discrete(
    limits = c(environments, top_row),
    labels = function(x) ifelse(x == top_row, "", x)
  ) +
  
  scale_colour_manual(
    values = c(`FALSE` = "black", `TRUE` = "white"),
    guide = "none"
  ) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
           fill = NA, colour = "black", linewidth = 0.8) +
  theme_fig4+
  theme(legend.position = "none")+
  labs(y = "Environment", x = NULL,
       title = "Environmental fingerprints of product agreement")

fig4

### maps -----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

environments <- c(
  "BWh", "ARP", "Barren", "Deserts",
  "ET", "TIB", "Shrublands", "Tundra",
  "Af",  "NWS", "Forests", "T/S Forests",
  "BSh", "EAU", "Savannas", "Mediterranean"
)

environment_colours <- c(
  # Distribution agreement higher
  "BWh"           = "#E2A374",
  "ARP"           = "#E2A374",
  "Barren"        = "#E2A374",
  "Deserts"       = "#E2A374",
  
  # Both lower
  "ET"           = "#A63A3A",
  "TIB"           = "#A63A3A",
  "Shrublands"      = "#A63A3A",
  "Tundra"        = "#A63A3A",
  
  # Magnitude agreement higher
  "Af"            = "#6E5773",
  "NWS"           = "#6E5773",
  "Forests"       = "#6E5773",
  "T/S Forests"   = "#6E5773",
  
  # Both higher
  "BSh"           = "#4D648D",
  "EAU"           = "#4D648D",
  "Savannas"      = "#4D648D",
  "Mediterranean" = "#4D648D"
)

evap_mask[, biome_short_class := as.factor(biome_short_class)]

to_plot_dt <- evap_mask[biome_short_class %in% environments, .(lon, lat, biome_short_class,
                                                               value = as.numeric(biome_short_class))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, biome_short_class)]
)

to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  biome_short_class
]

biome_levels <- unique(evap_mask$biome_short_class)

to_plot_stars$biome_short_class <- array(
  factor(
    rel_vals,
    levels = biome_levels
  ),
  dim = dim(to_plot_stars$value)
)

fig_biome <- ggplot() +
  geom_stars(data = to_plot_stars, aes(color = biome_short_class, fill = biome_short_class)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = environment_colours,
                    na.value = "white",
                    na.translate = FALSE,
                    drop = TRUE) + 
  scale_color_manual(values = environment_colours,
                     guide = "none",
                     na.value = "white") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  ggtitle("Biome")+
  theme_map_SI

to_plot_dt <- evap_mask[land_cover_short_class %in% environments, .(lon, lat, land_cover_short_class,
                                                                    value = as.numeric(land_cover_short_class))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, land_cover_short_class)]
)

to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  land_cover_short_class
]

land_levels <- unique(evap_mask$land_cover_short_class)

to_plot_stars$land_cover_short_class <- array(
  factor(
    rel_vals,
    levels = land_levels
  ),
  dim = dim(to_plot_stars$value)
)

fig_land <- ggplot() +
  geom_stars(data = to_plot_stars, aes(color = land_cover_short_class, fill = land_cover_short_class)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = environment_colours,
                    na.value = "white",
                    na.translate = FALSE,
                    drop = TRUE) + 
  scale_color_manual(values = environment_colours,
                     guide = "none",
                     na.value = "white") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  ggtitle("Landcover")+
  theme_map_SI


to_plot_dt <- evap_mask[KG_beck %in% environments, .(lon, lat, KG_beck,
                                                     value = as.numeric(KG_beck))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, KG_beck)]
)

to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  KG_beck
]

KG_beck_levels <- unique(evap_mask$KG_beck)

to_plot_stars$KG_beck <- array(
  factor(
    rel_vals,
    levels = KG_beck_levels
  ),
  dim = dim(to_plot_stars$value)
)

fig_KG_beck <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = KG_beck)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = environment_colours,
                    na.value = "white",
                    na.translate = FALSE,
                    drop = TRUE) + 
  scale_color_manual(values = environment_colours,
                     guide = "none",
                     na.value = "white") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  ggtitle("Köppen Geiger classes (Beck et al. 2018)")+
  theme_map_SI


to_plot_dt <- evap_mask[IPCC_ref_region %in% environments, .(lon, lat, IPCC_ref_region,
                                                             value = as.numeric(IPCC_ref_region))
]

problem_to_val <- unique(
  to_plot_dt[, .(value, IPCC_ref_region)]
)

to_plot_stars <- to_plot_dt[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars()

vals <- as.vector(to_plot_stars$value)

rel_vals <- problem_to_val[
  data.table(value = vals),
  on = "value",
  IPCC_ref_region
]

IPCC_ref_region_levels <- unique(evap_mask$IPCC_ref_region)

to_plot_stars$IPCC_ref_region <- array(
  factor(
    rel_vals,
    levels = IPCC_ref_region_levels
  ),
  dim = dim(to_plot_stars$value)
)

fig_IPCC_ref_region <- ggplot() +
  geom_stars(data = to_plot_stars, aes(fill = IPCC_ref_region)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_manual(values = environment_colours,
                    na.value = "white",
                    na.translate = FALSE,
                    drop = TRUE) + 
  scale_color_manual(values = environment_colours,
                     guide = "none",
                     na.value = "white") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(ylim = c(-70, 90), expand = F)+
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  ggtitle("IPCC regions")+
  theme_map_SI

## ggarrange
fig_env <- ggarrange(fig_biome, fig_land, fig_IPCC_ref_region, fig_KG_beck,
                     nrow = 2, ncol = 2, align = "v", legend = "bottom")

fig_4_env <- ggarrange(fig4, fig_env, ncol = 2, labels = c("a", "b"),
                       widths = c(1.2, 1.6))

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig4_fingerprint.pdf"
  ),
  plot = fig_4_env,
  width = figure_widths,
  height = 15,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
)

