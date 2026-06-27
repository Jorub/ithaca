source('source/partition_evap.R')
source('source/graphics.R')
source('source/partition_evap_graphics.R')
source('projects/partition_evap/figures/figure4_SI_fingerprints_function.R')

library(tibble)

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
  "Cfc", "TIB", "Snow/Ice", "Tundra",
  "Af",  "NWS", "T/S Forests", "Forests",
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

environments_select_q_high <- environments_select[rel_dataset_agreement == "High", 
                                                  .(x = "Magnitude high", area_fraction = sum(area_fraction)), .(environment)]
environments_select_q_high <- merge(environments_select_q_high, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_q_high[is.na(area_fraction), area_fraction := 0]
environments_select_q_high[is.na(x), x := "Magnitude high"]
environments_select_q_high[, x_pos := 1]


environments_select_q_low <- environments_select[rel_dataset_agreement == "Low", 
                                                 .(x = "Magnitude low",  area_fraction = sum(area_fraction)), .(environment)]
environments_select_q_low <- merge(environments_select_q_low, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_q_low [is.na(area_fraction), area_fraction := 0]
environments_select_q_low [is.na(x), x := "Magnitude low"]
environments_select_q_low [, x_pos := 2]

environments_select_d_high <- environments_select[dist_dataset_agreement == "High", 
                                                  .(x = "Distribution high", area_fraction = sum(area_fraction)), .(environment)]
environments_select_d_high <- merge(environments_select_d_high, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_d_high[is.na(area_fraction), area_fraction := 0]
environments_select_d_high[is.na(x), x := "Distribution high"]
environments_select_d_high[, x_pos := 3]

environments_select_d_low <- environments_select[dist_dataset_agreement == "Low", 
                                                 .(x = "Distribution low", area_fraction = sum(area_fraction)), .(environment)]
environments_select_d_low <- merge(environments_select_d_low, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_d_low[is.na(area_fraction), area_fraction := 0]
environments_select_d_low[is.na(x), x := "Distribution low"]
environments_select_d_low[, x_pos := 4]

environments_select_higher <- environments_select[rel_dataset_agreement %in% higher &
                                                  dist_dataset_agreement %in%  higher, 
                                                  .(x = "Both higher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_higher <- merge(environments_select_higher, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_higher[is.na(area_fraction), area_fraction := 0]
environments_select_higher[is.na(x), x := "Both higher"]
environments_select_higher[, x_pos := 5]

environments_select_lower <- environments_select[rel_dataset_agreement %in% lower &
                                                 dist_dataset_agreement %in%  lower, 
                                                 .(x = "Both lower", area_fraction = sum(area_fraction)), .(environment)]
environments_select_lower <- merge(environments_select_lower, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_lower[is.na(area_fraction), area_fraction := 0]
environments_select_lower[is.na(x), x := "Both lower"]
environments_select_lower[, x_pos := 6]

environments_select_q_higher <- environments_select[rel_dataset_agreement %in% higher &
                                                      dist_dataset_agreement %in% lower, 
                                                    .(x = "Contrasting\nMagnitude higher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_q_higher <- merge(environments_select_q_higher, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_q_higher[is.na(area_fraction), area_fraction := 0]
environments_select_q_higher[is.na(x), x := "Contrasting\nMagnitude higher"]
environments_select_q_higher[, x_pos := 7]

environments_select_d_higher <- environments_select[rel_dataset_agreement %in% lower &
                                                           dist_dataset_agreement %in% higher, 
                                                    .(x = "Contrasting\nDistribution higher", area_fraction = sum(area_fraction)), .(environment)]
environments_select_d_higher <- merge(environments_select_d_higher, 
                                    environments_select[,.(environment = unique(environment))], 
                                    all = T, by = "environment") 
environments_select_d_higher[is.na(area_fraction), area_fraction := 0]
environments_select_d_higher[is.na(x), x := "Contrasting\nDistribution higher"]
environments_select_d_higher[, x_pos := 8]



environments_joined <- rbind(environments_select_q_high,
                             environments_select_q_low,
                             environments_select_d_high,
                             environments_select_d_low,
                             environments_select_higher,
                             environments_select_lower,
                             environments_select_q_higher,
                             environments_select_d_higher
                             )

environments_level <- environments_joined[, unique(as.factor(x))]

environments_blue <- rbind(environments_select_q_high,
                             environments_select_d_high,
                             environments_select_higher
)

environments_red <- rbind(environments_select_q_low,
                           environments_select_d_low,
                           environments_select_lower
)

environments_purple <- rbind(environments_select_q_higher,
                             environments_select_d_higher
)

# plot ----
## label ----
top_row <- " "

block_labels <- tibble(
  x_pos = c(1.5, 3.5, 6.5),
  environment = top_row,
  label = c(
    "Magnitude agreement",
    "Distribution agreement",
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
    high = "#7A4F64",
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
    breaks = 1:8,
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
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
           fill = NA, colour = "black", linewidth = 0.8) +
  annotate("rect", xmin = 2.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
           fill = NA, colour = "black", linewidth = 0.8) +
  annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -Inf, ymax = Inf,
           fill = NA, colour = "black", linewidth = 0.8) +
  theme_fig4+
  theme(legend.position = "bottom")+
  labs(y = "Environment", x = NULL,
       title = "Environmental fingerprints of product agreement")

# Save figure ----
ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig4_fingerprint.png"
  ),
  plot = fig4,
  width = figure_widths,
  height = 15,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig4_fingerprint.pdf"
  ),
  plot = fig4,
  width = figure_widths,
  height = 15,
  units = "cm",
  dpi = 300,
  device = cairo_pdf,
)


