source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

library(data.table)

color_key <- data.table(
  broad_group = c(
    1, 1,
    2, 2, 2, 2, 2,
    3, 3,
    4, 4,
    5, 5, 5,
    6, 6, 6,
    7, 7, 7, 7, 7
  ),
  color = c(
    "#F7E7A9", "#D8AE38",
    "#86B6D8", "#2166AC", "#238B9F", "#8996C7", "#4E5AA7",
    "#FAD7A0", "#E6953B",
    "#F1C2BA", "#C9786B",
    "#D7E9CC", "#8EBD69", "#347C52",
    "#E6DFC0", "#AA9C59", "#68652D",
    "#9B88C7", "#5B3F99", "#C184B3", "#B36F91", "#763457"
  ),
  level = c(
    0.2, 0.4,
    0.4, 0.6, 0.6, 0.4, 0.6,
    0.2, 0.4,
    0.2, 0.4,
    0.2, 0.4, 0.6,
    0.2, 0.4, 0.6,
    0.4, 0.6, 0.4, 0.4, 0.6
  )
)

color_key[, subgroup := c(
  "1a", "1b",
  "2a", "2a1", "2a2", "2b", "2b1",
  "3a", "3b",
  "4a", "4b",
  "5a", "5b", "5c",
  "6a", "6b", "6c",
  "7a", "7a1", "7b", "7c", "7c1"
)]

color_key[, x_plot := as.numeric(broad_group)]

color_key[, x_plot := fcase(
  subgroup == "2a",  1.90,
  subgroup == "2a1", 1.8,
  subgroup == "2a2", 2,
  subgroup == "2b",  2.30,
  subgroup == "2b1", 2.30,
  
  subgroup == "7a",  6.8,
  subgroup == "7a1", 6.8,
  subgroup == "7b",  7.10,
  subgroup == "7c",  7.3,
  subgroup == "7c1", 7.3,
  
  default = x_plot
)]

colors <- color_key$color
names(colors) <- color_key$color

ggplot() +
  geom_point(
    data = color_key,
    aes(x = x_plot, y = level, col = color),
    size = 5
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = 1:7,
    labels = paste("Group", 1:7)
  ) +
  scale_y_continuous(
    breaks = c(0.2, 0.4, 0.6),
    labels = c("≥ 0.2", "≥ 0.4", "≥ 0.6")
  ) +
  labs(
    x = "",
    y = "Mean Spearman correlation threshold"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# IPCC prep ----
ipcc_hexagon <- read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv")) #don't use fread
setDT(ipcc_hexagon)
data <- ipcc_hexagon[topology_sel_dataset, on = 'Acronym']
# Identify the rows corresponding to Madagascar, NAU, CAU, EAU, and SAU hexagons
med_rows <- which(data$Acronym %in% c("NAU", "CAU", "EAU", "SAU"))
med_rows_nz <- which(data$Acronym == "NZ")
med_rows_mdg <- which(data$Acronym == "MDG")
med_rows_gic <- which(data$Acronym == "GIC")

# Define the amount by which you want to shift leftward

shift_lon_gic <- 7  # You can adjust this value based on your preference
shift_lat_gic <- -4

data$long[med_rows_gic] <- data$long[med_rows_gic] - shift_lon_gic
data$lat[med_rows_gic] <- data$lat[med_rows_gic] - shift_lat_gic
data$V1[med_rows_gic] <- data$V1[med_rows_gic] - shift_lon_gic
data$V2[med_rows_gic] <- data$V2[med_rows_gic] - shift_lat_gic

shift_lon_mdg <- 7  # You can adjust this value based on your preference
shift_lat_mdg <- 3

# Shift the longitude (long) values for Madagascar hexagon
data$long[med_rows_mdg] <- data$long[med_rows_mdg] - shift_lon_mdg
data$lat[med_rows_mdg] <- data$lat[med_rows_mdg] - shift_lat_mdg
data$V1[med_rows_mdg] <- data$V1[med_rows_mdg] - shift_lon_mdg
data$V2[med_rows_mdg] <- data$V2[med_rows_mdg] - shift_lat_mdg
# Define the amount by which you want to shift leftward
shift_lon_amount <- 5  
shift_lat_amount <- 12

shift_lon_amount_nz <- 10 
shift_lat_amount_nz <- 9

# Shift the longitude (long) and latitude (lat) values for the specified hexagons
data$long[med_rows] <- data$long[med_rows] + shift_lon_amount
data$lat[med_rows] <- data$lat[med_rows] + shift_lat_amount
data$V1[med_rows] <- data$V1[med_rows] + shift_lon_amount
data$V2[med_rows] <- data$V2[med_rows] + shift_lat_amount

data$long[med_rows_nz] <- data$long[med_rows_nz] + shift_lon_amount_nz
data$lat[med_rows_nz] <- data$lat[med_rows_nz] + shift_lat_amount_nz
data$V1[med_rows_nz] <- data$V1[med_rows_nz] + shift_lon_amount_nz
data$V2[med_rows_nz] <- data$V2[med_rows_nz] + shift_lat_amount_nz


ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = cluster, group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym), size = 4, color = "White") +
  geom_text(data = data_acron, aes(V1, V2, label = Acronym), size = 4, color = "black") +
  coord_equal() + 
  scale_fill_manual(values = fill_topology_rank,
                    name = "Rank\nStrongest \u2192 Weakest") +
  labs(x = NULL, y = NULL, fill = "Rank", title = "Spatial topology use-case",
       subtitle = "Where does GLEAM oppose the majority trend direction the strongest?") + 
  theme_void() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12)
  ) +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL)



ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig6_topology_use-case_ipcc.png"), 
       width = 8, height = 8)