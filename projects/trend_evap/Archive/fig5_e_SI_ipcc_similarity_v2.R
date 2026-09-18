# Figure 5 — Similarity among IPCC reference regions ----------------------
source("source/evap_trend.R")
source("source/evap_trend_graphics.R")

library(data.table)
library(ggplot2)
library(ggpubr)


# Hierarchical colour key --------------------------------------------------

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
    # Group 1 — yellow
    "#F5E6B3",  # 0.2
             "#C79A32",  # 0.4
             
             # Group 2 — blue
             "#72A1C3",  # 0.4
             "#355C7D",  # 0.6, dark slate blue
             "#155B70",  # 0.6, dark blue
             "#808FC0",  # 0.4
             "#4C4F8A",  # 0.6, dark indigo blue
             
             # Group 3 — orange
             "#F4D7B2",  # 0.2
             "#D8893A",  # 0.4
             
             # Group 4 — purple
             "#E5D5EA",  # 0.2
             "#996EAA",  # 0.4
             
             # Group 5 — green
             "#DCE9D5",  # 0.2
             "#7AA65F",  # 0.4
             "#397A52",  # 0.6
             
             # Group 6 — olive
             "#E8E3D0",  # 0.2
             "#A49A52",  # 0.4
             "#6E692F",  # 0.6
             
             # Group 7 — red and rose
             "#C97468",  # 0.4, salmon
             "#7B332E",  # 0.6, dark rust
             "#C2708B",  # 0.4, rose
             "#B96769",  # 0.4, muted red
             "#6E2E4A"   # 0.6, dark wine
  ),
  level = c(
    0.2, 0.4,
    0.4, 0.6, 0.6, 0.4, 0.6,
    0.2, 0.4,
    0.2, 0.4,
    0.2, 0.4, 0.6,
    0.2, 0.4, 0.6,
    0.4, 0.6, 0.4, 0.4, 0.6
  ),
  
  subgroup = c(
    "1a", "1b",
    "2a", "2a1", "2a2", "2b", "2b1",
    "3a", "3b",
    "4a", "4b",
    "5a", "5b", "5c",
    "6a", "6b", "6c",
    "7a", "7a1", "7b", "7c", "7c1"
  )
)

# Horizontal positions in the colour key ----------------------------------

color_key[, x_plot := as.numeric(broad_group)]

color_key[subgroup == "2a",  x_plot := 1.90]
color_key[subgroup == "2a1", x_plot := 1.80]
color_key[subgroup == "2a2", x_plot := 2.00]
color_key[subgroup == "2b",  x_plot := 2.30]
color_key[subgroup == "2b1", x_plot := 2.30]

color_key[subgroup == "7a",  x_plot := 6.80]
color_key[subgroup == "7a1", x_plot := 6.80]
color_key[subgroup == "7b",  x_plot := 7.10]
color_key[subgroup == "7c",  x_plot := 7.30]
color_key[subgroup == "7c1", x_plot := 7.30]

# IPCC regions belonging to each subgroup ---------------------------------

region_subgroups <- data.table(
  subgroup = c(
    "1a", "1b",
    "2a", "2a1", "2a2", "2b", "2b1",
    "3a", "3b",
    "4a", "4b",
    "5a", "5b", "5c",
    "6a", "6b", "6c",
    "7a", "7a1", "7b", "7c", "7c1"
  ),
  
  regions = c(
    "ARP",
    "SAH,WAF",
    
    "CAR,EAS",
    "CAF,ECA,SCA,SEA",
    "ESB,TIB",
    "SAM,SAS,SEAF",
    "SES,WNA",
    
    "CAU",
    "EAU,SAU",
    
    "CNA,EEU,NAU",
    "MED,WCE",
    
    "NES",
    "ENA,MDG,NZ",
    "GIC,SWS",
    
    "NCA",
    "NEAF",
    "ESAF,WSAF",
    
    "NWN,WSB",
    "NEN,NSA,WCA",
    "NEU,SSA",
    "NWS",
    "RAR,RFE"
  )
)

region_subgroups <- region_subgroups[, .(Acronym = trimws(unlist(strsplit(regions, ",")))), by = subgroup]

# Add colours and hierarchy information
region_colors <- merge(
  region_subgroups,
  color_key,
  by = "subgroup",
  all.x = TRUE,
  sort = FALSE
)


# Read IPCC hexagons -------------------------------------------------------

# Keep read.csv: fread may interpret the geometry columns differently
ipcc_hexagon <- read.csv(
  paste0(PATH_IPCC_data, "/gloabl_ipcc_ref_hexagons.csv")
)

setDT(ipcc_hexagon)
ipcc_hexagon <- ipcc_hexagon[Acronym != "PAC"]
# Preserve the original polygon-vertex ordering through the merge
ipcc_hexagon[, row_id := .I]

data <- merge(
  ipcc_hexagon,
  region_colors,
  by = "Acronym",
  all.x = TRUE,
  sort = FALSE,
  allow.cartesian = TRUE
)

setorder(data, row_id)

# Check that all IPCC regions received a colour
data[is.na(color), unique(Acronym)]


# Reposition selected hexagons --------------------------------------------

# Australia
australia_rows <- which(
  data$Acronym %in% c("NAU", "CAU", "EAU", "SAU")
)

shift_lon_australia <- 5
shift_lat_australia <- 12

data$long[australia_rows] <-
  data$long[australia_rows] + shift_lon_australia

data$lat[australia_rows] <-
  data$lat[australia_rows] + shift_lat_australia

data$V1[australia_rows] <-
  data$V1[australia_rows] + shift_lon_australia

data$V2[australia_rows] <-
  data$V2[australia_rows] + shift_lat_australia


# New Zealand
nz_rows <- which(data$Acronym == "NZ")

shift_lon_nz <- 10
shift_lat_nz <- 9

data$long[nz_rows] <- data$long[nz_rows] + shift_lon_nz
data$lat[nz_rows]  <- data$lat[nz_rows]  + shift_lat_nz
data$V1[nz_rows]   <- data$V1[nz_rows]   + shift_lon_nz
data$V2[nz_rows]   <- data$V2[nz_rows]   + shift_lat_nz


# Madagascar
mdg_rows <- which(data$Acronym == "MDG")

shift_lon_mdg <- -7
shift_lat_mdg <- -3

data$long[mdg_rows] <- data$long[mdg_rows] + shift_lon_mdg
data$lat[mdg_rows]  <- data$lat[mdg_rows]  + shift_lat_mdg
data$V1[mdg_rows]   <- data$V1[mdg_rows]   + shift_lon_mdg
data$V2[mdg_rows]   <- data$V2[mdg_rows]   + shift_lat_mdg


# Greenland and Iceland
gic_rows <- which(data$Acronym == "GIC")

shift_lon_gic <- -7
shift_lat_gic <- 4

data$long[gic_rows] <- data$long[gic_rows] + shift_lon_gic
data$lat[gic_rows]  <- data$lat[gic_rows]  + shift_lat_gic
data$V1[gic_rows]   <- data$V1[gic_rows]   + shift_lon_gic
data$V2[gic_rows]   <- data$V2[gic_rows]   + shift_lat_gic


# Select contrasting text colours -----------------------------------------

rgb_values <- col2rgb(data$color)

# Relative perceived brightness
brightness <- (
  0.299 * rgb_values[1, ] +
    0.587 * rgb_values[2, ] +
    0.114 * rgb_values[3, ]
)

data[, label_color := ifelse(
  brightness < 145,
  "white",
  "black"
)]

# Draw each region label only once
label_data <- unique(
  data[, .(Acronym, V1, V2, label_color)]
)


# Panel a: IPCC hexagon map ------------------------------------------------

fig_map <- ggplot(data) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = color
    ),
    colour = "white",
    linewidth = 0.6
  ) +
  geom_text(
    data = label_data,
    aes(
      x = V1,
      y = V2,
      label = Acronym,
      colour = label_color
    ),
    size = 3.5
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_equal() +
  labs(title = "Similarity of topology across IPCC reference regions")+
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 5,
      l = 10
    )
  )


# Panel b: hierarchical colour key ----------------------------------------

fig_key <- ggplot(
  color_key,
  aes(
    x = x_plot,
    y = level,
    colour = color
  )
) +
  geom_point(size = 4) +
  scale_colour_identity() +
  scale_x_continuous(
    breaks = 1:7,
    labels = paste("Group", 1:7),
    limits = c(0.6, 7.5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = c(0.2, 0.4, 0.6),
    labels = c("≥ 0.2", "≥ 0.4", "≥ 0.6"),
    limits = c(0.18, 0.62),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Correlation threshold"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "none",
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 10,
      l = 10
    )
  )


# Combine map and hierarchical key ----------------------------------------

fig_ipcc_similarity <- ggarrange(
  fig_map,
  fig_key,
  ncol = 1,
  heights = c(3, 1.2),
  labels = c("a", "b"),
  font.label = list(
    size = 16,
    face = "bold"
  ),
  align = "v"
)

fig_ipcc_similarity


# Save --------------------------------------------------------------------

ggsave(
  filename = paste0(
    PATH_SAVE_EVAP_TREND_FIGURES_MAIN,
    "fig6_ipcc_topology_similarity.png"
  ),
  plot = fig_ipcc_similarity,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)
