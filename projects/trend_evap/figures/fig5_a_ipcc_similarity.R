# Figure 5 — Similarity among IPCC reference regions ----------------------
source("source/evap_trend.R")
source("source/evap_trend_graphics.R")

library(data.table)
library(ggplot2)
library(ggpubr)


# Hierarchical colour key --------------------------------------------------

color_key <- data.table(
  broad_group = c(
    1,
    2, 2, 2, 2,
    3,
    4,
    5, 5,
    6, 6,
    7, 7, 7
  ),
  color = c(
    # Group 1 — pale lemon
    "#F1E58F",
             
             # Group 2 — cool sky blue
             "#C5DFEA",
             "#426B82",
             "#2856A0",
             "#244F70",
             
             # Group 3 — light apricot
             "#F5C78F",
             
             # Group 4 — cool periwinkle
             "#D8D0E6",
             
             # Group 5 — cool light green
             "#CBE4CD",
             "#327A55",
             
             # Group 6 — cool grey-green
             "#D4CCAD",
             "#6F6236",
             
             # Group 7 — light terracotta
             "#EFC9B8",
             "#9D4638",
             "#7D3B43"
  ),  
  level = c(
    0.2,
    0.2, 0.6, 0.6, 0.6,
    0.2,
    0.2,
    0.2, 0.6,
    0.2, 0.6,
    0.2, 0.6, 0.6
  ),
  
  subgroup = c(
    "1",
    "2a", "2a1", "2a2", "2b1",
    "3",
    "4",
    "5a", "5c",
    "6a", "6c",
    "7a", "7a1", "7c1"
  )
)
# Horizontal positions in the colour key ----------------------------------

color_key[, x_plot := as.numeric(broad_group)]

color_key[subgroup == "2a1", x_plot := 1.70]
color_key[subgroup == "2a2", x_plot := 2.00]
color_key[subgroup == "2b1", x_plot := 2.30]

color_key[subgroup == "7a1", x_plot := 6.80]
color_key[subgroup == "7c1", x_plot := 7.20]

# IPCC regions belonging to each subgroup ---------------------------------

region_subgroups <- data.table(
  subgroup = c(
    "1",
    "2a", "2a1", "2a2", "2b1",
    "3",
    "4", 
    "5a", "5c",
    "6a", "6c",
    "7a", "7a1", "7c1"
  ),
  
  regions = c(
    "ARP,SAH,WAF",
    
    "CAR,EAS,SAM,SAS,SEAF",
    "CAF,ECA,SCA,SEA",
    "ESB,TIB",
    "SES,WNA",
    
    "CAU,EAU,SAU",
    
    "CNA,EEU,NAU,MED,WCE",
    
    "NES,ENA,MDG,NZ",
    "GIC,SWS",
    
    "NCA,NEAF",
    "ESAF,WSAF",
    
    "NWN,WSB,NEU,SSA,NWS",
    "NEN,NSA,WCA",
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
  labs(title = "Topology-based clustering of IPCC reference regions")+
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

color_key[, level := factor(level, levels = c(0.6, 0.2))]

fig_key <- ggplot(
  color_key,
  aes(
    x = x_plot,
    y = as.factor(level),
    colour = color
  )
) +
  geom_point(size = 6) +
  scale_colour_identity() +
  scale_x_continuous(
    breaks = 1:7,
    labels = paste("Group", 1:7),
    limits = c(0.6, 7.5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_discrete(
    labels = c("Subcluster","Cluster")
  ) +
  labs(
    x = NULL,
    y = "",
    title = "Cluster key"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(),
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
  align = "v",
  heights = c(3, 1),
  font.label = list(
    size = 16,
    face = "bold"
  )
)

fig_ipcc_similarity

# save ----
ggsave(
  filename = paste0(
    PATH_SAVE_EVAP_TREND_FIGURES_MAIN,
    "fig5_ipcc_topology_similarity.png"
  ),
  plot = fig_ipcc_similarity,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)
