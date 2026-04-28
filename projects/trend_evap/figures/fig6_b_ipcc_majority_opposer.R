# Figure 6 - Topology -----
## Where does Gleam oppose the majority the most ----

source('source/evap_trend.R')

# data ----
topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ref_regions_dataset_trend_topology.rds"))

topology_sel <- topology[p_value == "p <= 0.05"]
topology_sel_melt <- melt(topology_sel, id.vars = c("dataset", "p_value", "IPCC_ref_region"))
topology_sel_melt[variable == "rank_pos_signal", plot_name := "Positive \nbooster"]
topology_sel_melt[variable == "rank_neg_signal", plot_name := "Negative \nbooster"]
topology_sel_melt[variable == "rank_dampener", plot_name := "Signal \ndampener"]
topology_sel_melt[variable == "rank_trend_opposer", plot_name := "Majority trend\nopposer"]
topology_sel_melt[variable == "rank_opposition_contributor", plot_name := "Opposition\ncontributor"]
topology_sel_melt[variable == "rank_significance_opposer", plot_name := "Majority Significance \nopposer"]
setnames(topology_sel_melt, "IPCC_ref_region", "Acronym")

topology_sel_dataset <- topology_sel_melt[dataset == "GLEAM" & plot_name == "Majority trend\nopposer"]
topology_sel_dataset[,p_value := NULL]
topology_sel_dataset[,variable := NULL]


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

# plot ----
summary(data)
data_acron <- data[value > 8]

data[, Rank := cut(value, breaks = c(0,2,4,6,8,10,12,14),
                                labels = c("[1,2]", "[3,4]", "[5,6]",
                                           "[7,8]", "[9,10]", "[11,12]",
                                           "[13,14]"))]

fill_topology_rank <- c("[1,2]" = "#440154FF", "[3,4]" = "#414487FF",
                        "[5,6]" = "#2A7B8EFF",  "[7,8]" = "#22A384FF",
                        "[9,10]" = "#7AD151FF", "[11,12]" = "#C7E020FF",
                        "[13,14]" = "#FDE725FF")

ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = Rank, group = group), colour = "black") +
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
