# Figure 4 - SI -----
## Where does Product X oppose the majority the most ----

source('source/evap_trend.R')

# data ----
topology <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_ref_regions_dataset_trend_topology.rds"))

topology_sel <- topology[p_value == "p <= 0.05"]
topology_sel_melt <- melt(topology_sel, id.vars = c("dataset", "p_value", "IPCC_ref_region"))
topology_sel_melt[variable == "rank_pos_signal", plot_name := "Positive \nbooster"]
topology_sel_melt[variable == "rank_neg_signal", plot_name := "Negative \nbooster"]
topology_sel_melt[variable == "rank_no_trenders", plot_name := "Signal \ndampener"]
topology_sel_melt[variable == "rank_DCI_opposer", plot_name := "Majority trend\nopposer"]
topology_sel_melt[variable == "rank_opposer", plot_name := "Opposition\ncontributor"]
topology_sel_melt[variable == "rank_significance_opposer", plot_name := "Majority significance\nopposer"]
setnames(topology_sel_melt, "IPCC_ref_region", "Acronym")

topology_sel_dataset <- topology_sel_melt[dataset == "GLEAM" & plot_name == "Majority trend\nopposer"]
topology_sel_dataset[,p_value := NULL]
topology_sel_dataset[,variable := NULL]


# IPCC prep ----
ipcc_hexagon <- read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv")) #don't use fread
setDT(ipcc_hexagon)

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

data <- ipcc_hexagon[topology_sel_dataset, on = 'Acronym']