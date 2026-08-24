# Figure 3 ---- 
## Ensemble trend and then product trends with significance in tile format ----
## Uncertainty groups, area fraction and aggregated
source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

## colors ----
cols_problem <- c("Both" = "#330000", "Direction" = "darkred","Magnitude" = "orange2",
                  "None" = "#7CA8F3")



## IPCC reference regions ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_IPCC_ref_regions_trends_by_product.rds"))
ipcc_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_IPCC_ref_regions_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_IPCC_ref_regions_problem_aggregated.rds"))
ipcc_trends[problem %in% c("Direction", "Both"), problem_fraction := sum(ipcc_fraction), .(IPCC_ref_region)]
ipcc_trends[, problem_fraction := min(problem_fraction, na.rm = T), .(IPCC_ref_region)]
ipcc_problems_select <- unique(ipcc_trends[, .(IPCC_ref_region, problem_fraction)])
ipcc_problems_select <- ipcc_problems_select[order(-problem_fraction)]

ipcc_trends[, IPCC_ref_region := factor(IPCC_ref_region, 
                                        levels = ipcc_problems_select$IPCC_ref_region)]

ipcc_trends[, problem := factor(problem, 
                                        levels = c("Both", "Direction", "None", "Magnitude"))]

data_trend[, IPCC_ref_region := factor(IPCC_ref_region, 
                                        levels = ipcc_problems_select$IPCC_ref_region)]


data_trend[IPCC_ref_region %in% c("SAH", "ECA", "NCA", "NWN"), highlight := TRUE]



### trends ----
ipcc_slopes <- ggplot(data_trend)+
  geom_tile(aes(y = dataset, 
                x = IPCC_ref_region, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_tile(
    data = data_trend[highlight == TRUE],
    aes(
      y = dataset,
      x = IPCC_ref_region
    ),
    fill = NA,
    color = "black",
    linewidth = 1.4
  ) +
  geom_text(aes(label = round(slope, 1), y = dataset, 
                x = IPCC_ref_region, 
                col = trend_direction_detailed), size = 3.5)+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "#82AEF2",
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"))+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide="none")+  
  theme_bw()+
  labs(fill = "", x = "", y = "",
       title = "ET trends and significance across IPCC reference regions")+
  theme_fig3+
  theme(legend.position = "top")+
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  guides(fill = guide_legend(ncol = 4, byrow = TRUE))


## quantiles ----
quantiles <- data_trend[, .(Q25 = quantile(slope, 0.25), Q75 = quantile(slope, 0.75),
                            region = first(region)), 
                        IPCC_ref_region]

quantile_plot <- ggplot(quantiles, aes(x = IPCC_ref_region)) +
  geom_hline(yintercept = 0, colour = "grey50",
             linewidth = 0.6, linetype = 1
             ) +
  geom_segment(
    aes(
      y = Q25,
      yend = Q75,
      xend = IPCC_ref_region
    ),
    linewidth = 1.1, lineend = "round", col = "gray20"
  ) +
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  labs( x = "", y = expression(paste("ET trend [mm year"^{-2},"]")), 
        title = "25th–75th percentiles of ET trends across datasets"
)+
  theme_minimal()+
  theme_fig3


### uncertainty for entire region ----
# IPCC prep ----
ipcc_hexagon <- read.csv(paste0(PATH_IPCC_data,"/gloabl_ipcc_ref_hexagons.csv")) #don't use fread
setDT(ipcc_hexagon)
setnames(data_trend_env, "IPCC_ref_region", "Acronym")

data <- ipcc_hexagon[data_trend_env, on = 'Acronym']

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

data[problem %in% c("Magnitude", "None"), label_col := "black"]
data[problem %in% c("Direction", "Both"), label_col := "white"]

label_col = c("Magnitude" = "black",
              "None" = "black",
              "Direction" = "white",
              "Both" = "white")

ipcc_hexagon <- ggplot(data) +
  geom_polygon(aes(x = long, y = lat, fill = as.factor(problem), group = group), colour = "black") +
  geom_text(aes(V1, V2, label = Acronym, color = problem), size = 5) +
  coord_equal() + 
  scale_fill_manual(values = cols_problem)+  
  scale_color_manual(values = label_col, guide = "none")+
  labs(x = NULL, y = NULL, fill = "", 
       title = "Quartile uncertainty aggregated across IPCC reference regions",) + 
  theme_void() + 
  theme_fig3+
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) 


### problems  ----
ipcc_problems <- ggplot(ipcc_trends)+
  geom_bar(aes(y = ipcc_fraction*100, 
               x = IPCC_ref_region, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = '', y = "Area fraction [%]", x = "",
       title = "Quartile uncertainty across IPCC reference regions")+
  theme_fig3+
  geom_hline(yintercept = seq(25, 75, 25), color = "gray80") + 
  facet_grid(cols = vars(region), scales = "free", space = "free")+
  guides(fill = "none")

### plot all ----

fig <- plot_grid(
  ipcc_slopes, quantile_plot, ipcc_problems,
  ncol = 1,
  align = "v",
  axis = "l",
  rel_heights = c(3, 1.5, 1.5), 
  labels = c("a", "b", "c"), 
  label_size = 20
)

hex_left <- plot_grid(
  NULL,
  ipcc_hexagon + theme(plot.title = element_blank()),
  NULL,
  nrow = 1,
  rel_widths = c(0.25, 0.7, 0.15)
)

hex_left <- ggdraw() +
  draw_plot(
    hex_left,
    x = 0,
    y = 0,
    width = 1,
    height = 0.90
  ) +
  draw_label(
    "Quartile uncertainty aggregated across IPCC reference regions",
    x = 0.12,
    y = 0.98,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 20
  )

ggarrange(fig, hex_left, 
          heights = c(2.5, 0.8), 
          labels = c("", "d"), 
          font.label = list(size = 20), nrow = 2)

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig3_quartile_uncertainty_ipcc.png"), 
        width = 18, height = 18,
       bg = "white")

