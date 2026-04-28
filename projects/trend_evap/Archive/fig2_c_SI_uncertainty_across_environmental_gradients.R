source('source/evap_trend.R')

library(ggpubr)

## colors ----
cols_problem <- c("Both" = "#330000", "Direction" = "darkred",
                  "Magnitude" = "orange2","None" = "forestgreen")

## Latitude ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_lat_groups_trends_by_product.rds"))
elev_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_lat_groups_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_lat_groups_problem_aggregated.rds"))


### trends ----
lat_slopes <- ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = lat_brk, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = lat_brk, 
                col = trend_direction_detailed), size = 4)+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
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
    guide ="none")+  
  theme_light()+
  labs(fill = 'Trend significance   ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1),
        legend.position = "bottom")+  
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

### uncertainty for entire region ----
lat_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = lat_brk, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem, guide = "none")+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Elevation")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),       
        legend.text = element_text(size = 16, margin = margin(r = 10, unit = "pt")), 
        legend.title = element_text(size = 16),      
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(size = 14))+    
  guides(fill = "none")

### problems  ----
lat_problems <- ggplot(lat_trends)+
  geom_bar(aes(x = lat_fraction*100, 
               y = lat_brk, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction [%]", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

ggarrange(lat_slopes, lat_problems, lat_problems_agg, align = "h", legend = "bottom", 
          widths = c(1,0.9, 0.15), labels = c("a", "b", "c"), ncol = 3, font.label = list(size = 20))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_lat_groups.png"), 
       width = 15, height = 8)



## Elevation ----
data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_elevation_trends_by_product.rds"))
elev_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_elevation_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_elevation_problem_aggregated.rds"))


### trends ----
elevation_slopes <- ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = elev_class, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = elev_class, 
                col = trend_direction_detailed), size = 4)+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
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
    guide ="none")+  
  theme_light()+
  labs(fill = 'Trend significance   ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1),
        legend.position = "bottom")+  
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

### uncertainty for entire region ----
elevation_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem, guide = "none")+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Elevation")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),       
        legend.text = element_text(size = 16, margin = margin(r = 10, unit = "pt")), 
        legend.title = element_text(size = 16),      
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(size = 14))+    
  guides(fill = "none")

### problems  ----
elevation_problems <- ggplot(elev_trends)+
  geom_bar(aes(x = elev_fraction*100, 
               y = elev_class, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction [%]", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 16, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

ggarrange(elevation_slopes, elevation_problems,  elevation_problems_agg, align = "h", legend = "bottom", 
          widths = c(1,0.9, 0.1), labels = c("a", "b", "c"), ncol = 3, font.label = list(size = 20))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_SI_slope_problem_elevation.png"), 
       width = 15, height = 8)

## Evaporation quantiles ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_evap_quantiles_trends_by_product.rds"))
evap_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_evap_quantiles_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_evap_quantiles_problem_aggregated.rds"))

### trends ----
evap_slopes <- ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = evap_quant, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = evap_quant, 
                col = trend_direction_detailed))+
  scale_fill_manual(values = c(
    "neg. p <= 1   " =   "lightblue",
    "neg. p <= 0.1   " =   "royalblue1", 
    "neg. p <= 0.05   " =   "royalblue3", 
    "neg. p <= 0.01   " =   "darkblue", 
    "pos. p <= 0.01   " =   "#330000",
    "pos. p <= 0.05   " =   "darkred",
    "pos. p <= 0.1   " =   "lightcoral",
    "pos. p <= 1   " =   "orange"),
    guide = "none")+  
  scale_color_manual(values = c(
    "neg. p <= 1   " =   "black",
    "neg. p <= 0.1   " =   "black", 
    "neg. p <= 0.05   " =   "white", 
    "neg. p <= 0.01   " =   "white", 
    "pos. p <= 0.01   " =   "white",
    "pos. p <= 0.05   " =   "white",
    "pos. p <= 0.1   " =   "black",
    "pos. p <= 1   " =   "black"),
    guide = "none")+  
  theme_bw()+
  labs(fill = 'Trend \nsignificance   ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),        
        plot.margin = unit(c(0.5,0,0,0.8), "cm"),        
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16),      
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_text(size = 16),
        legend.position = "none")

### uncertainty for entire region ----

evap_quant_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = evap_quant, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 16, margin = margin(r = 10, unit = "pt")), 
        legend.title = element_text(size = 16),      
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(size = 16))+  
  guides(fill = "none")

### problems  ----
evap_quant_problems <- ggplot(evap_trends)+
  geom_bar(aes(x = evap_fraction*100, 
               y = evap_quant, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem, guide = "none")+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "Evaporation quantiles")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.8), "cm"),                 
        legend.text = element_text(size = 16, margin = margin(r = 15, unit = "pt")), 
        legend.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position = "none")

# plot all ----
quantiles <- ggarrange(evap_slopes, evap_quant_problems, evap_quant_problems_agg,
                    align = "h", 
                    widths = c(1.0,0.9, 0.1), 
                    labels = c("a", "b", "c"), 
                    font.label = list(size = 20), nrow = 1, ncol = 3,
                    legend = "none")

elevation <- ggarrange( elevation_slopes, elevation_problems, elevation_problems_agg,
                    align = "h", 
                    widths = c(1.0,0.9, 0.1), 
                    labels = c("d", "e", "f"), 
                    font.label = list(size = 20), nrow = 1, ncol = 3,
                    legend = "bottom")

fig_5 <- ggarrange(quantiles, elevation,
          heights = c(1, 1.1), nrow = 2)

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig5_quartile_uncertainty_gradients.png"), 
       width = 15, height = 10)
