# Support figure of trend consistency across gradients ----
source('source/evap_trend.R')

library(ggpubr)

## colors ----
cols_problem <- c("Both" = "#330000", "Direction" = "darkred",
                  "Magnitude" = "orange2","None" = "forestgreen")

## data
elev_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_elevation_problem_area_fraction.rds"))
evap_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_evap_quantiles_problem_area_fraction.rds"))
KG_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_Koeppen_Geiger_beck_v2_1_problem_area_fraction.rds"))
lat_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_lat_groups_problem_area_fraction.rds"))

lat_trends[, lat_fraction_combined := lat_fraction]
lat_trends[problem %in% c("Both", "Direction"), lat_fraction_combined := sum(lat_fraction), lat_brk]
lat_trends[problem %in% c("Both", "Magnitude"), lat_fraction_combined := sum(lat_fraction), lat_brk]
lat_trends[problem %in% c("Both", "None"), lat_fraction_combined := lat_fraction]
lat_trends[, problem:= factor(problem, levels = c( "Direction", "Magnitude", "Both", "None"))]
lat_trends[, lat_brk:= factor(lat_brk, levels = c("(75,90]",   "(60,75]",   "(45,60]",   "(30,45]",   "(15,30]",   "(0,15]",    "(-15,0]",  
                                                  "(-30,-15]", "(-45,-30]", "(-60,-45]"))]

elev_trends[, elev_fraction_combined := elev_fraction]
elev_trends[problem %in% c("Both", "Direction"), elev_fraction_combined := sum(elev_fraction), elev_class]
elev_trends[problem %in% c("Both", "Magnitude"), elev_fraction_combined := sum(elev_fraction), elev_class]
elev_trends[problem %in% c("Both", "None"), elev_fraction_combined := elev_fraction]
elev_trends[, problem:= factor(problem, levels = c( "Direction", "Magnitude", "Both", "None"))]

evap_trends[, evap_fraction_combined := evap_fraction]
evap_trends[problem %in% c("Both", "Direction"), evap_fraction_combined := sum(evap_fraction), evap_quant]
evap_trends[problem %in% c("Both", "Magnitude"), evap_fraction_combined := sum(evap_fraction), evap_quant]
evap_trends[problem %in% c("Both", "None"), evap_fraction_combined := evap_fraction]
evap_trends[, problem:= factor(problem, levels = c( "Direction", "Magnitude", "Both", "None"))]

KG_trends[, KG_fraction_combined := KG_fraction]
KG_trends[problem %in% c("Both", "Direction"), KG_fraction_combined := sum(KG_fraction), KG_beck_v2_1]
KG_trends[problem %in% c("Both", "Magnitude"), KG_fraction_combined := sum(KG_fraction), KG_beck_v2_1]
KG_trends[problem %in% c("Both", "None"), KG_fraction_combined := KG_fraction]
KG_trends[, problem:= factor(problem, levels = c( "Direction", "Magnitude", "Both", "None"))]

# latitude ----
fig_lat <- ggplot(lat_trends)+
  geom_bar(aes(y = lat_fraction_combined*100, x = lat_brk, fill = problem),  
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity",
           position = "dodge")+ 
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = '', y = "Area fraction [%]", x = "Latitude [°]")+
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 12, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = "none")+
  facet_wrap(~problem, nrow = 1)

# elevation ----
fig_elev <- ggplot(elev_trends)+
  geom_bar(aes(y = elev_fraction_combined*100, x = elev_class, fill = problem),  
               color = "white", lwd = 0.8, linetype = 1,
               stat = "identity",
               position = "dodge")+ 
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = '', y = "Area fraction [%]", x = "Elevation [m]")+
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 12, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = "none")+
  facet_wrap(~problem, nrow = 1)

# evaporation quantiles ----
fig_evap <- ggplot(evap_trends)+
  geom_bar(aes(y = evap_fraction_combined*100, x = evap_quant, fill = problem),  
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity",
           position = "dodge")+ 
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = '', y = "Area fraction [%]", x = "ET quantiles")+
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 12, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = "none")+
  facet_wrap(~problem, nrow = 1)

# Climate ----
fig_climate <- ggplot(KG_trends)+
  geom_bar(aes(y = KG_fraction_combined*100, x = climate, fill = problem),  
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity",
           position = "dodge")+ 
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = '', y = "Area fraction [%]", x = "Koeppen Geiger classes")+
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = unit(c(0.5,1.0,0,0.5), "cm"),
        legend.text = element_text(size = 12, 
                                   margin = margin(r = 10, unit = "pt")),
        legend.title = element_text(size = 12),
        legend.position = "none")+
  facet_wrap(~problem, nrow = 1)


fig <- ggarrange(fig_evap, fig_elev, fig_lat, fig_climate, nrow = 4,
          labels = c("a", "b", "c", "d"))
annotate_figure(fig, top = text_grob("Quartile uncertainty across environmental gradients", face = "bold", size = 14))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig3_quartile_uncertainty_area_fraction.png"), 
       width = 10, height = 10)
