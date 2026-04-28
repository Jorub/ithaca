# Ensemble trend and then product trends with significance in tile format ----
source('source/evap_trend.R')
source('source/geo_functions.R')

library(ggpubr)

### colors

## colors ----
cols_problem <- c("Both" = "#330000", "Direction" = "darkred","Magnitude" = "orange2",
                  "None" = "royalblue1")

## KG beck ----

data_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_Koeppen_Geiger_beck_trends_by_product.rds"))
KG_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_Koeppen_Geiger_beck_problem_area_fraction.rds"))
data_trend_env <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_Koeppen_Geiger_beck_problem_aggregated.rds"))

### trends ----
KG_slopes <- ggplot(data_trend)+
  geom_tile(aes(x = dataset, 
                y = KG_beck, 
                fill = trend_direction_detailed), 
            color = "white", lwd = 0.8, linetype = 1)+
  geom_text(aes(label = round(slope, 1), x = dataset, 
                y = KG_beck, 
                col = trend_direction_detailed))+
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
    guide="none")+  
  theme_bw()+
  labs(fill = 'Trend \nsignificance   ', x = "Dataset", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),        
        plot.margin = unit(c(0.5,0,0,0.8), "cm"),        
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),      
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        strip.text.y = element_text(size = 16))+  
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))+
  facet_grid(rows = vars(climate), scales = "free", space = "free")

### uncertainty for entire region ----

KG_problems_agg <- ggplot(data_trend_env)+
  geom_bar(aes(x = 1, 
               y = KG_beck, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "", y = "")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        legend.text = element_text(size = 18, margin = margin(r = 10, unit = "pt")), 
        legend.title = element_text(size = 18),      
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 16))+  
  guides(fill = "none")+
  facet_grid(rows = vars(climate), scales = "free", space = "free")

### problems  ----
KG_problems <- ggplot(KG_trends)+
  geom_bar(aes(x = KG_fraction, 
               y = KG_beck, 
               fill = problem), 
           color = "white", lwd = 0.8, linetype = 1,
           stat = "identity")+
  scale_fill_manual(values = cols_problem)+  
  theme_bw()+
  labs(fill = 'Uncertainty  ', x = "Area fraction", y = "Koeppen-Geiger")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(0.5,0,0,0.8), "cm"),                 
        legend.text = element_text(size = 18, margin = margin(r = 10, unit = "pt")), 
        legend.title = element_text(size = 18),
        strip.text.y = element_text(size = 16))+  
  facet_grid(rows = vars(climate), scales = "free", space = "free")+
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

### plot ----
ggarrange(KG_slopes, KG_problems,  KG_problems_agg, align = "h", legend = "bottom", 
          widths = c(1,0.8, 0.16), labels = c("a", "b", "c"), ncol = 3, font.label = list(size = 20))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig_SI_ET_trend_consistency_KG_beck.png"), 
       width = 15, height = 14)


