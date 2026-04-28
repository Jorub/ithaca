# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# Maps of global ensemble slope
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_a
evap_trend_grid_ensemble <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_grid_bootstrap.rds"))  

## Global map of ensemble trend and significant trend ----
evap_trend_grid_ensemble[, slope_sig_brk:= cut(slope*significant_theil_sen, breaks = c(min(slope), -5, -3, -0.1, 0.1, 3, 5, max(slope)))]
evap_trend_grid_ensemble[, slope_brk:= cut(slope, breaks = c(min(slope), -5, -3, -0.1, 0.1, 3, 5, max(slope)))]
evap_trend_grid_ensemble[, slope_percent_brk:= cut(slope_percent, breaks = c(-5, -2, -1, -0.1, 0.1, 1, 2, 10))]

cols_grid_ensemble <- c("darkblue", 
                                  "steelblue3",
                                  "steelblue1", 
                                  "gray80",
                                  "gold",
                                  "darkorange",
                                  "darkred"
)

## Global map of significant trend ----
ggplot(evap_trend_grid_ensemble)+
  geom_tile(aes(x = lon, y = lat, fill = slope_sig_brk))+
  scale_fill_manual(values = cols_grid_ensemble)+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_evap_ensemble_trend_theil_sen_significant_bootstrap.png"), 
       width = 12, height = 8)
