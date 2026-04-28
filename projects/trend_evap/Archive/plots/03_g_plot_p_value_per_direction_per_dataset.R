# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# plot increase of uncertainty with dataset leftout
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend <- evap_trend[dataset_count >= 14]
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Analysis ----
evap_trend_masks <- merge(evap_trend, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_masks <- grid_cell_area[evap_trend_masks, on = .(lon, lat)]
evap_trend_masks[slope > 0, direction := "positive"]
evap_trend_masks[slope < 0, direction := "negative"]
evap_trend_masks[slope == 0, direction := "exactly zero"]

evap_trend_masks[,p_brk := cut(p, c(-1e-5,0.05,0.1,0.2,1))]

## plot ----

ggplot(evap_trend_masks[p < 0.2])+
  geom_violin(aes(x = land_cover_short_class, y = p, col = direction))+
  geom_abline(slope = 0, intercept = 0.05)+
  geom_abline(slope = 0, intercept = 0.1)+
  geom_abline(slope = 0, intercept = 0.01)+
  theme_bw()+
  facet_wrap(~dataset)+
  scale_color_manual(values = c("positive" = "darkred", "negative" = "royalblue"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))


ggplot(evap_trend_masks[p < 0.2 & !land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "hist_land_use_p_val_smaller_02_distribution_direction_dataset.png"), 
       width = 12, height = 12)

ggplot(evap_trend_masks, aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "hist_land_use_p_val_distribution_direction_dataset.png"), 
       width = 16, height = 12)


ggplot(evap_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "hist_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)


ggplot(evap_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice")], aes(x = p_brk, fill= direction))+
  geom_bar(stat = "count", position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "bar_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)

ggplot(evap_trend_masks[!land_cover_short_class %in% c("Other", "Snow/Ice") & p < 0.2], aes(x = p_brk, fill= direction))+
  geom_bar(stat = "count", position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(land_cover_short_class~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "bar_land_use_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)

ggplot(evap_trend_masks[!is.na(KG_class_1)], aes(x = p, fill= direction))+
  geom_histogram(binwidth = 0.05, position = "dodge")+
  labs(y = "grid count")+
  theme_bw()+
  facet_grid(KG_class_1~dataset, scale = "free_y")+
  scale_fill_manual(values = c("positive" = "darkred", "negative" = "royalblue", "exactly zero" = "black"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "hist_KG_1_p_val_distribution_direction_dataset_selected.png"), 
       width = 16, height = 12)
