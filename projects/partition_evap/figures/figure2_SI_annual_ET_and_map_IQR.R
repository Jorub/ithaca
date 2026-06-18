
### Data ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]

evap_annual_vol <- evap_annual_vol[!(dataset == "etmonitor" & year == 2000), ]



gg_volume <- ggplot(evap_annual_vol, aes(x = 0, y = evap_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Global annual volume [km'^3,']')))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


### IQR agreement ----

to_plot_sf <- dataset_agreement_grid_wise[, .(lon, lat, IQR_agreement)
][, value := as.numeric(IQR_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_quantile_range <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), labels = levels(dataset_agreement_grid_wise$IQR_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile\nrange") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20))
