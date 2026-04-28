# Figure 2: Supplementary plots The motivation  -----
# a. Number of significant trends at p-value 0.05 
# b. Quartile ratio
# c. Quartile direction (dis)agreement
source('source/evap_trend.R')
source('source/geo_functions.R')

# library -----
library(rnaturalearth)

## Map preparation -----
## World and Land borders ----
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
load(paste0(PATH_SAVE_PARTITION_EVAP, "paths.Rdata"))

earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -160, lat = c(50, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

# a. Number of significant trends at p-value 0.05 ----
evap_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))
evap_data[,N_sig_0_05 := N_pos_0_05 + N_neg_0_05]

evap_data_sel <- subset(evap_data, select = c("lon", "lat", "N_sig_0_05"))
evap_data_sel[, N_sig_brk := cut(N_sig_0_05, c(-1,0,1,2,4,7,10, 14))]
evap_data_sel[N_sig_brk == "(-1,0]", N_sig_brk := "[0,0]"]
evap_data_sel[, N_sig_brk:= factor(N_sig_brk, levels = c("[0,0]", "(0,1]","(1,2]",
                                                         "(2,4]", 
                                                         "(4,7]","(7,10]","(10,14]"))]

cols_sig <- c("[0,0]" = "gray40","(0,1]" = "gold",
              "(1,2]" = "lightcoral",
                   "(2,4]" = "darkorchid1",
                   "(4,7]" = "darkorchid3",
                   "(7,10]" = "darkblue", 
                   "(10,14]" = "black")

to_plot_sf <- evap_data_sel[, .(lon, lat, N_sig_brk)
][, value := as.numeric(N_sig_brk)]

sf_to_val <- unique(to_plot_sf[,(.(N_sig_brk = N_sig_brk,
                                        value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, sf_to_val, by = "value", all = T)

fig_map_sig_trends <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "gray90") +
  geom_sf(aes(color = N_sig_brk, fill = N_sig_brk), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_sig) +
  scale_color_manual(values = cols_sig,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "", title = "Number of significant trends",
       subtitle = "Significance level of p-value 0.05") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.position = "none",
        margin(t = 0, r = 0, b = 2, l = 2, unit = "cm"))

grid_cell_area <- unique(evap_data_sel[, .(lon, lat)]) %>% grid_area() # m2
evap_data_sel <- grid_cell_area[evap_data_sel, on = .(lon, lat)]
total_area <- evap_data_sel[, sum(area)]
area_stats <- evap_data_sel[, .(area_fraction = sum(area)/total_area), .(N_sig_brk)]

area_stats <- area_stats[,area_stats[order(N_sig_brk, decreasing = T)]]


bar_sig_trends <- ggplot(area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = N_sig_brk, x = N_sig_brk), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_sig)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 40, 10), color = "white") + 
  coord_flip()


## merge ----
legend_grob = ggplotGrob(bar_sig_trends)

build <- ggplot_build(fig_map_sig_trends)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_sig_trends_plot <- fig_map_sig_trends +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[1] - 0.1 * x_width,
    xmax = x_range[1] + 0.15 * x_width,
    ymin = y_range[1] - 0.25 * y_height,
    ymax = y_range[1] + 0.15 * y_height)

fig <- final_sig_trends_plot+
  theme(plot.margin = margin(0.1, 0.1, 1, 2.5, "cm")) 
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_number_significante_trends.png"), 
       width = 12, height = 8)

# b. Quartile ratio ----

evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_quartile_stats.rds"))

## Map ----

cols_fold <- c("(1,3.3]" = "gold",
              "(3.3,5]" = "lightcoral",
              "(5,10]" = "darkorchid2",
              "(10,Inf]" = "darkblue")

to_plot_sf <- evap_trend_stats[, .(lon, lat, fold_brk_detailed)
][, value := as.numeric(fold_brk_detailed)]

sf_to_val <- unique(to_plot_sf[,(.(fold_brk_detailed = fold_brk_detailed,
                                   value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, sf_to_val, by = "value", all = T)

fig_map_quartile_fold <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "gray90") +
  geom_sf(aes(color = fold_brk_detailed, fill = fold_brk_detailed), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_fold) +
  scale_color_manual(values = cols_fold,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "", title = "Quartile uncertainty in magnitude", 
       subtitle = "Symmetric ratio of ensemble quartiles") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.position = "none")

grid_cell_area <- unique(evap_trend_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_stats <- grid_cell_area[evap_trend_stats, on = .(lon, lat)]
total_area <- evap_trend_stats[, sum(area)]
area_stats <- evap_trend_stats[, .(area_fraction = sum(area)/total_area), .(fold_brk_detailed)]

area_stats <- area_stats[,area_stats[order(fold_brk_detailed, decreasing = T)]]


bar_quartile_fold <- ggplot(area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = fold_brk_detailed, x = fold_brk_detailed), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_fold)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 40, 10), color = "white") + 
  coord_flip()


## merge ----
legend_grob = ggplotGrob(bar_quartile_fold)

build <- ggplot_build(fig_map_quartile_fold)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_map_quartile_fold <- fig_map_quartile_fold +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[1] - 0.1 * x_width,
    xmax = x_range[1] + 0.15 * x_width,
    ymin = y_range[1] - 0.25 * y_height,
    ymax = y_range[1] + 0.15 * y_height)

fig <- final_map_quartile_fold+
  theme(plot.margin = margin(0.1, 0.1, 1, 2.5, "cm")) 

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_quartile_uncertainty_magnitude.png"), 
       width = 12, height = 8)


# c. Quartile direction (dis)agreement -----

cols_sign <- c("different"= "darkblue",
                         "same" = "gold")

to_plot_sf <- evap_trend_stats[, .(lon, lat, sign)
][, value := as.numeric(sign)]

sf_to_val <- unique(to_plot_sf[,(.(sign = sign,
                                   value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, sf_to_val, by = "value", all = T)


fig_map_quartile_direction <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "gray90") +
  geom_sf(aes(color = sign, fill = sign), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_sign) +
  scale_color_manual(values = cols_sign,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "", 
       title = "Quartile uncertainty in direction", 
       subtitle = "Trend direction of ensemble quartiles") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.position = "none")

area_stats <- evap_trend_stats[, .(area_fraction = sum(area)/total_area), .(sign)]

area_stats <- area_stats[,area_stats[order(sign, decreasing = T)]]

bar_quartile_direction <- ggplot(area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = sign, x = sign), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_sign)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 50, 10), color = "white") + 
  coord_flip()


## merge ----
legend_grob <- ggplotGrob(bar_quartile_direction)

build <- ggplot_build(fig_map_quartile_direction)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_map_quartile_direction <- fig_map_quartile_direction +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[1] - 0.1 * x_width,
    xmax = x_range[1] + 0.15 * x_width,
    ymin = y_range[1] - 0.15 * y_height,
    ymax = y_range[1] + 0.15 * y_height)


fig <- final_map_quartile_direction+
  theme(plot.margin = margin(0.1, 0.1, 1, 2.5, "cm")) 

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_quartile_uncertainty_direction.png"), 
       width = 12, height = 8)

## Read data
evap_trend_min_max <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_quartile_stats.rds"))

evap_trend_min_max[, Q25_brk := cut(Q25, 
                                    breaks = c(min(Q25)-0.1, -2, -1, -0.5, 0, 0.5, 1, 2, 4, max(Q75)+0.1))]

evap_trend_min_max[, Q75_brk := cut(Q75, 
                                    breaks = c(min(Q25)-0.1, -2, -1, -0.5, 0, 0.5, 1, 2, 4, max(Q75)+0.1))]


# d. Q25 and Q75 ----
cols_Q <- c("(-20.4,-2]" = "darkblue", "(-2,-1]" = "royalblue3",
            "(-1,-0.5]" = "royalblue1", "(-0.5,0]" = "lightblue", 
            "(0,0.5]" = "orange", "(0.5,1]" = "lightcoral" , 
            "(1,2]" = "firebrick", "(2,4]" = "darkred", "(4,21]" = "#330000")


## Q25 ----
to_plot_sf <- evap_trend_min_max[, .(lon, lat, Q25_brk)
][, value := as.numeric(Q25_brk)]

sf_to_val <- unique(to_plot_sf[,(.(Q25_brk = Q25_brk,
                                   value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, sf_to_val, by = "value", all = T)

fig_Q25 <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = Q25_brk, fill = Q25_brk), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_Q,
                    guide = "none") +
  scale_color_manual(values = cols_Q ,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill =  "") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 6) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 6) +
  theme_bw() +
  ggtitle(expression(paste("Lower Quartile ET Trend (Q25) [mm year"^-~2,"]")))+
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.spacing.x = unit(1, "cm"),
        legend.spacing.y = unit(1, "cm"),
        plot.title = element_text(size = 20),
        legend.position = "none")

grid_cell_area <- unique(evap_trend_min_max[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_min_max<- grid_cell_area[evap_trend_min_max, on = .(lon, lat)]
total_area <- evap_trend_min_max[, sum(area)]
area_stats <- evap_trend_min_max[, .(area_fraction = sum(area)/total_area), .(Q25_brk)]
area_stats <- area_stats[,area_stats[order(Q25_brk, decreasing = T)]]

bar_Q25 <- ggplot(area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = Q25_brk, x = Q25_brk), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_Q)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 20, 10), color = "white") + 
  coord_flip()


### merge ----
legend_grob <- ggplotGrob(bar_Q25)

build <- ggplot_build(fig_Q25)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_map_Q25 <- fig_Q25 +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[1] - 0.15 * x_width,
    xmax = x_range[1] + 0.15 * x_width,
    ymin = y_range[1] - 0.25 * y_height,
    ymax = y_range[1] + 0.15 * y_height)


fig <- final_map_Q25+
  theme(plot.margin = margin(0.1, 0.1, 1.0, 4, "cm")) 

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_Q25.png"), 
       width = 12, height = 8)

## Q75 ----
to_plot_sf <- evap_trend_min_max[, .(lon, lat, Q75_brk)
][, value := as.numeric(Q75_brk)]
sf_to_val <- unique(to_plot_sf[,(.(Q75_brk = Q75_brk,
                                   value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, sf_to_val, by = "value", all = T)

fig_Q75 <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = Q75_brk, fill = Q75_brk), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_Q,
                    guide = "none") +
  scale_color_manual(values = cols_Q ,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill =  "") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 6) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 6) +
  theme_bw() +
  ggtitle(expression(paste("Upper Quartile ET Trend (Q75) [mm year"^-~2,"]")))+
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.spacing.x = unit(1, "cm"),
        legend.spacing.y = unit(1, "cm"),
        plot.title = element_text(size = 20),
        legend.position = "none")


area_stats <- evap_trend_min_max[, .(area_fraction = sum(area)/total_area), .(Q75_brk)]
area_stats <- area_stats[,area_stats[order(Q75_brk, decreasing = T)]]

bar_Q75 <- ggplot(area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = Q75_brk, x = Q75_brk), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_Q)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 50, 10), color = "white") + 
  coord_flip()


### merge ----
legend_grob <- ggplotGrob(bar_Q75)

build <- ggplot_build(fig_Q75)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_map_Q75 <- fig_Q75 +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[1] - 0.15 * x_width,
    xmax = x_range[1] + 0.15 * x_width,
    ymin = y_range[1] - 0.25 * y_height,
    ymax = y_range[1] + 0.15 * y_height)


fig <- final_map_Q75+
  theme(plot.margin = margin(0.1, 0.1, 1.0, 4, "cm")) 

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_Q75.png"), 
       width = 12, height = 8)

