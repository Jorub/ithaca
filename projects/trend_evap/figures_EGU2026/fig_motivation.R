# Figure 1: The motivation  -----
# a. Global ET trends across ET products 
# b. Significant trends are opposing and area fraction bar plots
# c. Majority trend direction DCI of all trends and area fraction bar plots
# d. Quartile uncertainty groups and area fraction bar plots

# source -----
source('source/evap_trend.R')
source('source/geo_functions.R')

# library -----
library(rnaturalearth)
library(ggpubr)

# Map preparation -----
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


# a. Global ET trends across ET products  ----
## data ----
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_global_evap_trend.rds"))
evap_annual_trend[, trend_significance := factor(trend_significance, 
                                                 levels = c("\u2264 0.01", "\u2264 0.05",
                                                            "\u2264 0.1",
                                                            "\u2264 1"))]

dataset_list <- evap_annual_trend[, unique(dataset)]
evap_annual_trend[, dataset := factor(dataset, levels = dataset_list)]

fig_trend <- ggplot(evap_annual_trend)+
  geom_segment(aes(x = dataset, y = lower, yend = upper), lwd = 0.5)+
  geom_point(aes(x = dataset, y = slope, col = as.factor(trend_significance)), 
             size = 2)+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_color_manual(values = c("\u2264 1" = "gray40", 
                                "\u2264 0.01" = "darkblue",
                                "\u2264 0.05" = "darkorchid2",
                                "\u2264 0.1" = "lightcoral"))+
  labs(y = expression(paste("ET trend [mm year"^-~2,"] ")), 
       color = "P-value ", x = "Dataset",
       title = "Global ET trend across datasets")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1, size = 12),
        axis.text.y = element_text(size = 10))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 10), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = "bottom")+
  theme(plot.margin = unit(c(1,1,1,1), 'cm'))+
  guides(color = guide_legend(ncol = 4, byrow = TRUE))

# b. Significant trends are opposing and area fraction bar plots ----
## data ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_grid_trend_stats.rds"))

to_plot_sf <- evap_index[, .(lon, lat, p_val_opposing)
][, value := as.numeric(p_val_opposing)]

problem_to_val <- unique(to_plot_sf[,(.(p_val_opposing = p_val_opposing,
                                        value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()


to_plot_sf <- merge(to_plot_sf, problem_to_val, by = "value", all = T)


cols_opposing <- c("\u2264 0.01" = "darkblue",
                   "\u2264 0.05" = "darkorchid3",
                   "\u2264 0.1" = "lightcoral",
                   "\u2264 1" = "gold", 
                   "None" = "royalblue1")

fig_map_opposing <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "transparent") +
  geom_sf(aes(fill = p_val_opposing, color = p_val_opposing), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_opposing) +
  scale_color_manual(values = cols_opposing,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "",
       title = "Significant trends oppose",
       subtitle = "Onset of opposition (p-value)") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), 
        panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18),
        legend.position = "none",
        margin(l = 100, b = 30))

## barplot ----

grid_cell_area <- unique(evap_index[, .(lon, lat)]) %>% grid_area() # m2
evap_index <- grid_cell_area[evap_index, on = .(lon, lat)]
total_area <- evap_index[, sum(area)]
opposing_area_stats <- evap_index[, .(area_fraction = sum(area)/total_area), .(p_val_opposing)]
opposing_area_stats  <- opposing_area_stats[,opposing_area_stats[order(p_val_opposing, decreasing = T)]]

bar_opposing <- ggplot(opposing_area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = p_val_opposing, x = p_val_opposing), stat = "identity") +
  ylab('') +
  xlab('')+
  labs(fill = '', title = "Area fraction [%]") +
  scale_fill_manual(values = cols_opposing)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 60, 10), color = "white") + 
  coord_flip()

## merge ----
legend_grob = ggplotGrob(bar_opposing)

build <- ggplot_build(fig_map_opposing)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_opposition_plot <- fig_map_opposing +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[2] - 0.195 * x_width,
    xmax = x_range[2] + 0.2 * x_width,
    ymin = y_range[1] - 0.35 * y_height,
    ymax = y_range[1] + 0.25 * y_height)

final_opposition_plot


# c. Majority trend direction DCI of all trends and area fraction bar plots----
## map ----
to_plot_sf <- evap_index[, .(lon, lat, DCI_all_brk)
][, value := as.numeric(DCI_all_brk)]

problem_to_val <- unique(to_plot_sf[,(.(DCI_all_brk = DCI_all_brk,
                                        value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, problem_to_val, by = "value", all = T)

cols_DCI <- c("[-1,-0.5]" = "darkblue","(-0.5,-0.07]" = "lightblue",
              "(-0.07,0.07]" = "gray90", "(0.07,0.5]" = "orange",
              "(0.5,1]" = "darkred")

fig_map_DCI <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "transparent") +
  geom_sf(aes(fill = DCI_all_brk, color = DCI_all_brk ), lwd = 0.03) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_DCI) +
  scale_color_manual(values = cols_DCI,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "", title = "Majority trend direction",
       subtitle = "Trend concurrence index") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
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
        margin(l = 100, b = 30))

## data ----
DCI_area_stats <- evap_index[, .(area_fraction = sum(area)/total_area), .(DCI_all_brk)]
DCI_area_stats  <- DCI_area_stats[,DCI_area_stats[order(DCI_all_brk, decreasing = T)]]

## barplot ----
bar_DCI <- ggplot(DCI_area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = DCI_all_brk, x = DCI_all_brk), stat = "identity") +
  xlab('')  +
  ylab('')  +
  labs(fill = '', title = "Area fraction [%]")  +
  scale_fill_manual(values = cols_DCI)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 40, 10), color = "white") + 
  coord_flip()

## merge ----
legend_grob = ggplotGrob(bar_DCI)

build <- ggplot_build(fig_map_DCI)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_DCI_plot <- fig_map_DCI +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[2] - 0.22 * x_width,
    xmax = x_range[2] + 0.2 * x_width,
    ymin = y_range[1] - 0.35 * y_height,
    ymax = y_range[1] + 0.25 * y_height)

final_DCI_plot

# d. Quartile uncertainty groups and area fraction bar plots ----
#cols_problem <- c("Direction\nmagnitude" = "#330000", "Direction" = "darkred","Magnitude" = "orange2", 
#                  "Small trend:\ndirection" ="royalblue3", 
#                  "Small trend:\nmagnitude" = "lightblue", "None" = "forestgreen")

cols_problem <- c("Both" = "#330000", 
                  "Direction" = "darkred",
                  "Magnitude" = "orange2", 
                  "None" = "royalblue1")

## data ----
evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_grid_quartile_stats.rds"))

evap_trend_stats[fold_brk == "(1,3.3]" & sign == "same", problem := "None"] 

evap_trend_stats[fold_brk == "(1,3.3]" & sign == "different", problem := "Direction"] 

evap_trend_stats[fold_brk == "(3.3,Inf]" & sign == "same", problem := "Magnitude"] 

evap_trend_stats[fold_brk == "(3.3,Inf]" & sign == "different", problem := "Both"] 

category_list <- evap_trend_stats[, unique(problem)]
category_level <- category_list[c(4,3,1,2)]
evap_trend_stats[, problem:= factor(problem, levels = category_level)]

## map ----
to_plot_sf <- evap_trend_stats[, .(lon, lat, problem)
][, value := as.numeric(problem)]

problem_to_val <- unique(to_plot_sf[,(.(problem = problem, value = value))])

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- merge(to_plot_sf, problem_to_val, by = "value", all = T)

map_problem <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray90", color = "transparent") +
  geom_sf(aes(fill = as.factor(problem), color = as.factor(problem)), lwd = 0.02) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_problem) +
  scale_color_manual(values = cols_problem,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "", title = "Quartile uncertainty in magnitude and direction") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        margin(l = 100, b = 30))


## barplot ----

grid_cell_area <- unique(evap_trend_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_stats <- grid_cell_area[evap_trend_stats, on = .(lon, lat)]
total_area <- evap_trend_stats[, sum(area)]
problem_area_stats <- evap_trend_stats[, .(area_fraction = sum(area)/total_area), .(problem)]

problem_area_stats <- problem_area_stats[,problem_area_stats[order(problem, decreasing = T)]]


bar_problem <- ggplot(problem_area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = problem, x = problem), stat = "identity") +
  xlab('')  +
  ylab('')  +
  scale_fill_manual(values = cols_problem)+
  labs(fill = '', title = "Area fraction [%]")  +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0), 
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_hline(yintercept = seq(0, 30, 10), color = "white") + 
  coord_flip()


## merge ----
legend_grob = ggplotGrob(bar_problem)

build <- ggplot_build(map_problem)
x_range <- build$layout$panel_params[[1]]$x_range
y_range <- build$layout$panel_params[[1]]$y_range

x_width  <- diff(x_range)
y_height <- diff(y_range)

final_problem_plot <- map_problem +
  annotation_custom(
    grob = legend_grob,
    xmin = x_range[2] - 0.22 * x_width,
    xmax = x_range[2] + 0.15 * x_width,
    ymin = y_range[1] - 0.35 * y_height,
    ymax = y_range[1] + 0.25 * y_height)

final_problem_plot

# arrange final plot ----

fig1 <- ggarrange(fig_trend, NULL, final_DCI_plot, 
                  final_problem_plot, NULL, final_opposition_plot,
                  nrow = 2, ncol = 3,
                  widths = c(1, 0.25, 1),
                  labels = c("a", "", "b", "c", "", "d"))+
  theme(plot.margin = margin(0.1, 3.1, 1.5, 1.5, "cm")) 

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig1_motivation.png"), 
       width = 12, height = 9, bg = "white")
