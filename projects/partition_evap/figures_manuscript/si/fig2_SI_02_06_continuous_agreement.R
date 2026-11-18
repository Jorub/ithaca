# Figure 2 SI 02-06: continuous agreement and ET context ----
library(data.table)
library(sf)
library(stars)
library(raster)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)

source("source/geo_functions.R", local = TRUE)
source("source/graphics.R", local = TRUE)

# Configuration ----
paths <- manuscript_paths()
stems <- c(mean_et = "fig2_SI_02_mean_et", iqr = "fig2_SI_03_iqr",
           siqr = "fig2_SI_04_siqr", match_ratio = "fig2_SI_05_match_ratio",
           iqr_mean_relationship = "fig2_SI_06_iqr_mean_relationship")

# Data and plots ----
# Data ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))

## Continuous maps ----

### IQR ----
p5 <- quantile(dataset_agreement_grid_wise$quant_range, 0.05, na.rm = TRUE)
p50 <- quantile(dataset_agreement_grid_wise$quant_range, 0.50, na.rm = TRUE)
p95 <- quantile(dataset_agreement_grid_wise$quant_range, 0.95, na.rm = TRUE)

break_vals <- c(p5, 50, p50, 150, 200, p95)
leg_labels <- c(paste0("≤", round(p5)), "50",
                paste0(round(p50), " Median"),
                "150", "200", paste0("≥", round(p95)))

leg_labels
col_gradient_values <- c(0, (p50 - p5) / (p95 - p5), 1)

leg_iqr <- arrow_colourbar_vertical(
  limits = c(p5, p95),
  breaks = break_vals,
  labels = leg_labels,
  colours = c(fig5_col_high, fig5_col_mid, fig5_col_low),
  values  = col_gradient_values,   # important for asymmetric placement
  title = "",
  ticks_colour = "black",
  frame_colour = "gray35",
  bar_width = 0.18,
  cap_height = 0.055,
  tick_length = 0.06
)

leg_iqr

fig_IQR <- ggplot(dataset_agreement_grid_wise) +
  geom_raster(aes(fill = quant_range, x = lon, y = lat), interpolate = FALSE) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_gradientn(
    colors = c(fig5_col_high, fig5_col_mid, fig5_col_low),
    values = col_gradient_values,
    limits = c(p5, p95),
    oob = squish,
    guide = guide_colourbar(
      ticks = TRUE,
      ticks.colour = "black",
      frame.colour = "gray35"
    )
  ) +
  labs(x = NULL, y = NULL,
       fill = "",
       title = expression(paste('Inter-quartile range [mm yr'^-1,']'))
       ) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map

fig_IQR
fig_SI_IQR_leg <- ggarrange(fig_IQR, leg_iqr, widths = c(0.85, 0.15))

### mean ET ----
p5 <- quantile(dataset_agreement_grid_wise$ens_mean_mean, 0.05, na.rm = TRUE)
p50 <- quantile(dataset_agreement_grid_wise$ens_mean_mean, 0.50, na.rm = TRUE)
p95 <- quantile(dataset_agreement_grid_wise$ens_mean_mean, 0.95, na.rm = TRUE)

break_vals <- c(p5, 200, p50, 600, 800, 1000, p95)
leg_labels <- c(paste0("≤", round(p5)), "200",
                paste0(round(p50), " Median"),
                "600", "800", "1000", paste0("≥", round(p95)))

leg_labels
col_gradient_values <- c(0, (p50 - p5) / (p95 - p5), 1)

leg_mean <- arrow_colourbar_vertical(
  limits = c(p5, p95),
  breaks = break_vals,
  labels = leg_labels,
  colours = c(fig5_col_high, fig5_col_mid, fig5_col_low),
  values  = col_gradient_values,   # important for asymmetric placement
  title = "",
  ticks_colour = "black",
  frame_colour = "gray35",
  bar_width = 0.18,
  cap_height = 0.055,
  tick_length = 0.06
)

leg_mean

fig_mean <- ggplot(dataset_agreement_grid_wise) +
  geom_raster(aes(fill = ens_mean_mean, x = lon, y = lat), interpolate = FALSE) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_gradientn(
    colors = c(fig5_col_high, fig5_col_mid, fig5_col_low),
    values = col_gradient_values,
    limits = c(p5, p95),
    oob = squish,
    guide = guide_colourbar(
      ticks = TRUE,
      ticks.colour = "black",
      frame.colour = "gray35"
    )
  ) +
  labs(x = NULL, y = NULL,
       fill = "",
       title = expression(paste('Long-term ensemble mean (2000-2019) [mm yr'^-1,']'))
  ) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map

fig_mean
fig_SI_mean_leg <- ggarrange(fig_mean, leg_mean, widths = c(0.85, 0.15))

### sIQR ----

p5 <- quantile(dataset_agreement_grid_wise$std_quant_range, 0.05, na.rm = TRUE)
p50 <- quantile(dataset_agreement_grid_wise$std_quant_range, 0.50, na.rm = TRUE)
p95 <- quantile(dataset_agreement_grid_wise$std_quant_range, 0.95, na.rm = TRUE)

break_vals <- c(p5, p50, 0.4, 0.5, 0.6, p95)

leg_labels <- c(paste0("≤", round(p5,2)),
                paste0(round(p50,2), " Median"),
                "0.40", "0.50", "0.60", paste0("≥", round(p95,2)))

leg_labels
col_gradient_values <- c(0, (p50 - p5) / (p95 - p5), 1)

leg_siqr <- arrow_colourbar_vertical(
  limits = c(p5, p95),
  breaks = break_vals,
  labels = leg_labels,
  colours = c(fig5_col_high, fig5_col_mid, fig5_col_low),
  values  = col_gradient_values,   # important for asymmetric placement
  title = "",
  ticks_colour = "black",
  frame_colour = "gray35",
  bar_width = 0.18,
  cap_height = 0.055,
  tick_length = 0.06
)

leg_siqr

fig_sIQR <- ggplot(dataset_agreement_grid_wise) +
  geom_tile(aes(fill = std_quant_range, x = lon, y = lat)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_gradientn(
    colors = c(fig5_col_high, fig5_col_mid, fig5_col_low),
    values = col_gradient_values,
    limits = c(p5, p95),
    oob = squish,
    guide = guide_colourbar(
      ticks = TRUE,
      ticks.colour = "black",
      frame.colour = "gray35"
    )
  ) +
  labs(x = NULL, y = NULL,
       fill = expression(paste('')),
       col = "",
       title = expression(paste('Standardized inter-quartile range [-]'))
  ) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map

fig_SI_sIQR_leg <- ggarrange(fig_sIQR, leg_siqr, widths = c(0.85, 0.15))

### match ratio ----

p5 <- quantile(dataset_agreement_grid_wise$match_ratio, 0.05, na.rm = TRUE)
p50 <- quantile(dataset_agreement_grid_wise$match_ratio, 0.50, na.rm = TRUE)
p95 <- quantile(dataset_agreement_grid_wise$match_ratio, 0.95, na.rm = TRUE)

dataset_agreement_grid_wise[, summary(match_ratio)]
break_vals <- c(p5, p50, 0.30, 0.40, p95)

leg_labels <- c(paste0("≤", round(p5,2)),
                paste0(round(p50,2), " Median"),
                "0.30", "0.40", paste0("≥", round(p95,2)))

leg_labels
col_gradient_values <- c(0, (p50 - p5) / (p95 - p5), 1)

leg_match <- arrow_colourbar_vertical(
  limits = c(p5, p95),
  breaks = break_vals,
  labels = leg_labels,
  colours = c(fig5_col_high, fig5_col_mid, fig5_col_low),
  values  = col_gradient_values,   # important for asymmetric placement
  title = "",
  ticks_colour = "black",
  frame_colour = "gray35",
  bar_width = 0.18,
  cap_height = 0.055,
  tick_length = 0.06
)

leg_match

fig_match <- ggplot(dataset_agreement_grid_wise) +
  geom_tile(aes(fill = match_ratio, x = lon, y = lat)) +
  geom_sf(data = world_no_antarctica, fill = NA, color = "gray35") +
  scale_fill_gradientn(
    colors = c(fig5_col_high, fig5_col_mid, fig5_col_low),
    values = col_gradient_values,
    limits = c(p5, p95),
    oob = squish,
    guide = guide_colourbar(
      ticks = TRUE,
      ticks.colour = "black",
      frame.colour = "gray35"
    )
  ) +
  labs(x = NULL, y = NULL,
       fill = expression(paste('')),
       col = "",
       title = expression(paste('KS-test based match ratio [-]'))
  ) +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  coord_sf(ylim = c(-70, 90), expand = F)+
  theme_map

fig_SI_match_leg <- ggarrange(fig_match, leg_match, widths = c(0.85, 0.15))

## IQR over ensemble mean

lm_model <- lm(quant_range ~ ens_mean_mean+0 , data = dataset_agreement_grid_wise)
summary(lm_model)

model_slope <- lm_model$coefficients
model_r2 <- summary(lm_model)$r.squared

eq_label <- paste0(
  "hat(IQR)[ET] == ", round(model_slope, 2),
  " %.% bar(ET)[ensemble]",
  "*',' ~~ R^2 == ", round(model_r2, 2)
)

label_data <- data.frame(
  x = Inf,
  y = Inf,
  label = eq_label
)

fig_SI_IQR_mean_dependence <- ggplot(dataset_agreement_grid_wise,
       aes(x = ens_mean_mean, y = quant_range))+
  geom_point(alpha = 0.5, color = "gray70", pch = 1)+
  geom_density_2d()+
  geom_abline(slope = model_slope[1], intercept = 0, lwd = 1.2)+
  labs(x = expression(paste('Long-term ensemble mean (2000-2019) [mm yr'^-1,']')),
       y = expression(paste('Inter-quartile range of long-term means [mm yr'^-1,']')))+
  scale_y_continuous(limits = c(0, 500))+
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    parse = TRUE,
    hjust = 1.05,
    vjust = 1.2,
    size = 4
  ) +
  theme_fig1

panels <- list(mean_et = fig_SI_mean_leg, iqr = fig_SI_IQR_leg,
     siqr = fig_SI_sIQR_leg, match_ratio = fig_SI_match_leg,
     iqr_mean_relationship = fig_SI_IQR_mean_dependence)

for (name in names(stems)) {
  size <- if (name == "iqr_mean_relationship") c(figure_widths, figure_widths) else
    c(figure_widths / 2, 10)
  save_manuscript_figure(panels[[name]], file.path(paths$si, stems[[name]]), size)
}
