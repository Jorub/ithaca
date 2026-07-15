# Figure showing agreement methodology ----
source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

## data ----
agreement <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
figure1_timeseries <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "figure1_timeseries.rds"))
selected_locations <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "figure1_selected_locations.rds"))

# Magnitude agreement -----
agreement_high <- selected_locations[example_label == "M1"]
agreement_low <- selected_locations[example_label == "M2"]

agreement_merge <- data.table::rbindlist(
  list(agreement_high, agreement_low),
  use.names = TRUE,
  fill = TRUE
)

agreement_merge <- subset(agreement_merge, select = c("lon", "lat",
                                                      "rel_dataset_agreement",
                                                      "std_quant_range", "ens_mean_q25",
                                                      "ens_mean_mean", "ens_mean_q75",
                                                      "quant_range"))

agreement_merge[, annotation := sprintf(
  "IQR = %.0f mm yr\u207B\u00B9\nmean ET = %.0f mm yr\u207B\u00B9\nsIQR = %.2f",
  quant_range,
  ens_mean_mean,
  std_quant_range
)]                          

evap_time_high <- figure1_timeseries[example_label == "M1"]
evap_time_high[, mean_evap_dataset := mean(evap), .(dataset)]
evap_time_high_sub <- subset(evap_time_high, select = c("lon", "lat", "mean_evap_dataset",
                                                      "rel_dataset_agreement",
                                                      "std_quant_range", "ens_mean_q25",
                                                      "ens_mean_mean", "ens_mean_q75",
                                                      "quant_range"))

evap_time_low <- figure1_timeseries[example_label == "M2"]
evap_time_low[, mean_evap_dataset := mean(evap), .(dataset)]
evap_time_low_sub <- subset(evap_time_low, select = c("lon", "lat", "mean_evap_dataset",
                                                        "rel_dataset_agreement",
                                                        "std_quant_range", "ens_mean_q25",
                                                        "ens_mean_mean", "ens_mean_q75",
                                                        "quant_range"))

evap_data_a <- merge(evap_time_high_sub, evap_time_low_sub, all = T,
                     by = c("lon", "lat", "mean_evap_dataset",
                            "rel_dataset_agreement",
                            "std_quant_range", "ens_mean_q25",
                            "ens_mean_mean", "ens_mean_q75",
                            "quant_range"))
evap_data_a <- unique(evap_data_a)

evap_data_a[rel_dataset_agreement == "High", location := 1]
evap_data_a[rel_dataset_agreement == "Low", location := 1.2]
agreement_merge[rel_dataset_agreement == "High", location := 1]
agreement_merge[rel_dataset_agreement == "Low", location := 1.2]

evap_data_a[rel_dataset_agreement == "High", location_label := "M1"]
evap_data_a[rel_dataset_agreement == "Low", location_label := "M2"]
agreement_merge[rel_dataset_agreement == "High", location_label := "M1"]
agreement_merge[rel_dataset_agreement == "Low", location_label := "M2"]


## panel a ----
panel_a <- ggplot(evap_data_a)+
  geom_point(aes(y = mean_evap_dataset, x = location_label,
                 col = rel_dataset_agreement), 
             position = position_jitter(width = 0.02), alpha = 0.5)+
  geom_errorbar(data = agreement_merge, 
                aes(ymin = ens_mean_q25, ymax = ens_mean_q75, 
                    x = location_label),
                width = 0.1)+
  annotate(
    "text",
    x = "M2",
    y = 1450,
    label = "IQR == Q[75] - Q[25]",
    parse = TRUE,
    size = 3.5
  )+
  geom_text(data = agreement_merge,
            aes(label = "IQR", color = rel_dataset_agreement,
                x = location_label,
                y = ens_mean_mean
            ), nudge_x = 0.15)+
  scale_color_manual(values = color_agreement)+
  labs(
    y = expression("Product mean ET" ~ "(" * mm ~ yr^{-1} * ")"),
    x = NULL,
    colour = NULL,
    title = "Same IQR, different agreement"
  ) +
  theme_fig1

## panel b ----

set.seed(42)
evap_mask_sample <- agreement[sample(.N, 50)]


panel_b <- ggplot(evap_mask_sample)+
  geom_point(aes(y = quant_range, x = ens_mean_mean),
             alpha = 0.2)+
  geom_point(data = agreement_merge, 
             aes(y = quant_range, x = ens_mean_mean,
                 color = rel_dataset_agreement), 
             size = 4.5, shape = 10)+
  geom_text(data = agreement_merge, 
             aes(label = c("M1", "M2"), y = quant_range, x = ens_mean_mean,
                 color = rel_dataset_agreement), 
             size = 3, nudge_x = -120)+
  scale_color_manual(values = color_agreement)+
  labs(
    y = expression("Interquartile range" ~ "(" * mm ~ yr^{-1} * ")"),
    x = expression("Ensemble mean ET" ~ "(" * mm ~ yr^{-1} * ")"),
    colour = NULL,
    title = "IQR depends on mean ET"
  ) +
  theme_fig1

## panel c ----
panel_c <- ggplot(evap_mask_sample)+
  geom_point(aes(y = std_quant_range, x = ens_mean_mean),
             alpha = 0.2)+
  geom_point(data = agreement_merge, 
             aes(y = std_quant_range, x = ens_mean_mean,
                 color = rel_dataset_agreement), 
             size = 4.5, shape = 10)+
  geom_text(data = agreement_merge, 
            aes(label = c("M1", "M2"), y = std_quant_range, x = ens_mean_mean,
                color = rel_dataset_agreement), 
            size = 3, nudge_x = -120)+
  geom_text(data = agreement_merge[rel_dataset_agreement == "Low"],
            aes(label = "high sIQR\nlow agreement", color = rel_dataset_agreement,
                x = ens_mean_mean+250,
                y = std_quant_range-0.05
            ))+
  geom_text(data = agreement_merge[rel_dataset_agreement == "High"],
            aes(label = "low sIQR\nhigh agreement", color = rel_dataset_agreement,
                x = ens_mean_mean-150,
                y = std_quant_range+0.14
            ))+
  annotate(
    "text",
    x = 1100,
    y = 0.78,
    label = "sIQR == frac(IQR, bar(ET[plain(ensemble)]))",
    parse = TRUE,
    size = 3.5
  )+
  scale_color_manual(values = color_agreement)+
  labs(
    y = expression("Standardized IQR" ~ "(-)"),
    x = expression("Ensemble mean ET" ~ "(" * mm ~ yr^{-1} * ")"),
    colour = NULL,
    title = "Standardized IQR"
  ) +
  theme_fig1


## panel d ----
thresholds_sIQR <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_sIQR.RDS')) 

evap_mask_sample[, rel_dataset_agreement := factor(rel_dataset_agreement, 
                                                   levels = c("High","Above average",
                                                     "Average",
                                                     "Below average",
                                                     "Low")),]

agreement_merge[, rel_dataset_agreement := factor(rel_dataset_agreement, 
                                                   levels = c("High","Above average",
                                                              "Average",
                                                              "Below average",
                                                              "Low")),]


agreement_bands_siqr <- data.frame(
  ymin = c(0.00, 0.10, 0.3, 0.7, 0.90),
  ymax = c(0.10, 0.3, 0.7, 0.90, 1.00),
  xmin = thresholds_sIQR[1:5],
  xmax = c(thresholds_sIQR[2:6]),
  agreement = factor(
    c("High", "Above average", "Average", "Below average", "Low"),
    levels = c("High", "Above average", "Average", "Below average", "Low")
  )
)

agreement_band_labels <- transform(
  agreement_bands_siqr,
  y = (ymin + ymax)/2,
  x = 1.05
)

panel_d <- ggplot(agreement)+
  geom_rect(
    data = agreement_bands_siqr,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = agreement
    ),
    inherit.aes = FALSE,
    alpha = 0.6
  ) +
  stat_ecdf(aes(x = std_quant_range))+
  geom_text(
    data = agreement_band_labels,
    aes(x = x, y = y, label = agreement),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3, color = "gray25" 
  ) +
  scale_fill_manual(values = color_agreement)+
  labs(
    x = expression("Standardized IQR" ~ "(-)"),
    y = expression("Empirical cumulative probability"),
    colour = NULL,
    title = "Agreement classes from sIQR"
  ) +
  geom_point(data = agreement_merge, 
            aes(x = std_quant_range, y = c(0.09, 0.96)), 
            size = 3, shape = 10)+
  geom_text(data = agreement_merge, 
            aes(label = c("M1", "M2"), x = std_quant_range, 
                y = c(0.05, 0.93)), 
            size = 3, nudge_x = c(0.1))+
  geom_hline(yintercept = c(0.1, 0.3, 0.7, 0.9), linewidth = 0.25,
             color = "gray35")+
  coord_cartesian(xlim = c(0, 1.55), expand = F)+
  theme_fig1 


## ggarrange row 1 ----
fig1_row1 <- ggarrange(
  panel_a, panel_b, panel_c, panel_d,
  ncol = 4, nrow = 1,
  labels = c("a", "b", "c", "d"),
  font.label = list(size = 12, face = "bold"),
  align = "hv",
  widths = c(1, 1, 1, 1)
)


# Distribution agreement -----

agreement_select_high <- selected_locations[example_label == "D1"]
agreement_select_low <- selected_locations[example_label == "D2"]

time_series_high <- figure1_timeseries[example_label == "D1"]
time_series_low <- figure1_timeseries[example_label == "D2"]

## panel e ----
label_e <- data.table(
  label = c("D1", "D2"),
  x = c(median(time_series_high$year), median(time_series_low$year)),
  y = c(
    mean(range(time_series_high$evap, na.rm = TRUE)),
    mean(range(time_series_low$evap, na.rm = TRUE))
  ),
  dist_dataset_agreement = c("High", "Low")
)

panel_e <- ggplot()+
  geom_line(data = time_series_high,
            aes(x = year, y = evap, group = dataset, 
                color = dist_dataset_agreement))+
  geom_line(data = time_series_low,
            aes(x = year, y = evap, group = dataset, 
                color = dist_dataset_agreement))+
  geom_label(
    data = label_e,
    aes(
      x = x,
      y = y,
      label = label,
      color = dist_dataset_agreement
    ),
    fill = "white",
    size = 3
  ) +
  scale_color_manual(values = color_agreement)+
  labs(
    y = expression("Product annual ET" ~ "(" * mm ~ yr^{-1} * ")"),
    x = expression("Year"),
    colour = NULL,
    title = "Contrasting annual ET"
  ) +
  theme_fig1

## panel f ----



x1 <- time_series_low[dataset %in% c("bess"), evap]
x2 <- time_series_low[dataset %in% c("gldas-vic"), evap]

# KS test
ks_out <- ks.test(x1, x2)

# ECDF functions
f1 <- ecdf(x1)
f2 <- ecdf(x2)

# Evaluate both ECDFs at all observed ET values
x_all <- sort(unique(c(x1, x2)))

d_all <- abs(f1(x_all) - f2(x_all))

# x-position of maximum vertical distance
x_D <- x_all[which.max(d_all)]

# ECDF values at that point
y1_D <- f1(x_D)
y2_D <- f2(x_D)

D_value <- max(d_all)

ks_annotation_low <- data.table(
  x = x_D,
  y = min(y1_D, y2_D),
  yend = max(y1_D, y2_D),
  label_y = mean(c(y1_D, y2_D)),
  D = D_value,
  p_value = ks_out$p.value
)

x1 <- time_series_high[dataset %in% c("era5-land"), evap]
x2 <- time_series_high[dataset %in% c("terraclimate"), evap]

# KS test
ks_out <- ks.test(x1, x2)

# ECDF functions
f1 <- ecdf(x1)
f2 <- ecdf(x2)

# Evaluate both ECDFs at all observed ET values
x_all <- sort(unique(c(x1, x2)))

d_all <- abs(f1(x_all) - f2(x_all))

# x-position of maximum vertical distance
x_D <- x_all[which.max(d_all)]

# ECDF values at that point
y1_D <- f1(x_D)
y2_D <- f2(x_D)

D_value <- max(d_all)

ks_annotation_high <- data.table(
  x = x_D,
  y = min(y1_D, y2_D),
  yend = max(y1_D, y2_D),
  label_y = mean(c(y1_D, y2_D)),
  D = D_value,
  p_value = ks_out$p.value
)

panel_f <- ggplot()+
  stat_ecdf(data = time_series_high[dataset %in% c("era5-land", "terraclimate")],
            aes(x = evap, group = dataset, 
                color = "match"))+
  stat_ecdf(data = time_series_low[dataset %in% c("bess", "gldas-vic")],
            aes(x = evap, group = dataset, 
                color = "no match"))+
  geom_segment(data = ks_annotation_low,
               aes(x = x,
                   y = y, yend = yend))+
  geom_text(
    data = ks_annotation_low,
    aes(
      x = x+10,
      y = label_y-0.3,
      label = sprintf("D = %.2f", D)
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3.3
  ) +
  geom_text(
    data = ks_annotation_low,
    aes(
      x = x+10,
      y = label_y-0.2,
      label = "no match", color = "no match"
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3.3
  ) +
  geom_segment(data = ks_annotation_high,
               aes(x = x, y = y, yend = yend))+
  geom_text(
    data = ks_annotation_high,
    aes(
      x = x-130,
      y = label_y+0.03,
      label = sprintf("D = %.2f", D)
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3.3
  ) +
  geom_text(
    data = ks_annotation_high,
    aes(
      x = x-130,
      y = label_y+0.11,
      label = "match", color = "match"
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3.3
  ) +
  scale_color_manual(values = match_cols)+
  labs(
    x = expression("Product annual ET" ~ "(" * mm ~ yr^{-1} * ")"),
    y = expression("Empirical cumulative probability"),
    colour = NULL,
    title = "Pairwise ECDF comparison"
  ) +
  theme_fig1


## panel g ----
ks_test_gridwise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise.rds"))

ks_high <- merge(agreement_select_high, ks_test_gridwise, by = c("lon", "lat") )
ks_low <- merge(agreement_select_low, ks_test_gridwise, by = c("lon", "lat") )

ks_panel <- rbind(ks_high, ks_low)
ks_panel[, matching := KS_test_p > 0.05]
ks_panel[matching == T, match := "Match"]
ks_panel[matching == F, match := "No match"]
ks_panel[dist_dataset_agreement == "High", location := "D1"]
ks_panel[dist_dataset_agreement == "Low", location := "D2"]

facet_annotation <- ks_panel[
  ,
  .(
    n_matches = sum(matching),
    n_pairs = .N,
    match_ratio = mean(matching)
  ),
  by = location
]

facet_annotation[, label := sprintf(
  "%s\n%d / %d matches\n%.0f%%",
  location,
  n_matches,
  n_pairs,
  100 * match_ratio
)]

panel_g <- ggplot(ks_panel, aes(x = dataset.x, y = dataset.y, fill = match))+
  geom_tile(color = "white",lwd = 0.8,linetype = 1) +
  scale_fill_manual(values = c("No match" = "gray82","Match" = "gray20"))+
  labs(fill = "", x = "", y = "", title = "Pairwise match matrices")+
  theme_bw()+
  geom_label(
    data = facet_annotation,
    aes(
      x = Inf,
      y = -Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = -0.1,
    size = 3,
    label.size = 0.2,
    fill = "white"
  ) +
  facet_wrap(~location, nrow = 2)+
  theme_fig1+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
  
  
## panel h ----

thresholds_dist <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_distribution_agreement.RDS'))
distribution <- agreement[, .(lon, lat, index = match_ratio)]

match_ratio <- ks_panel[matching == T, .(match_ratio = .N/91), dist_dataset_agreement]

annotation <- data.table(dist_dataset_agreement = c("High",
                                             "Above average",
                                             "Average",
                                             "Below average",
                                             "Low"),
                         match_ratio = c(thresholds_dist[6:2]),
                         yend = c(thresholds_dist[6:2]),
                         y = c(thresholds_dist[5:1]),
                         annotation = c("> 90 %", 
                                        "65-90 %", 
                                        "35-65 %",
                                        "10-35 %",
                                        "< 10 %"))

annotation[, dist_dataset_agreement := factor(dist_dataset_agreement, 
                                              levels = c("High",
                               "Above average",
                               "Average",
                               "Below average",
                               "Low"))]

agreement_bands_dist <- data.frame(
  ymin = c(0.00, 0.10, 0.3, 0.7, 0.90),
  ymax = c(0.10, 0.3, 0.7, 0.90, 1.00),
  xmin = thresholds_dist[1:5],
  xmax = thresholds_dist[2:6],
  agreement = factor(
    c("Low", "Below average", "Average", "Above average", "High"),
    levels = c("Low", "Below average", "Average", "Above average", "High")
  )
)

agreement_band_labels_dist <- transform(
  agreement_bands_dist,
  y = (ymin + ymax) / 2,
  x = 0.7   # adjust if needed
)

panel_h <- ggplot(distribution) +
  geom_rect(
    data = agreement_bands_dist,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = agreement
    ),
    inherit.aes = FALSE,
    alpha = 0.6
  ) +
  stat_ecdf(
    aes(x = index),
    linewidth = 0.5,
    colour = "black"
  ) +
  geom_hline(
    yintercept = c(0.10, 0.3, 0.7, 0.90),
    linewidth = 0.25,
    color = "gray35"
  ) +
  geom_text(
    data = agreement_band_labels_dist,
    aes(x = x, y = y, label = agreement),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3, color = "gray25"
  ) +
  geom_point(data = match_ratio, 
             aes(x = match_ratio, y = c(0.98, 0.09)), 
             size = 3, shape = 10)+
  geom_text(data = match_ratio, 
            aes(label = c("D1", "D2"), x = match_ratio, 
                y = c(0.94, 0.06)), 
            size = 3, nudge_x = c(0.06, 0.06))+
  scale_fill_manual(values = color_agreement, drop = FALSE) +
  coord_cartesian(xlim = c(0, 1))+
  labs(
    x = "Match ratio (-)",
    y = "Empirical cumulative probability",
    fill = NULL,
    title = "Agreement classes from match ratio"
  ) +
  theme_fig1 +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1,
      vjust = 1
    )
  )

## ggarrange row 2 ----

fig1_row2 <- ggarrange(
  panel_e, panel_f, panel_g, panel_h,
  ncol = 4, nrow = 1,
  labels = c("e","f", "g", "h"),
  font.label = list(size = 12, face = "bold"),
  align = "hv",
  widths = c(1, 1, 1, 1)
)


# Patterns ----
example_locations_data <- data.table::copy(figure1_timeseries)

example_locations_data[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
example_locations_data[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
example_locations_data[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
example_locations_data[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]

example_locations_data[rel_dataset_agreement == "High", 
                       rel_dataset_agreement := "High magnitude\nagreement"]

example_locations_data[rel_dataset_agreement == "Low", 
                       rel_dataset_agreement := "Low magnitude\nagreement"]

example_locations_data[dist_dataset_agreement == "High", 
                       dist_dataset_agreement := "High distribution\nagreement"]

example_locations_data[dist_dataset_agreement == "Low", 
                       dist_dataset_agreement := "Low distribution\nagreement"]

example_locations_data[, evap_mean := mean(evap), .(lat, lon, dataset)]
example_locations_data[, ens_mean_mean := mean(evap_mean), .(lat, lon)]

# Annotation labels for the 2 x 2 joint agreement examples
facet_labels <- data.table::data.table(
  rel_dataset_agreement = c(
    "High magnitude\nagreement",
    "High magnitude\nagreement",
    "Low magnitude\nagreement",
    "Low magnitude\nagreement"
  ),
  dist_dataset_agreement = c(
    "High distribution\nagreement",
    "Low distribution\nagreement",
    "High distribution\nagreement",
    "Low distribution\nagreement"
  ),
  example_label = c("D1", "M1", "M2", "D2"),
  example_desc = c(
    "High magnitude + high distribution",
    "High magnitude + low distribution",
    "Low magnitude + high distribution",
    "Low magnitude + low distribution"
  )
)

# Compute label positions within each facet
facet_label_pos <- example_locations_data[
  ,
  .(
    x = min(year, na.rm = TRUE) + 0.04 * diff(range(year, na.rm = TRUE)),
    y = max(evap, na.rm = TRUE) - 0.08 * diff(range(evap, na.rm = TRUE))
  ),
  by = .(rel_dataset_agreement, dist_dataset_agreement)
]


facet_labels <- merge(
  facet_labels,
  facet_label_pos,
  by = c("rel_dataset_agreement", "dist_dataset_agreement"),
  all.x = TRUE
)

facet_labels[, y := max(y), rel_dataset_agreement]


fig_types <- ggplot(example_locations_data)+
  geom_line(aes(y = evap, 
                x = year, group = dataset), linewidth = 0.2)+
  geom_label(
    data = facet_labels,
    aes(
      x = x,
      y = y,
      label = example_label
    ),
    inherit.aes = FALSE,
    label.size = 0.25,
    size = 3.5,
    fontface = "bold",
    fill = "white"
  ) +
  facet_grid(rel_dataset_agreement~dist_dataset_agreement, scales = 'free')+
  labs(y = expression(paste('Product annual ET (mm yr'^-1,')')), x = 'Year')+
  theme_fig1+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "plain"),
        panel.spacing = unit(0.6, "lines"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4))

# ggarrange ----
row1 <- annotate_figure(
  fig1_row1, top = text_grob("Magnitude agreement", face = "bold",
                             size = 13)
)

row2 <- annotate_figure(
  fig1_row2, top = text_grob("Distribution agreement", face = "bold",
                             size = 13)
)

agreement_pattern <- annotate_figure(fig_types,
                              top = text_grob("Joint agreement patterns",
                              face = "bold", size = 13))

figure_1 <- ggarrange(row1, row2, agreement_pattern, 
                      labels = c("", "", "i"),
                      nrow = 3, align = "hv") 

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig1_agreement_metrics.pdf"
  ),
  plot = figure_1,
  width = figure_widths,
  height = 25,
  units = "cm",
  device = cairo_pdf
)
