source('source/partition_evap.R')
source('source/partition_evap_graphics.R')

agreement_stats_scale <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "magnitude_agreement_scale_transfer.rds"))

# 8 degrees ----
agreement_stats_scale[, area_8deg := sum(area), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, N_8deg := .N, .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[, median_sIQR_0_25_8deg := median(std_quant_range, na.rm = T), .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[rel_dataset_agreement %in% c("High", "Above average"), higher_agreement_frac_8deg := sum(area)/area_8deg, .(deg_8_lon, deg_8_lat)]
agreement_stats_scale[rel_dataset_agreement %in% c("Low", "Below average"), lower_agreement_frac_8deg := sum(area)/area_8deg, .(deg_8_lon, deg_8_lat)]

# 4 degrees ----
agreement_stats_scale[, area_4deg := sum(area), .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, N_4deg := .N, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[rel_dataset_agreement %in% c("High", "Above average"), higher_agreement_frac_4deg := sum(area)/area_4deg, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[rel_dataset_agreement %in% c("Low", "Below average"), lower_agreement_frac_4deg := sum(area)/area_4deg, .(deg_4_lon, deg_4_lat)]
agreement_stats_scale[, median_sIQR_0_25_4deg := median(std_quant_range, na.rm = T), .(deg_4_lon, deg_4_lat)]

### Comparison of 0.25 to larger grid cell ----

reduction_long <- melt(
  agreement_stats_scale,
  id.vars = c("lon", "lat", "area", "rel_dataset_agreement"),
  measure.vars = c(
    "reduction_1deg",
    "reduction_2deg",
    "reduction_4deg",
    "reduction_8deg"
  ),
  variable.name = "resolution",
  value.name = "sIQR_reduction"
)

reduction_long[
  ,
  resolution := factor(
    resolution,
    levels = c(
      "reduction_1deg",
      "reduction_2deg",
      "reduction_4deg",
      "reduction_8deg"
    ),
    labels = c("1°", "2°", "4°", "8°")
  )
]

sIQR_long <- melt(
  agreement_stats_scale,
  id.vars = c("lon", "lat", "area", "rel_dataset_agreement"),
  measure.vars = c(
    "sIQR_1deg",
    "sIQR_2deg",
    "sIQR_4deg",
    "sIQR_8deg"
  ),
  variable.name = "resolution",
  value.name = "sIQR"
)

sIQR_long[
  ,
  resolution := factor(
    resolution,
    levels = c(
      "sIQR_1deg",
      "sIQR_2deg",
      "sIQR_4deg",
      "sIQR_8deg"
    ),
    labels = c("1°", "2°", "4°", "8°")
  )
]


sIQR_long_merge <- merge(sIQR_long, reduction_long, by = c("lon", "lat", "area", "rel_dataset_agreement", "resolution"))

sIQR_long_merge[, sIQR_red_break := cut(sIQR_reduction, breaks = c(-Inf, -0.1, 0.1, Inf),
                                        labels = c(
                                          "sIQR increased by >0.1",
                                          "Change within ±0.1",
                                          "sIQR decreased by >0.1"
                                        ))]


pathway_proportions <- sIQR_long_merge[,
                                       .(affected_area = sum(area)),
                                       by = .(resolution,rel_dataset_agreement,sIQR_red_break)
][, proportion := affected_area / sum(affected_area),
  by = .(resolution,rel_dataset_agreement)
]

### plots ----
ggplot(
  sIQR_long_merge,
  aes(x = rel_dataset_agreement, y = sIQR_reduction)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~resolution)+
  coord_cartesian(ylim = c(-0.5, 0.5))+
  theme_bw()+
  labs(
    x = "Grid-scale (0.25 degrees) magnitude-agreement class",
    y = expression(sIQR[0.25]-sIQR[aggregated])
  )

### FIGURE agreement class dependence ----
fig_agreement_class <- ggplot(
  sIQR_long_merge,
  aes(x = rel_dataset_agreement, y = sIQR, color = sIQR_red_break)
) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~resolution)+
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  labs(
    x = "Grid-scale (0.25 degrees) magnitude-agreement class",
    y = expression(sIQR[aggregated]
    )
  )+
  scale_colour_manual(
    values = c(
      "sIQR increased by >0.1" = "#D55E00",
      "Change within ±0.1"     = "grey65",
      "sIQR decreased by >0.1" = "#0072B2"
    ),
    labels = c(
      "Higher sIQR (>0.1)",
      "Similar sIQR (±0.1)",
      "Lower sIQR (>0.1)"
    ),
    name = "Change after aggregation"
  )+ theme_scale_grid+
  theme(
    axis.text.x = element_text(angle= 40, hjust = 1),
    legend.position = "top"
  )

ggplot(
  pathway_proportions,
  aes(
    x = rel_dataset_agreement,
    y = proportion,
    fill = sIQR_red_break
  )
) +
  geom_col(width = 0.75) +
  facet_wrap(~ resolution, ncol = 2) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "sIQR increased by >0.1" = "#D55E00",
      "Change within ±0.1"     = "grey65",
      "sIQR decreased by >0.1" = "#0072B2"
    ),
    labels = c(
      "Higher sIQR (>0.1)",
      "Similar sIQR (±0.1)",
      "Lower sIQR (>0.1)"
    ),
  ) +
  labs(
    x = "Magnitude-agreement class at 0.25°",
    y = "Area affected",
    fill = "Change after aggregation"
  ) +
  theme_bw()

### does area fraction of higher agreement result in higher aggregated sIQR-----
agreement_stats_scale_deg_8 <- unique(agreement_stats_scale[,.(higher_agreement_frac_8deg,
                                                               lower_agreement_frac_8deg,
                                                               sIQR_8deg, 
                                                               area_8deg, N_8deg,
                                                               median_sIQR_0_25_8deg)])

agreement_stats_scale_deg_8[is.na(higher_agreement_frac_8deg), higher_agreement_frac_8deg := 0]
agreement_stats_scale_deg_8[is.na(lower_agreement_frac_8deg), lower_agreement_frac_8deg := 0]

agreement_stats_scale_deg_8[N_8deg >= 128 & area_8deg >= 1e+11, .N]
agreement_stats_scale_deg_8[higher_agreement_frac_8deg > 0, .N]

### filter ----
agreement_deg_8_filt <- agreement_stats_scale_deg_8[N_8deg >= 128 & area_8deg >= 1e+11]

agreement_stats_scale_deg_4 <- unique(agreement_stats_scale[,.(higher_agreement_frac_4deg,
                                                               lower_agreement_frac_4deg,
                                                               sIQR_4deg, 
                                                               area_4deg, N_4deg,
                                                               median_sIQR_0_25_4deg)])

agreement_stats_scale_deg_4[is.na(higher_agreement_frac_4deg), higher_agreement_frac_4deg := 0]
agreement_stats_scale_deg_4[is.na(lower_agreement_frac_4deg), lower_agreement_frac_4deg := 0]

agreement_stats_scale_deg_4[N_4deg >= 128/4 & area_4deg >= 1e+11, .N]
agreement_deg_4_filt <- agreement_stats_scale_deg_4[N_4deg >= 128/4 & area_4deg >= 1e+11]

ggplot(agreement_deg_4_filt)+
  geom_point(aes(x = higher_agreement_frac_4deg, y = sIQR_4deg), alpha = 0.5)+
  theme_bw()

### FIGURE  higher area fraction plot -----
higher_af <- ggplot()+
  geom_point(data = agreement_deg_4_filt, 
             aes(x = higher_agreement_frac_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt, 
             aes(x = higher_agreement_frac_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  theme_bw()+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) + 
  labs(
    x = "Area fraction of 0.25° grid cells with higher agreement",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale_grid+
  theme(legend.position = "top")



ggplot()+
  geom_point(data = agreement_deg_4_filt[higher_agreement_frac_4deg == 0], 
             aes(x = median_sIQR_0_25_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt[higher_agreement_frac_8deg == 0], 
             aes(x = median_sIQR_0_25_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    linewidth = 0.7,
    colour = "grey35"
  ) +
  coord_equal(
    xlim = c(0, 1.8),
    ylim = c(0, 1.8)
  ) +
  labs(
    x = "Median sIQR of constituent 0.25° grid cells",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale+
  theme(legend.position = "top")


### FIGURE  lower area fraction plot -----
lower_af <- ggplot()+
  geom_point(data = agreement_deg_4_filt, 
             aes(x = lower_agreement_frac_4deg, y = sIQR_4deg, color = "4 degrees"), alpha = 0.5)+
  geom_point(data = agreement_deg_8_filt, 
             aes(x = lower_agreement_frac_8deg, y = sIQR_8deg, color = "8 degrees"), alpha = 0.5)+
  theme_bw()+
  scale_colour_manual(
    values = c(
      "4 degrees" = "#7B4F9D",
      "8 degrees" = "#168C8C"
    ),
    labels = c(
      "4° × 4°",
      "8° × 8°"
    )
  )+
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) + 
  labs(
    x = "Area fraction of 0.25° grid cells with lower agreement",
    y = "sIQR after spatial aggregation",
    colour = "Aggregation scale"
  ) +
  theme_scale_grid+
  theme(legend.position = "top")

ggarrange(higher_af, lower_af, common.legend = T, nrow = 2)

## Does coarse scale agreement predict fine scale agreement?

ggplot(
  agreement_deg_8_filt,
  aes(
    x = sIQR_8deg,
    y = higher_agreement_frac_8deg,
  )
) +
  geom_point(alpha = 0.45) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "sIQR at 8° × 8°",
    y = "Area fraction of 0.25° grid cells\nwith higher magnitude agreement"
  )+
  theme_bw()+
  theme_scale


agreement_deg_8_filt[, quantile(
  sIQR_8deg)
]

agreement_deg_8_filt[, sIQR_8deg_bin := cut(
  sIQR_8deg,
  breaks = c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.75),
  include.lowest = TRUE,
  right = FALSE
)
]


agreement_deg_8_filt[
  ,
  higher_fraction_bin := fcase(
    higher_agreement_frac_8deg == 0, "0%",
    higher_agreement_frac_8deg <= 0.1, ">0–10%",
    higher_agreement_frac_8deg <= 0.2, "10–20%",
    higher_agreement_frac_8deg <= 0.3, "20–30%",
    higher_agreement_frac_8deg <= 0.4, "30–40%",
    higher_agreement_frac_8deg <= 0.5, "40–50%",
    higher_agreement_frac_8deg <= 0.6, "50–60%",
    higher_agreement_frac_8deg <= 0.7, "60–70%",
    higher_agreement_frac_8deg <= 0.8, "70–80%",
    higher_agreement_frac_8deg <= 0.9, "80–90%",
    default = "90–100%"
  )
]

agreement_deg_8_filt[
  ,
  higher_fraction_bin := factor(
    higher_fraction_bin,
    levels = c(
      "0%", ">0–10%", "10–20%", "20–30%",
      "30–40%", "40–50%", "50–60%",
      "60–70%", "70–80%", "80–90%", "90–100%"
    )
  )
]


agreement_deg_8_filt[higher_agreement_frac_8deg == 0, .N]


heatmap_data <- agreement_deg_8_filt[
  ,
  .N,
  by = .(sIQR_8deg_bin, higher_fraction_bin)
][
  ,
  proportion := N / sum(N),
  by = sIQR_8deg_bin
]

heatmap_data[
  ,
  share_bin := fcase(
    proportion == 0,    "0%",
    proportion <= 0.02, ">0–2%",
    proportion <= 0.05, "2–5%",
    proportion <= 0.10, "5–10%",
    proportion <= 0.20, "10–20%",
    proportion <= 0.40, "20–40%",
    proportion <= 0.60, "40–60%",
    default =            ">60%"
  )
]

### FIGURE heatmap ----
heatmap_data[
  ,
  share_bin := factor(
    share_bin,
    levels = c(
      "0%", ">0–2%", "2–5%", "5–10%",
      "10–20%", "20–40%", "40–60%", ">60%"
    )
  )
]

heatmap <- ggplot(
  heatmap_data,
  aes(
    x = sIQR_8deg_bin,
    y = higher_fraction_bin,
    fill = share_bin
  )
) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "0%"     = "#F7F7F7",
      ">0–2%"  = "#DEEBF7",
      "2–5%"   = "#C6DBEF",
      "5–10%"  = "#9ECAE1",
      "10–20%" = "#6BAED6",
      "20–40%" = "#3182BD",
      "40–60%" = "#3182BD",
      ">60%" = "#08519C"
    ),
    limits = levels(heatmap_data$share_bin),
    breaks = levels(heatmap_data$share_bin),
    drop = FALSE,
    name = "Share within\nsIQR bin"
  )+
  labs(
    x = "sIQR at 8° × 8°",
    y = "Area fraction of 0.25° grid cells\nwith higher magnitude agreement"
  ) +
  theme_scale+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

## ggarange ----
fig_scale_transfer <- ggarrange(fig_agreement_class, heatmap, higher_af,
          lower_af)

ggsave(
  filename = paste0(
    PATH_SAVE_PARTITION_EVAP_FIGURES,
    "main/fig3_scale_transfer.pdf"
  ),
  plot = fig_scale_transfer,
  width = figure_widths,
  height = 23,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)
