# ============================================================
# Mock Figure 3 panel: paired stacked bars with ggnewscale
# - x positions are numeric
# - quartile agreement uses blue palette
# - distribution agreement uses orange palette
# - joint lower/higher shown below
# ============================================================

library(data.table)
library(ggplot2)
library(ggnewscale)
library(cowplot)
library(scales)

# -----------------------------
# 1. Levels and colors
# -----------------------------

agreement_levels <- c(
  "Low",
  "Below average",
  "Average",
  "Above average",
  "High"
)

color_quartile_agreement <- c(
  "Low" = "#EAF2FB",
  "Below average" = "#BFD7EE",
  "Average" = "#78AEDA",
  "Above average" = "#347DBB",
  "High" = "#0B3C6D"
)

color_distribution_agreement <- c(
  "Low" = "#FFF1E6",
  "Below average" = "#FBC89A",
  "Average" = "#F59A4A",
  "Above average" = "#D95F02",
  "High" = "#8C2D04"
)

color_joint_agreement <- c(
  "Joint lower" = "#B75D69",
  "Joint higher" = "#2A9D8F"
)

# -----------------------------
# 2. Mock data
# -----------------------------

evap_classes <- c("Q1 low", "Q2", "Q3", "Q4", "Q5 high")

evap_agreement <- data.table(
  evap_quant = rep(evap_classes, each = 2 * length(agreement_levels)),
  group = rep(
    rep(
      c("Quartile agreement", "Distribution agreement"),
      each = length(agreement_levels)
    ),
    times = length(evap_classes)
  ),
  dataset_agreement = rep(
    agreement_levels,
    times = length(evap_classes) * 2
  ),
  area_fraction = c(
    # Q1 low
    0.30, 0.25, 0.20, 0.15, 0.10,
    0.40, 0.25, 0.18, 0.10, 0.07,
    
    # Q2
    0.22, 0.25, 0.22, 0.18, 0.13,
    0.30, 0.25, 0.20, 0.15, 0.10,
    
    # Q3
    0.16, 0.22, 0.24, 0.22, 0.16,
    0.22, 0.24, 0.22, 0.18, 0.14,
    
    # Q4
    0.10, 0.18, 0.23, 0.25, 0.24,
    0.16, 0.20, 0.24, 0.22, 0.18,
    
    # Q5 high
    0.06, 0.12, 0.20, 0.25, 0.37,
    0.10, 0.16, 0.22, 0.24, 0.28
  )
)

evap_agreement[
  , evap_quant := factor(evap_quant, levels = evap_classes)
]

evap_agreement[
  , dataset_agreement := factor(
    dataset_agreement,
    levels = agreement_levels
  )
]

evap_agreement[
  , group := factor(
    group,
    levels = c("Quartile agreement", "Distribution agreement")
  )
]

# -----------------------------
# 3. Numeric x positions
# -----------------------------

evap_agreement[, x_base := as.numeric(evap_quant)]

evap_agreement[
  group == "Quartile agreement",
  x_pos := x_base - 0.18
]

evap_agreement[
  group == "Distribution agreement",
  x_pos := x_base + 0.18
]

# -----------------------------
# 4. Split data for ggnewscale
# -----------------------------

evap_quartile <- evap_agreement[group == "Quartile agreement"]
evap_distribution <- evap_agreement[group == "Distribution agreement"]

# -----------------------------
# 5. Main plot: paired stacked bars
# -----------------------------

p_main <- ggplot() +
  geom_col(
    data = evap_quartile,
    aes(
      x = x_pos,
      y = area_fraction,
      fill = dataset_agreement
    ),
    width = 0.32
  ) +
  scale_fill_manual(
    values = color_quartile_agreement,
    limits = agreement_levels,
    breaks = agreement_levels,
    drop = FALSE,
    name = "Quartile agreement"
  ) +
  ggnewscale::new_scale_fill() +
  geom_col(
    data = evap_distribution,
    aes(
      x = x_pos,
      y = area_fraction,
      fill = dataset_agreement
    ),
    width = 0.32
  ) +
  scale_fill_manual(
    values = color_distribution_agreement,
    limits = agreement_levels,
    breaks = agreement_levels,
    drop = FALSE,
    name = "Distribution agreement"
  ) +
  scale_x_continuous(
    breaks = seq_along(evap_classes),
    labels = evap_classes,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Area fraction"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, margin = margin(t = 3)),
    axis.title.y = element_text(size = 9),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.32, "cm"),
    plot.margin = margin(t = 3, r = 4, b = 2, l = 4)
  )

# -----------------------------
# 6. Joint agreement data
# -----------------------------

joint_agreement <- data.table(
  evap_quant = rep(evap_classes, each = 2),
  joint_class = rep(
    c("Joint lower", "Joint higher"),
    times = length(evap_classes)
  ),
  area_fraction = c(
    0.14, 0.05,
    0.11, 0.08,
    0.09, 0.11,
    0.06, 0.14,
    0.04, 0.17
  )
)

joint_agreement[
  , evap_quant := factor(evap_quant, levels = evap_classes)
]

joint_agreement[
  , joint_class := factor(
    joint_class,
    levels = c("Joint lower", "Joint higher")
  )
]

joint_agreement[, x_base := as.numeric(evap_quant)]

joint_agreement[
  joint_class == "Joint lower",
  x_pos := x_base - 0.13
]

joint_agreement[
  joint_class == "Joint higher",
  x_pos := x_base + 0.13
]

# -----------------------------
# 7. Joint agreement plot
# -----------------------------

p_joint <- ggplot(
  joint_agreement,
  aes(
    x = x_pos,
    y = area_fraction,
    fill = joint_class
  )
) +
  geom_col(width = 0.22) +
  scale_fill_manual(
    values = color_joint_agreement,
    drop = FALSE,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq_along(evap_classes),
    labels = evap_classes,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.1, 0.2),
    limits = c(0, 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Area fraction",
    title = "Joint agreement"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, margin = margin(t = 3)),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.32, "cm"),
    plot.margin = margin(t = 2, r = 4, b = 4, l = 4)
  )

# -----------------------------
# 8. Assemble panel
# -----------------------------

panel_title <- ggdraw() +
  draw_label(
    "A  ET quantiles",
    x = 0,
    hjust = 0,
    fontface = "bold",
    size = 11
  )

fig3_mock_panel <- plot_grid(
  panel_title,
  p_main,
  p_joint,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.35),
  align = "v"
)

fig3_mock_panel