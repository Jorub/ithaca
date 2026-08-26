source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

library(data.table)

color_key <- data.table(
  broad_group = c(
    1, 1,
    2, 2, 2, 2, 2,
    3, 3,
    4, 4,
    5, 5, 5,
    6, 6, 6,
    7, 7, 7, 7, 7
  ),
  color = c(
    "#F7E7A9", "#D8AE38",
    "#86B6D8", "#2166AC", "#238B9F", "#8996C7", "#4E5AA7",
    "#FAD7A0", "#E6953B",
    "#F1C2BA", "#C9786B",
    "#D7E9CC", "#8EBD69", "#347C52",
    "#E6DFC0", "#AA9C59", "#68652D",
    "#9B88C7", "#5B3F99", "#C184B3", "#B36F91", "#763457"
  ),
  level = c(
    0.2, 0.4,
    0.4, 0.6, 0.6, 0.4, 0.6,
    0.2, 0.4,
    0.2, 0.4,
    0.2, 0.4, 0.6,
    0.2, 0.4, 0.6,
    0.4, 0.6, 0.4, 0.4, 0.6
  )
)

color_key[, subgroup := c(
  "1a", "1b",
  "2a", "2a1", "2a2", "2b", "2b1",
  "3a", "3b",
  "4a", "4b",
  "5a", "5b", "5c",
  "6a", "6b", "6c",
  "7a", "7a1", "7b", "7c", "7c1"
)]

color_key[, x_plot := as.numeric(broad_group)]

color_key[, x_plot := fcase(
  subgroup == "2a",  1.90,
  subgroup == "2a1", 1.8,
  subgroup == "2a2", 2,
  subgroup == "2b",  2.30,
  subgroup == "2b1", 2.30,
  
  subgroup == "7a",  6.8,
  subgroup == "7a1", 6.8,
  subgroup == "7b",  7.10,
  subgroup == "7c",  7.3,
  subgroup == "7c1", 7.3,
  
  default = x_plot
)]

colors <- color_key$color
names(colors) <- color_key$color

ggplot() +
  geom_point(
    data = color_key,
    aes(x = x_plot, y = level, col = color),
    size = 5
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = 1:7,
    labels = paste("Group", 1:7)
  ) +
  scale_y_continuous(
    breaks = c(0.2, 0.4, 0.6),
    labels = c("≥ 0.2", "≥ 0.4", "≥ 0.6")
  ) +
  labs(
    x = "",
    y = "Mean Spearman correlation threshold"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
