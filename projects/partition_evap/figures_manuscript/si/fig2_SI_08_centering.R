# Figure 2 SI 08: centered versus uncentered distributions ----
library(data.table)
library(sf)
library(ggplot2)
library(ggpubr)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")
source("projects/partition_evap/figures_manuscript/source/manuscript_io.R", local = TRUE)


# Configuration ----
paths <- manuscript_paths()

anomaly <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "anomaly_distribution_agreement_index_gridwise.rds"))
agreement <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds"))

stopifnot(!anyDuplicated(anomaly[, .(lat, lon)]),
          !anyDuplicated(agreement[, .(lat, lon)]))
test_merge <- merge(anomaly, agreement, by = c("lat", "lon"))

test_merge[, count_diff:= counts.x -counts.y]
test_merge[, sum(count_diff)]

test_merge[, index_diff := index.x - index.y]

test_merge[index_diff < 0, .N]
test_merge[index_diff > 0, .N]
test_merge[index_diff == 0, .N]
test_merge[, median(index_diff)]
test_merge[, summary(index_diff)]

ggplot(test_merge)+
  geom_point(aes(x = index.x , y = index.y))+
  geom_abline(slope = 1)+
  labs(x = "Calculated with anomaly", y = "Calculated with evap")+
  theme_bw()

improvement <- ggplot(test_merge, aes(x = index_diff)) +
  geom_histogram(
    binwidth = 0.2,
    boundary = 0,
    fill = "#4D648D",
    colour = "white"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent()
  ) +
  labs(
    x = "Increase in grid-cell match ratio",
    y = "Number of grid cells"
  )+
  theme_bw()


match_ratios <- ggplot(test_merge)  +
  geom_boxplot(aes(y = index.x, x = 1)) +
  geom_boxplot(aes(y = index.y, x = 2)) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("ET-centered", "ET")
  ) +
  labs(
    x = "",
    y = "Match ratio"
  )+
  theme_bw()

fig_distribution <- ggarrange(improvement, match_ratios, labels = c("a", "b"))


save_manuscript_figure(fig_distribution, file.path(paths$si, "fig2_SI_08_centering"),
                       c(figure_widths / 2, 10))
