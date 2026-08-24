source('source/evap_trend.R')

packages <- c(
  "ggplot2",
  "ggpubr",
  "ggnewscale",
  "ggrepel",
  "cowplot",
  "scales",
  "rnaturalearth"
)

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))

library(ggplot2)
library(ggpubr)
library(ggnewscale)
library(ggrepel)
library(cowplot)
library(scales)
library(grid)
library(rnaturalearth)

## colors ----

## themes ----

theme_standard <- theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )


theme_fig3 <- theme(axis.text = element_text(size = 18), 
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 24, hjust = 0, face = "bold"),
      plot.margin = unit(c(0.5,0,0,0.5), "cm"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16,
                                 margin = margin(r = 10, unit = "pt")),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      strip.text = element_text(size = 16))
  