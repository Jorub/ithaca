source('source/evap_trend.R')

packages <- c(
  "ggplot2",
  "ggpubr",
  "ggnewscale",
  "ggrepel",
  "cowplot",
  "scales",
  "rnaturalearth",
  "colorspace"
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


standardize_level_colors <- function(colors, levels) {
  
  # Extract the hue of each existing colour
  hcl_coordinates <- coords(
    as(hex2RGB(colors), "polarLUV")
  )
  
  hue <- hcl_coordinates[, "H"]
  
  # Fixed perceptual lightness and chroma for each level
  target_lightness <- fifelse(
    levels == 0.2, 90,
    fifelse(levels == 0.4, 68, 48)
  )
  
  target_chroma <- fifelse(
    levels == 0.2, 30,
    fifelse(levels == 0.4, 45, 50)
  )
  
  hex(
    polarLUV(
      L = target_lightness,
      C = target_chroma,
      H = hue
    ),
    fixup = TRUE
  )
}

map_theme <- theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
                           panel.border = element_blank(),
                           axis.ticks.length = unit(0, "cm"),
                           panel.grid.major = element_line(colour = "gray60"),
                           axis.text = element_blank(), 
                           axis.title = element_text(size = 16), 
                           legend.text = element_text(size = 12), 
                           legend.title = element_text(size = 16),
                           legend.position = "none",
                           margin(t = 0.1, r = 0.1, b = 1, l = 2.5, unit = "cm"))

bar_theme <- theme(plot.title = element_text(size = 8, hjust = 0), 
                   axis.text.y = element_text(size = 9), 
                   axis.text.x = element_text(size = 9),
                   axis.line = element_blank(),
                   axis.ticks = element_blank(),
                   legend.position = "none",
                   panel.background = element_rect(fill = "transparent",colour = NA),
                   plot.background = element_rect(fill = "transparent",colour = NA))
  

map_theme_trend <- theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
                   panel.border = element_blank(),
                   axis.ticks.length = unit(0, "cm"),
                   panel.grid.major = element_line(colour = "gray60"),
                   axis.text = element_blank(), 
                   axis.title = element_text(size = 16), 
                   legend.text = element_text(size = 12), 
                   legend.title = element_text(size = 16),
                   margin(t = 0.1, r = 0.1, b = 1, l = 2.5, unit = "cm"))
