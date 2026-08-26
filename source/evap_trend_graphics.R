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

## colors ----

region_colors <- c(
  ARP = "#F7E7A9",
  SAH = "#D8AE38", WAF = "#D8AE38",
  
  CAR = "#86B6D8", EAS = "#86B6D8",
  CAF = "#2166AC", ECA = "#2166AC",
  SCA = "#2166AC", SEA = "#2166AC",
  ESB = "#238B9F", TIB = "#238B9F",
  SAM = "#8996C7", SAS = "#8996C7", SEAF = "#8996C7",
  SES = "#4E5AA7", WNA = "#4E5AA7",
  
  CAU = "#FAD7A0",
  EAU = "#E6953B", SAU = "#E6953B",
  
  CNA = "#F1C2BA", EEU = "#F1C2BA", NAU = "#F1C2BA",
  MED = "#C9786B", WCE = "#C9786B",
  
  NES = "#D7E9CC",
  ENA = "#8EBD69", MDG = "#8EBD69", NZ = "#8EBD69",
  GIC = "#347C52", SWS = "#347C52",
  
  NCA = "#E6DFC0",
  NEAF = "#AA9C59",
  ESAF = "#68652D", WSAF = "#68652D",
  
  NWN = "#9B88C7", WSB = "#9B88C7",
  NEN = "#5B3F99", NSA = "#5B3F99", WCA = "#5B3F99",
  NEU = "#C184B3", SSA = "#C184B3",
  NWS = "#B36F91",
  RAR = "#763457", RFE = "#763457"
)
  