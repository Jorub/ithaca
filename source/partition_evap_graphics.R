# figure support ----
source('source/partition_evap.R')

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



## color ----

color_agreement <- c("Low" = "#A63A3A",
                     "Below average" = "#E38B75", 
                     "Average" = "#F4CC70",
                     "Above average"= "#97B8C2",
                     "High" = "#4D648D")

match_cols <- c(
  "match" = "grey20",
  "no match" = "grey82"
)



color_joint_agreement <- c("Both lower" = "#A63A3A",
                           "Both higher" = "#4D648D",
                           "Magnitude lower \nDistribution higher" = "#E2A374", 
                           "Magnitude higher \nDistribution lower"= "#6E5773",
                           "Average" = "gray90")


color_quartile_agreement <- c(
  "Low" = "#6A51A3",
  "Below average" = "#B2ABD2",
  "Average" = "#F7F7F7",
  "Above average" = "#92C5DE",
  "High" = "#0571B0"
)

color_distribution_agreement <- c(
  "Low" = "#A6611A",
  "Below average" = "#DFC27D",
  "Average" = "#F7F7F7",
  "Above average" = "#80CDC1",
  "High" = "#018571"
)

color_joint <- c("Both higher" = "#4D648D", "Both lower" = "#A63A3A")

color_global_positioning <- c("Closest"= "gray75","Higher" = "#0072B2", "Lower" = "#D55E00")


dataset_diff_cols <- c("Lower in biome" =  "#D55E00", 
                       "Similar" = "gray85",
                       "Higher in biome" = "#0072B2")


fig5_col_low  <- "#D55E00"   
fig5_col_mid  <- "#F7F7F7"
fig5_col_high <- "#0072B2"
fig5_col_neut <- "grey65"
fig5_col_dark <- "grey25"
  
color_ref <- c(
          "Below WB"   = fig5_col_low,
          "Crosses WB" = fig5_col_neut,
          "Above WB"   = fig5_col_high
        )
        

colors_agreement_strip <- c(
  "[0,2]" = "#EEF1F6",
  "(2,12]" = "#C8D0DE",
  "(12,42]" = "#8999B3",
  "(42,94]" = "#4D648D"
)
### SI colors

iqr_pal <- c(
  "(0,50]"        = "#EAF7FF",  # very light cyan
  "(50,100]"      = "#73C9F4",  # clear sky blue
  "(100,150]"     = "#0072B2",  # strong blue
  "(150,300]"     = "#3B2A8C",  # deep violet
  "(300,1.27e+03]" = "#000000"  # black
)

sIQR_pal <- c(
  "(0,0.2]"  = "#EAF7FF",  # very light mint
  "(0.2,0.3]" = "#73C9F4",  # distinct teal
  "(0.3,0.4]" = "#0072B2",  # saturated blue
  "(0.4,0.74]" = "#3B2A8C",  # saturated blue
  "(0.4,53]"  = "#000000"   # extreme standardized spread
)

cols_kg <- c(
  # A: equatorial / tropical
  "Af"  = "#960000",
  "Am"  = "#FF0000",
  "As"  = "#FF9999",
  "Aw"  = "#FFCDCD",
  
  # B: arid
  "BWk" = "#FFFF64",
  "BWh" = "#FFCD00",
  "BSk" = "#CDAA54",
  "BSh" = "#CD8D13",
  
  # C: warm temperate
  "Cfa" = "#003200",
  "Cfb" = "#005000",
  "Cfc" = "#007800",
  "Csa" = "#00FF00",
  "Csb" = "#96FF00",
  "Csc" = "#C8FF00",
  "Cwa" = "#B46400",
  "Cwb" = "#966400",
  "Cwc" = "#5A3C00",
  
  # D: snow / continental
  "Dfa" = "#320032",
  "Dfb" = "#640064",
  "Dfc" = "#C800C8",
  "Dfd" = "#C71485",
  "Dsa" = "#FF6DFF",
  "Dsb" = "#FFB4FF",
  "Dsc" = "#E6C8FF",
  "Dsd" = "#C8C8C8",
  "Dwa" = "#C8B4FF",
  "Dwb" = "#9A7FB3",
  "Dwc" = "#8859B3",
  "Dwd" = "#6F24B3",
  
  # E: polar
  "EF"  = "#6496FF",
  "ET"  = "#64FFFF"
)

## Themes ----

theme_fig1 <- theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )

theme_fig3 <- theme_fig1 + theme(legend.position = "none")


theme_fig4 <- theme_fig3
  

theme_fig5 <- theme_fig1 + 
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text = element_text(size = 10, colour = fig5_col_dark),
    axis.ticks = element_line(colour = "grey45", linewidth = 0.25),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = "black"),
    legend.position = "bottom"
  )

theme_fig6 <- theme_fig1 +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = "black"),
    legend.position = "none"
  )


theme_map <- theme_fig1 +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    panel.grid.major = element_line(colour = "gray70", linewidth = 0.2),
    axis.ticks.length = unit(0, "cm"),
    axis.text = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
  )


theme_map_fig5 <- theme_map +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    plot.margin = unit(c(0.1, 10, 0.1, 0.1), "cm")
  )

theme_map_SI <- theme_map + theme(legend.position = "right")
        
## figure parameters ----
figure_widths <- 1.5*25

## maps
# Map preparation ----
earth_box <- readRDS(
  paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL, "earth_box.rds")
) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
world_no_antarctica <- world_sf[world_sf$continent != "Antarctica", ]

labs_y <- data.frame(
  lon = -160,
  lat = c( 30, 0, -30, -60)
)

labs_y_labels <- seq(30, -60, -30)

labs_y$label <- ifelse(
  labs_y_labels == 0,
  "°",
  ifelse(labs_y_labels > 0, "°N", "°S")
)

labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)

labs_y <- st_as_sf(
  labs_y,
  coords = c("lon", "lat"),
  crs = "+proj=longlat +datum=WGS84 +no_defs"
)

labs_x <- data.frame(
  lon = seq(120, -120, -60),
  lat = -64
)

labs_x$label <- ifelse(
  labs_x$lon == 0,
  "°",
  ifelse(labs_x$lon > 0, "°E", "°W")
)

labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)

labs_x <- st_as_sf(
  labs_x,
  coords = c("lon", "lat"),
  crs = "+proj=longlat +datum=WGS84 +no_defs"
)

## extra functions ----

arrow_colourbar_vertical <- function(
    limits,
    breaks,
    labels = breaks,
    colours,
    values = NULL,          # 0–1 positions if supplied
    title = NULL,
    subtitle = NULL,
    bar_width = 0.18,
    cap_height = 0.06,
    tick_length = 0.06,
    text_size = 3,
    title_size = 3.4,
    subtitle_size = 2.8,
    ticks_colour = "black",
    frame_colour = "gray35",
    tick_linewidth = 0.35,
    frame_linewidth = 0.4,
    n = 500
) {
  
  stopifnot(length(limits) == 2)
  stopifnot(length(colours) >= 2)
  stopifnot(length(breaks) == length(labels))
  
  y_min <- limits[1]
  y_max <- limits[2]
  y_rng <- y_max - y_min
  
  if (is.null(values)) {
    values_use <- seq(0, 1, length.out = length(colours))
  } else {
    values_use <- values
  }
  
  xmin <- 1 - bar_width / 2
  xmax <- 1 + bar_width / 2
  
  pal <- scales::gradient_n_pal(
    colours = colours,
    values = values_use
  )
  
  y_seq <- seq(y_min, y_max, length.out = n + 1)
  y_mid <- (y_seq[-length(y_seq)] + y_seq[-1]) / 2
  
  # Convert y values to 0–1 positions for colour assignment
  y_pos <- (y_mid - y_min) / (y_max - y_min)
  fill_cols <- pal(y_pos)
  
  leg_df <- data.frame(
    xmin = xmin,
    xmax = xmax,
    ymin = y_seq[-length(y_seq)],
    ymax = y_seq[-1],
    fill_col = fill_cols
  )
  
  tick_df <- data.frame(
    y = as.numeric(breaks),
    lab = as.character(labels)
  )
  
  top_tri <- data.frame(
    x = c(xmin, xmax, 1),
    y = c(y_max, y_max, y_max + cap_height * y_rng)
  )
  
  bot_tri <- data.frame(
    x = c(xmin, xmax, 1),
    y = c(y_min, y_min, y_min - cap_height * y_rng)
  )
  
  x_tick_end <- xmax + tick_length
  x_text <- x_tick_end + 0.035
  
  xlim_min <- xmin - 0.06
  xlim_max <- x_text + 0.55
  
  ylim_min <- y_min - cap_height * y_rng - 0.08 * y_rng
  ylim_max <- y_max + cap_height * y_rng + 0.20 * y_rng
  
  p <- ggplot() +
    geom_rect(
      data = leg_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = fill_col,
        colour = fill_col
      ),
      linewidth = 0.15
    ) +
    scale_fill_identity() +
    scale_colour_identity() +
    geom_polygon(
      data = top_tri,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      fill = tail(colours, 1),
      color = frame_colour,
      linewidth = frame_linewidth
    ) +
    geom_polygon(
      data = bot_tri,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      fill = colours[1],
      color = frame_colour,
      linewidth = frame_linewidth
    ) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = y_min, ymax = y_max),
      inherit.aes = FALSE,
      fill = NA,
      color = frame_colour,
      linewidth = frame_linewidth
    ) +
    geom_segment(
      data = tick_df,
      aes(x = xmax, xend = x_tick_end, y = y, yend = y),
      color = ticks_colour,
      linewidth = tick_linewidth
    ) +
    geom_text(
      data = tick_df,
      aes(x = x_text, y = y, label = lab),
      hjust = 0,
      size = text_size
    ) +
    coord_cartesian(
      xlim = c(xlim_min, xlim_max),
      ylim = c(ylim_min, ylim_max),
      clip = "off"
    ) +
    theme_void()
  
  if (!is.null(title)) {
    p <- p +
      annotate(
        "text",
        x = 1,
        y = y_max + cap_height * y_rng + 0.13 * y_rng,
        label = title,
        fontface = "bold",
        size = title_size
      )
  }
  
  if (!is.null(subtitle)) {
    p <- p +
      annotate(
        "text",
        x = 1,
        y = y_max + cap_height * y_rng + 0.07 * y_rng,
        label = subtitle,
        size = subtitle_size
      )
  }
  
  p
}
