# Shared export helpers; existing project configuration supplies data paths.
manuscript_paths <- function() {
  base <- file.path(PATH_SAVE_PARTITION_EVAP_FIGURES, "manuscript")
  paths <- list(main = file.path(base, "main"), si = file.path(base, "si"),
                data = file.path(base, "data"), online = file.path(base, "online"))
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  paths
}

save_manuscript_figure <- function(plot, filename, dimensions) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(paste0(filename, ".pdf"), plot, width = dimensions[1],
                  height = dimensions[2], units = "cm", device = grDevices::cairo_pdf,
                  bg = "white", limitsize = FALSE)
  ggplot2::ggsave(paste0(filename, ".png"), plot, width = dimensions[1],
                  height = dimensions[2], units = "cm", dpi = 300,
                  bg = "white", limitsize = FALSE)
}

bottom_legend <- function(plot) {
  table <- ggplot2::ggplotGrob(plot + ggplot2::theme(legend.position = "bottom"))
  index <- which(table$layout$name == "guide-box-bottom")
  if (!length(index)) index <- which(table$layout$name == "guide-box")
  stopifnot(length(index) == 1, inherits(table$grobs[[index]], "gtable"))
  table$grobs[[index]]
}
