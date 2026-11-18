# Run manuscript figure entrypoints from the repository root ----
# Usage: Rscript projects/partition_evap/figures_manuscript/run_figures.R main
# Groups: main, si, all, online. Production data and configuration are required.
args <- commandArgs(trailingOnly = TRUE)
group <- if (length(args)) args[1] else "main"
stopifnot(group %in% c("main", "si", "all", "online"))
root <- "projects/partition_evap/figures_manuscript"
folders <- switch(group, main = "main", si = "si", all = c("main", "si"), online = "si")
scripts <- unlist(lapply(file.path(root, folders), list.files, pattern = "[.]R$", full.names = TRUE))
if (group == "online") {
  scripts <- scripts[grepl("_online_", basename(scripts))]
} else scripts <- scripts[!grepl("_online_", basename(scripts))]
for (script in sort(scripts)) {
  message("Rendering ", script)
  sys.source(script, envir = new.env(parent = globalenv()))
  invisible(gc())
}
