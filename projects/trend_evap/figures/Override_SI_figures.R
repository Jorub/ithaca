# Replace selected final SI figures with newly generated versions ----

source('source/evap_trend.R')

replace_map <- c(
  "fig_4_SI_global_topology_boosters_out.png" = 
    "figS31_global_topology_boosters_out.png",
  
  "fig_5_SI_regional_clustering_dendogram.png" =
    "figS32_regional_clustering_dendogram.png",
  
  "SI_fig_topology_ipcc_africa.png" =
    "figS33_topology_ipcc_africa.png",
  
  "SI_fig_topology_ipcc_asia.png" =
    "figS34_topology_ipcc_asia.png",
  
  "SI_fig_topology_ipcc_australasia.png" =
    "figS35_topology_ipcc_australasia.png",
  
  "SI_fig_topology_ipcc_europe.png" =
    "figS36_topology_ipcc_europe.png",
  
  "SI_fig_topology_ipcc_north_america.png" =
    "figS37_topology_ipcc_north_america.png",
  
  "SI_fig_topology_ipcc_south_america.png" =
    "figS38_topology_ipcc_south_america.png"
)

source_paths <- file.path(
  PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
  names(replace_map)
)

target_paths <- file.path(
  PATH_SAVE_EVAP_TREND_FIGURES_SUPP,
  unname(replace_map)
)

# Check that all newly generated figures exist
missing_source <- source_paths[!file.exists(source_paths)]

if (length(missing_source) > 0) {
  stop(
    "Missing replacement files:\n",
    paste(missing_source, collapse = "\n")
  )
}

# Check that the final filenames already exist
missing_target <- target_paths[!file.exists(target_paths)]

if (length(missing_target) > 0) {
  stop(
    "Final SI files to replace do not exist:\n",
    paste(missing_target, collapse = "\n")
  )
}

# Replace final SI figures
ok <- file.copy(
  from = source_paths,
  to = target_paths,
  overwrite = TRUE
)

if (!all(ok)) {
  failed <- names(replace_map)[!ok]
  stop(
    "Replacing failed for:\n",
    paste(failed, collapse = "\n")
  )
}

message(
  "Successfully replaced ",
  length(replace_map),
  " final SI figure files."
)