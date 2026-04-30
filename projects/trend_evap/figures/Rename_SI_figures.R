# Rename final SI figures ----

source('source/evap_trend.R')

rename_map <- c(
  "fig_SI_ipcc.png" = "figS01_ipcc_regions.png",
  "fig_SI_evap_quantiles.png" = "figS02_et_quantiles.png",
  "fig_SI_KG.png" = "figS03_koppen_geiger_classes.png",
  "fig_SI_elev.png" = "figS04_elevation_classes.png",
  "SI_fig_Q25.png" = "figS05_q25_trend.png",
  "SI_fig_Q75.png" = "figS06_q75_trend.png",
  "SI_fig_number_significante_trends.png" = "figS07_number_significant_trends.png",
  "SI_fig_support_evap_trend_direction_detailed_per_dataset_bootstrap.png" = "figS08_trend_direction_significance_by_dataset.png",
  "fig_SI_maps_trends_BESS.png" = "figS09_trends_BESS.png",
  "fig_SI_maps_trends_CAMELE.png" = "figS10_trends_CAMELE.png",
  "fig_SI_maps_trends_ERA5-land.png" = "figS11_trends_ERA5_land.png",
  "fig_SI_maps_trends_ETMonitor.png" = "figS12_trends_ETMonitor.png",
  "fig_SI_maps_trends_FLDAS.png" = "figS13_trends_FLDAS.png",
  "fig_SI_maps_trends_GLDAS-CLSM.png" = "figS14_trends_GLDAS_CLSM.png",
  "fig_SI_maps_trends_GLDAS-NOAH.png" = "figS15_trends_GLDAS_NOAH.png",
  "fig_SI_maps_trends_GLDAS-VIC.png" = "figS16_trends_GLDAS_VIC.png",
  "fig_SI_maps_trends_GLEAM.png" = "figS17_trends_GLEAM.png",
  "fig_SI_maps_trends_JRA-55.png" = "figS18_trends_JRA55.png",
  "fig_SI_maps_trends_MERRA-2.png" = "figS19_trends_MERRA2.png",
  "fig_SI_maps_trends_MOD16A.png" = "figS20_trends_MOD16A2.png",
  "fig_SI_maps_trends_SynthesizedET.png" = "figS21_trends_SynthesizedET.png",
  "fig_SI_maps_trends_TerraClimate.png" = "figS22_trends_TerraClimate.png",
  "fig_SI_CSI_p_val.png" = "figS23_pairwise_CSI.png",
  "fig_SI_BIAS_p_val.png" = "figS24_pairwise_BIAS.png",
  "SI_fig_quartile_uncertainty_direction.png" = "figS25_quartile_uncertainty_direction.png",
  "SI_fig_quartile_uncertainty_magnitude.png" = "figS26_quartile_uncertainty_magnitude.png",
  "fig3_quartile_uncertainty_area_fraction.png" = "figS27_quartile_uncertainty_environmental_gradients.png",
  "fig_global_topology_p_value_stability.png" = "figS28_topology_p_value_stability.png",
  "SI_fig_topology_ipcc_africa.png" = "figS29_topology_ipcc_africa.png",
  "SI_fig_topology_ipcc_asia.png" = "figS30_topology_ipcc_asia.png",
  "SI_fig_topology_ipcc_australasia.png" = "figS31_topology_ipcc_australasia.png",
  "SI_fig_topology_ipcc_europe.png" = "figS32_topology_ipcc_europe.png",
  "SI_fig_topology_ipcc_north_america.png" = "figS33_topology_ipcc_north_america.png",
  "SI_fig_topology_ipcc_south_america.png" = "figS34_topology_ipcc_south_america.png"
)

old_paths <- file.path(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, names(rename_map))
new_paths <- file.path(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, unname(rename_map))

missing_old <- old_paths[!file.exists(old_paths)]
existing_new <- new_paths[file.exists(new_paths)]

if (length(missing_old) > 0) {
  stop("Missing old files:\n", paste(missing_old, collapse = "\n"))
}

if (length(existing_new) > 0) {
  stop("New filenames already exist:\n", paste(existing_new, collapse = "\n"))
}

ok <- file.rename(old_paths, new_paths)

if (!all(ok)) {
  failed <- names(rename_map)[!ok]
  stop("Renaming failed for:\n", paste(failed, collapse = "\n"))
}

message("Successfully renamed ", length(rename_map), " SI figure files.")
