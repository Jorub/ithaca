# SI helpfer function for SI for figure 4 ----

make_environment_order <- function(joint_evap_agreement, group_name) {
  
  fp <- joint_evap_agreement[group %in% group_name,
                             .(
                               both_higher = sum(area_fraction[
                                 rel_dataset_agreement %in% higher &
                                   dist_dataset_agreement %in% higher
                               ], na.rm = TRUE),
                               
                               both_lower = sum(area_fraction[
                                 rel_dataset_agreement %in% lower &
                                   dist_dataset_agreement %in% lower
                               ], na.rm = TRUE),
                               
                               contrast_mag_higher = sum(area_fraction[
                                 rel_dataset_agreement %in% higher &
                                   dist_dataset_agreement %in% lower
                               ], na.rm = TRUE),
                               
                               contrast_dist_higher = sum(area_fraction[
                                 rel_dataset_agreement %in% lower &
                                   dist_dataset_agreement %in% higher
                               ], na.rm = TRUE),
                               
                               area_total = sum(area_agreement, na.rm = TRUE)
                             ),
                             by = environment
  ]
  
  spatial_cols <- c(
    "both_higher",
    "both_lower",
    "contrast_mag_higher",
    "contrast_dist_higher"
  )
  
  fp[, dominant_col := spatial_cols[max.col(.SD, ties.method = "first")],
     .SDcols = spatial_cols]
  
  fp[, dominant_value := apply(.SD, 1, max, na.rm = TRUE),
     .SDcols = spatial_cols]
  
  fp[, dominant_col := factor(
    dominant_col,
    levels = c(
      "both_higher",
      "both_lower",
      "contrast_mag_higher",
      "contrast_dist_higher"
    )
  )]
  
  setorder(fp, dominant_col, -dominant_value, -area_total, environment)
  
  fp[, environment]
}
