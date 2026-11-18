# Main figure entrypoints

Run from the repository root. All figures export PDF and PNG under
`PATH_SAVE_PARTITION_EVAP_FIGURES/manuscript/main/`.

| Script | Content | Existing source |
|---|---|---|
| `fig1_methods.R` | Methods for magnitude, distribution and joint agreement | `figures/figure1_methods.R` |
| `fig2_agreement_environments.R` | Individual/joint agreement maps, fingerprints and gradients | `figures/figure2_maps.R` and its fingerprint helper |
| `fig3_scale_persistence.R` | 2x2 maps at 8 degrees: two agreement patterns and two higher-agreement transitions | `figure3_b_maps_scale_transfer.R`, `figure3_c_maps_scale_transfer.R` |
| `fig4_agreement_water_balance.R` | Three median signed-error panels and four absolute-error panels; descriptive acceptance counts | `figure5_c_water_basin_comparison.R` |
| `fig5_water_balance_filtering.R` | Retained-product map and product-error heatmap with retention outlines | Plot definitions from `figure6_basin_filtering.R`; compact 11g product status |
| `fig6_basin_examples.R` | Two provisional 3x3 boards: signed and relative differences | `figure7_selected_pair_maps.R`; editable `../basin_examples.csv` |

The product-retention and basin examples form the main filtering argument.
All diagnostic overviews are in SI, following the latest allocation.
Main Figure 4 excludes the median-error map; that map is `fig4_SI_01`.
No environmental reference maps accompany the main fingerprint heatmap.

Figure 6 candidates: California, North Saskatchewan, Danube, Yellow River,
Yangtze, Ob, Parana, Niger and Swan. Both members must be water-balance
retained; ETmonitor is excluded from the candidate list. The nine-to-four
selection and explanations remain provisional.
