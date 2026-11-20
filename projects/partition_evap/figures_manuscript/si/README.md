# Supplementary figure entrypoints

Run from the repository root. Stems link each SI figure to its main figure.
PDF and PNG outputs go to `PATH_SAVE_PARTITION_EVAP_FIGURES/manuscript/si/`.

| Script | Export stems / content |
|---|---|
| `fig2_SI_01_coverage.R` | `fig2_SI_01_coverage`: available product counts |
| `fig2_SI_02_06_continuous_agreement.R` | `fig2_SI_02_mean_et`, `fig2_SI_03_iqr`, `fig2_SI_04_siqr`, `fig2_SI_05_match_ratio`, `fig2_SI_06_iqr_mean_relationship` |
| `fig2_SI_07_environment_maps.R` | `fig2_SI_07_environment_maps`: six environmental reference maps |
| `fig2_SI_08_centering.R` | `fig2_SI_08_centering`: centered versus uncentered distribution agreement |
| `fig3_SI_01_agreement_all_scales.R` | Agreement patterns at 1, 2, 4 and 8 degrees, both metrics |
| `fig3_SI_02_transitions_all_scales.R` | Higher-agreement transitions at all four scales |
| `fig3_SI_03_scale_transfer_heatmaps.R` | Four 8-degree quantitative scale-transfer heatmaps |
| `fig4_SI_01_median_water_balance_error.R` | Median-error tolerance map |
| `fig5_SI_01_filtering_change.R` | Two maps: change in local and aggregated differences, one-retained-product basins gray |
| `fig5_SI_02_retained_pair_consistency.R` | Joint-consistency map and four-class stacked bars |
| `fig5_SI_03_excluded_products.R` | Diverging 100% bars for excluded products only |
| `fig5_SI_04_compensating_pairs.R` | Proportion of retained pairs with gap >=10 pp and compensation >50% |
| `fig6_SI_01_basin_pair_heatmaps.R` | `fig6_SI_01_basin_<ID>_pair_heatmaps`: complete 2x2 per-basin series |
| `fig6_SI_02_basin_example_products.R` | Six columns (selected basins), three rows: relative difference, mean ET of product A and mean ET of product B |
| `fig6_online_selected_pair_maps.R` | Optional per-pair map series under `manuscript/online/`; excluded from normal SI runner |

The continuous agreement script builds five related panels in one pass.
The complete heatmaps preserve the current ensemble composition; ETmonitor
is excluded only from the selected example-map series until the full
sensitivity analysis is agreed.

Filtering changes use the median across per-pair summaries for both panels:
area-weighted grid-cell medians and basin-aggregated differences, respectively.
No legacy grid-comparison cache is needed.
Retained-pair proportions count unique unordered pairs with equal weight and
keep zero passes distinct from unavailable comparisons.

Exact counts, area fractions and pair membership are exported to `manuscript/data/`.
Additional regression tables are not implemented because their model and
subgroup definitions remain undecided. Older unallocated figures are retained
in their existing locations and are not automatically added to this series.
