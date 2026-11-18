# Manuscript figures

Executable main and SI entrypoints adapted from the existing plotting code.
Run from the **ithaca repository root**, with the existing project configuration
and server data available. The original plotting and analysis scripts are kept
intact. These entrypoints are now the manuscript plotting versions.

## Run

```sh
Rscript projects/partition_evap/figures_manuscript/run_figures.R main
Rscript projects/partition_evap/figures_manuscript/run_figures.R si
# Optional individual-map gallery exports:
Rscript projects/partition_evap/figures_manuscript/run_figures.R online
```

An individual main/SI script can also be sourced directly from the repository
root. No preparation or production analysis is started automatically.

Exports are isolated under `PATH_SAVE_PARTITION_EVAP_FIGURES/manuscript/`:

- `main/`: six main-figure slots; Figure 6 currently exports two provisional
  3x3 boards (signed and relative differences) for nine candidates.
- `si/`: contextual maps, scale series, basin overviews and full pair heatmaps.
- `data/`: supporting CSV/RDS summaries and the selected pair list.
- `online/`: optional individual two-panel pair maps; no website is published.

[Main entrypoints](main/README.md), [SI entrypoints](si/README.md), and
[source provenance](source_inventory.csv) document the mapping from old code.
Shared panel builders live in `source/` within this folder.

## Prepared inputs

Keep the existing preparation sequence and corrected caches:

1. Methods/global figures use the existing agreement grid, environmental
   masks and saved reference thresholds. Figure 1 uses the existing
   `figure1_timeseries.rds` and `figure1_selected_locations.rds`, with paths
   supplied by `source/partition_evap.R`, as in the original figure script.
   A failed lookup indicates that the configured path needs checking against
   the server layout; it does not establish that the data need regeneration.
2. Scale figures use corrected 13a output, KS outputs from 13d-13g, and
   magnitude/distribution classes from 13h/13i for 1, 2, 4 and 8 degrees.
3. Water-balance figures use the existing Figure 5a preparation outputs,
   annual basin product data and `distribution_agreement_index_ma_basin.rds`.
4. Filtering and pair figures use current outputs from
   `11g_basin_product_pair_comparison.R`: `basin_product_pair_summary.rds`
   (including area with symmetric relative difference >20%) and
   `basin_pair_product_status.rds`.
5. Main Figure 6 also reads the corrected global pairwise grid **once** and
   selects only the nine requested basin/pair combinations. It does not need
   11i. The optional online series reads the 11i selection first.
6. SI filtering changes use the two compact 11g summaries only. They do not
   require `basin_filtering_grid_comparison.rds` or recalculate the global grid.

Project configuration and graphics are sourced in the global environment,
matching the existing figure scripts. Shared manuscript panel functions are
sourced locally so the runner can keep figure settings separate. No manuscript
script assigns its own replacement for `PATH_SAVE_PARTITION_EVAP`.

The new main filtering figure reads compact product status only, avoiding
the old global pairwise recalculation. Existing project source files supply
paths, packages and themes; their configuration/install behaviour is inherited.

## Scientific definitions preserved

- sIQR uses the ensemble median denominator. True zero KS-match ratios remain
  valid data. Duplicate fine/coarse coordinates are rejected before plotting.
- Main Figure 3 is four maps at 8 degrees. Stored coarse classes use
  scale-specific recalculated thresholds; transition labels compare those
  classes with a >=50% area fraction of higher fine-grid agreement. Fixed
  fine-grid-reference classes are retained separately in the supporting RDS.
- Water-balance accuracy is calculated **after** taking the median of product
  basin mean estimates. Agreement retains the existing 2000-2019 period;
  water-balance errors use prepared 2000-2016 means. The existing ETmonitor
  treatment is preserved in ensemble figures pending the sensitivity rerun.
- The inclusive water-balance product filter is +/-10%. Retained-pair
  consistency requires an absolute basin/local gap <10 pp and <=20% area
  with pairwise relative difference >20%. Its four stacked groups partition
  one common assessable-pair denominator, with unavailable cases gray.
- Filtering-change SI: both panels take the median across per-pair summaries.
  The local panel uses each pair's area-weighted median grid-cell difference;
  the aggregated panel uses each pair's basin-aggregated relative difference.
  Pairs receive equal weight across the ensemble. Full and retained ensembles
  use the same order of operations. Definitions are attached to the saved RDS.
  Changes are filtered minus full, in percentage points;
  one-retained-product basins are gray.
- Excluded-product bars omit retained members from the denominator and span
  100 percentage points, below/above zero according to the sign of deviation.
  A basin with no excluded products has no artificial full-height bar.
- Exception overview uses positive local-minus-basin gap >=10 pp and C>50%.
  It is not the complement of the consistency definition.
- Main Figure 6 selection is editable in [basin_examples.csv](basin_examples.csv).
  ETmonitor is excluded from these examples and optional pair-map exports;
  JRA55 occurs only in Danube. This does not silently remove ETmonitor from
  the ensemble analyses or complete heatmaps. Signed maps preserve the
  requested A-minus-B orientation, including reversed stored pairs.

## Main versus SI

Overview maps/bars of consistency, compensation frequency, filtering changes
and excluded-product direction are in SI. Main Figure 5 contains the
product-retention map and signed water-balance-error heatmap with dark-gray
retention outlines. Main Figure 4 contains scatterplots; its median-error map
is in SI. Centering remains SI. Detailed pair heatmaps remain SI/online.

Nine basin examples are provisional, not representative of all retained pairs:
the source maps were chosen to explore exceptions. Terrain, climate,
land-cover and resolution explanations still require investigation.

## Validation and remaining decisions

```sh
Rscript projects/partition_evap/figures_manuscript/validation/check_manuscript_figures.R
```

This check parses code, checks local sources and uses synthetic fixtures for
scale transitions, pair orientation and per-pair median filtering summaries. It does not load
server data or regenerate production figures. Final rendering and layout
inspection must be done on the server after running the entrypoints.

Regression specifications and subgroup definitions for additional water-balance
statistics remain to be discussed. No regression model or retrospective
best-member ranking has been introduced. Overall median acceptance counts and
per-basin above/within/below-tolerance counts are exported with Figure 4.
The final reduction from nine examples to four and final caption wording
remain open.

AI assistance: OpenAI Codex adapted the existing human and AI-assisted plotting
code, assembled the new entrypoints and wrote validation/documentation.
The original source authorship is unchanged. No production data were run locally.
