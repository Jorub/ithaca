# Figure workflow history

- 2026-11-18: Created executable main/SI entrypoints from the existing sources.
  Shared builders serve main/SI scale maps and basin-pair maps. Exports use a
  separate manuscript namespace. Diagnostic overviews are SI; nine editable
  basin-pair candidates form provisional 3x3 boards, excluding ETmonitor and
  using JRA55 only in Danube. Product-retention plotting uses compact 11g
  status rather than recalculating the global grid. Existing production scripts
  were preserved. Code validation is separate from server-data rendering.

- 2026-11-18: Confirmed median across per-pair summaries for both SI filtering-
  change panels. Removed the grid-first alternative and legacy cache dependency;
  full and retained ensembles use the same pair-first order of operations.

- 2026-11-18: Corrected the interpretation of the server file error: the user
  confirmed a different server layout, not missing data. Removed regeneration
  advice and the extra Figure 1 missing-cache guard. Project configuration is
  now sourced globally as in the original figures; shared panel functions keep
  local scope. The remaining server directory mapping requires the actual
  file locations, rather than assumptions from a failed lookup.

- 2026-11-18: Put figure-specific preparation, plotting and layout directly in
  all main and SI/online scripts. Removed ten separate figure-builder files;
  kept the shared export/legend and environmental fingerprint utilities.
  Calculations, input/output names, periods and thresholds are preserved.
  Server listing confirms both Figure 1 inputs in the configured directory.
  Set explicit four-row layouts for the two all-scale SI map series so each
  eight-panel figure remains on one page. Offline checks use synthetic inputs;
  production data and figure generation were not run locally.
