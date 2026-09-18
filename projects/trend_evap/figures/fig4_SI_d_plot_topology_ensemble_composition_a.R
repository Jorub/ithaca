source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

sensitivity_test <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_oppositional_topology_sensitivity_analysis_ensemble_composition_a.rds"))
global_topo <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_topology_rank_roles.rds"))

metric_columns <- setdiff(
  names(global_topo),
  "dataset"
)

dataset_pairs <- CJ(
  dataset = global_topo$dataset,
  dataset_leftout = global_topo$dataset
)[
  dataset != dataset_leftout
]

leave_one_out_reference <- global_topo[
  dataset_pairs,
  on = "dataset"
]

setcolorder(
  leave_one_out_reference,
  c(
    "dataset",
    "dataset_leftout",
    metric_columns
  )
)

leave_one_out_reference[
  ,
  (metric_columns) := lapply(
    .SD,
    frank,
    ties.method = "average"
  ),
  by = dataset_leftout,
  .SDcols = metric_columns
]

merge <- merge(sensitivity_test, leave_one_out_reference, by = c("dataset", "dataset_leftout"))

merge[, diff_trend_opposer := trend_opposer-opposing_majority_trend]
merge[, diff_sig_opposer := sig_opposer-opposing_significance]
merge[, diff_opp_contributor := opp_contributor-opposition_contributor]


fig_trend_opposer <- ggplot(merge, aes(x = dataset, y = diff_trend_opposer)) +
  geom_hline(
    yintercept = 0,
    color = "gray55",
    linewidth = 0.5
  ) +
  geom_point(
    position = position_jitter(
      width = 0.16,
      height = 0,
      seed = 42
    ),
    size = 1.7,
    alpha = 0.5,
    color = "#414487"
  ) +
  scale_size_area(
    name = "Number of\nexperiments",
    max_size = 7,
    limits = c(1, 13),
    breaks = c(1, 4, 8, 13)
  ) +
  labs(
    y = "Rank difference\n\nstronger \u2190 0 \u2192 weaker",
    x = "Dataset"
  ) +
  theme_bw() +
  theme_standard +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid = element_blank()
  ) +
    scale_y_continuous(
      breaks = c(-1, 0, 1),
      limits = c(-1.1, 1.1)
    ) +
  ggtitle(
    "Trend opposer rank sensitivity with one dataset left out"
  )

fig_sig_opposer <- ggplot(merge, aes(y = diff_sig_opposer, x = dataset))+
  geom_hline(
    yintercept = 0,
    color = "gray55",
    linewidth = 0.5
  ) +
  geom_point(
    position = position_jitter(
      width = 0.16,
      height = 0,
      seed = 42
    ),
    size = 1.7,
    alpha = 0.5,
    color = "#414487"
  ) +
  scale_size_area(
    name = "Number of\nexperiments",
    max_size = 7,
    limits = c(1, 13),
    breaks = c(1, 4, 8, 13)
  ) +
  labs(
    y = "Rank difference\n\nstronger \u2190 0 \u2192 weaker",
    x = "Dataset"
  ) +
  theme_bw() +
  theme_standard +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(
    breaks = c(-1, 0, 1),
    limits = c(-1.1, 1.1)
  ) +
  ggtitle("Significance opposer rank sensitivity with one dataset left out")


fig_opp_contributor <- ggplot(merge, aes(y = diff_opp_contributor, x = dataset))+
  geom_hline(
    yintercept = 0,
    color = "gray55",
    linewidth = 0.5
  ) +
  geom_point(
    position = position_jitter(
      width = 0.16,
      height = 0,
      seed = 42
    ),
    size = 1.7,
    alpha = 0.5,
    color = "#414487"
  ) +
  scale_size_area(
    name = "Number of\nexperiments",
    max_size = 7,
    limits = c(1, 13),
    breaks = c(1, 4, 8, 13)
  ) +
  labs(
    y = "Rank difference\nstronger \u2190 0 \u2192 weaker",
    x = "Dataset"
  ) +
  theme_bw() +
  theme_standard +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ), 
    panel.grid = element_blank()
  ) +
  ggtitle("Opposition contributor rank sensitivity with one dataset left out")

ggarrange(fig_trend_opposer,
          fig_sig_opposer,
          fig_opp_contributor, ncol = 1,
          labels = c("a", "b", "c"))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig4_SI_global_topology_sensitivity_ensemble_composition_a.png"), 
       width = 8, height = 10)

