source('source/evap_trend.R')
source('source/evap_trend_graphics.R')

sensitivity_test <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_oppositional_topology_sensitivity_analysis_ensemble_composition_a.rds"))


fig_trend_opposer <- ggplot(sensitivity_test)+
  geom_boxplot(aes(y = trend_opposer, x = dataset))+
  labs( y = "Rank\nStrongest \u2192 Weakest", x = "Dataset")+
  theme_bw()+
  theme_standard +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ggtitle("Trend opposer rank sensitivity with one dataset leftout")


fig_sig_opposer <- ggplot(sensitivity_test)+
  geom_boxplot(aes(y = sig_opposer, x = dataset))+
  labs( y = "Rank\nStrongest \u2192 Weakest", x = "Dataset")+
  theme_bw()+
  theme_standard +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ggtitle("Significance opposer rank sensitivity with one dataset leftout")


fig_opp_contributor <- ggplot(sensitivity_test)+
  geom_boxplot(aes(y = opp_contributor, x = dataset))+
  labs( y = "Rank\nStrongest \u2192 Weakest", x = "Dataset")+
  theme_bw()+
  theme_standard +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ggtitle("Opposition contributor rank sensitivity with one dataset leftout")

ggarrange(fig_trend_opposer,
          fig_sig_opposer,
          fig_opp_contributor, ncol = 1,
          labels = c("a", "b", "c"))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig4_SI_global_topology_sensitivity_ensemble_compositon_a.png"), 
       width = 8, height = 10)

