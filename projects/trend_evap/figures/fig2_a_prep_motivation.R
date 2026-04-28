# Figure 2 ---- 
## prep the global overview ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Variation in global trends ----
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  
evap_annual_trend[p > 0.1 , trend_significance := "\u2264 1"]
evap_annual_trend[p <= 0.1 , trend_significance := "\u2264 0.1"]
evap_annual_trend[p <= 0.05 , trend_significance := "\u2264 0.05"]
evap_annual_trend[p <= 0.01 , trend_significance := "\u2264 0.01"]

### Table of range of trends ----
evap_annual_trend_fig_2 <- evap_annual_trend[, .(dataset, slope, trend_significance, lower, upper)][order(-slope),]

evap_annual_trend_fig_2[, dataset := toupper(dataset)]
evap_annual_trend_fig_2[dataset == "ETMONITOR", dataset := "ETMonitor"]
evap_annual_trend_fig_2[dataset == "SYNTHESIZEDET", dataset := "SynthesizedET"]
evap_annual_trend_fig_2[dataset == "ERA5-LAND", dataset := "ERA5-land"]
evap_annual_trend_fig_2[dataset == "MERRA2", dataset := "MERRA-2"]
evap_annual_trend_fig_2[dataset == "JRA55", dataset := "JRA-55"]
evap_annual_trend_fig_2[dataset == "TERRACLIMATE", dataset := "TerraClimate"]


### Save evap data ----
saveRDS(evap_annual_trend_fig_2, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_global_evap_trend.rds"))
write.csv(evap_annual_trend_fig_2, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_global_evap_trend.csv"))


Q25_global <- evap_annual_trend[, quantile(slope, 0.25)]
Q75_global <- evap_annual_trend[, quantile(slope, 0.75)]
global_fold <- round(Q75_global/Q25_global, digit = 1)

mad_global <- evap_annual_trend[, mad(slope)]
mad_std_global <- mad_global/evap_annual_trend[, median(slope)]

Q25_global_abs <- evap_annual_trend[, quantile(abs(slope), 0.25)]
Q75_global_abs <- evap_annual_trend[, quantile(abs(slope), 0.75)]
global_fold_abs <- round(Q75_global_abs/Q25_global_abs, digit = 1)

## Quartile fold ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend_min_max <- evap_trend[dataset_count >= 12,.(max = max(slope), min = min(slope),
                                                       Q75 = quantile(slope, 0.75), Q25 = quantile(slope, 0.25),
                                                       Q75_abs = quantile(abs(slope), 0.75), Q25_abs = quantile(abs(slope), 0.25),
                                                       MAD = mad(slope),
                                                       med_slope = median(slope)), .(lat,lon)]


evap_trend_min_max[abs(Q25) > Q75, fold := abs(Q25)/abs(Q75)]
evap_trend_min_max[abs(Q25) <= Q75, fold := abs(Q75)/abs(Q25)]
evap_trend_min_max[, fold_brk := cut(fold, breaks = c(1, global_fold, Inf))]
evap_trend_min_max[, fold_brk_detailed := cut(fold, breaks = c(1, global_fold, 5, 10, 20, Inf))]

evap_trend_min_max[, fold_abs := Q75_abs/Q25_abs]
evap_trend_min_max[, fold_abs_brk := cut(fold_abs, breaks = c(1, global_fold, Inf))]
evap_trend_min_max[, fold_abs_brk_detailed := cut(fold_abs, breaks = c(1, 2, global_fold, 4, 5, 6, 7, 8, 9, 10, 20, Inf))]

evap_trend_min_max[, med_slope_win := max(abs(med_slope), 1e-4), .(lon, lat)]
evap_trend_min_max[, mad_std := MAD/med_slope_win]
evap_trend_min_max[, mad_brk := cut(mad_std, breaks = c(0, mad_std_global, Inf))]
evap_trend_min_max[, mad_brk_detailed := cut(mad_std, breaks = c(0, mad_std_global, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, Inf))]

## Quartile sign disagreement ----
evap_trend_min_max[Q75/Q25 >= 0 , sign := "same" ]
evap_trend_min_max[Q75/Q25 < 0 , sign := "different"]
evap_trend_min_max[, sign := factor(sign, 
                                    levels = c("same", "different"))]

evap_trend_min_max[fold_brk == "(1,3.3]" & sign == "same", problem := "None"] 
evap_trend_min_max[fold_brk == "(1,3.3]" & sign == "different", problem := "Direction"] 
evap_trend_min_max[fold_brk == "(3.3,Inf]" & sign == "same", problem := "Magnitude"] 
evap_trend_min_max[fold_brk == "(3.3,Inf]" & sign == "different", problem := "Both"] 
evap_trend_min_max[Q75 >= 0, direction := "positive"] 
evap_trend_min_max[Q75 < 0, direction := "negative"] 

evap_sel <- subset(evap_trend_min_max, select = c("lon", "lat", "min", "max", "Q25", "Q75" ,"fold_brk", "fold_brk_detailed", "sign",
                                                  "fold_abs_brk", "fold_abs_brk_detailed", "mad_brk", "mad_brk_detailed", "problem"))

saveRDS(evap_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_quartile_stats.rds"))
write.csv(evap_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_quartile_stats.csv"))

### Area fraction ----
#### Symmetric fold ----
grid_cell_area <- unique(evap_trend_min_max[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_min_max <- grid_cell_area[evap_trend_min_max, on = .(lon, lat)]
total_area <- evap_trend_min_max[, sum(area)]
fold_stats <- evap_trend_min_max[, .(area_fraction = sum(area)/total_area), .(fold_brk_detailed)]
saveRDS(fold_stats, paste0(PATH_SAVE_EVAP_TREND_TABLES, "summary_table_quartile_stats.rds"))

#### Quartile uncertainty and direction ----
problem_stats <- evap_trend_min_max[, .(area_fraction = sum(area)/total_area), .(problem)]
problem_direction_stats <- evap_trend_min_max[, .(area_fraction = sum(area)/total_area), .(problem, direction)]

problem_merge <- merge(problem_stats, problem_direction_stats, by = "problem")
problem_merge[, area_fraction_dir := area_fraction.y/area_fraction.x]
saveRDS(problem_merge, paste0(PATH_SAVE_EVAP_TREND_TABLES, "summary_table_uncertainty_stats.rds"))

## Opposing data ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

# Maps ---
evap_index[p_val_opposing == "1", p_val_opposing := "None"]
evap_index[p_val_opposing == ">0.2", p_val_opposing := "\u2264 1"]
evap_index[p_val_opposing == "<=0.01", p_val_opposing := "\u2264 0.01"]
evap_index[p_val_opposing == "<=0.05", p_val_opposing := "\u2264 0.05"]
evap_index[p_val_opposing == "<=0.1", p_val_opposing := "\u2264 0.1"]
evap_index[p_val_opposing == "<=0.2", p_val_opposing := "\u2264 1"]

evap_index[, p_val_opposing := as.factor(p_val_opposing)]

evap_index[, p_val_opposing := factor(p_val_opposing, levels = 
                                        c("None", "\u2264 0.01", "\u2264 0.05", 
                                          "\u2264 0.1", "\u2264 1"))]

evap_index[N_none_0_2 >= 7, more_sig_trends := "\u2264 1" ]
evap_index[N_none_0_2 < 7, more_sig_trends := "\u2264 1" ]
evap_index[N_none_0_1 < 7, more_sig_trends := "\u2264 0.1" ]
evap_index[N_none_0_05 < 7, more_sig_trends := "\u2264 0.05" ]
evap_index[N_none_0_01 < 7, more_sig_trends := "\u2264 0.01" ]
evap_index[, more_sig_trends := as.factor(more_sig_trends) ]
evap_index[, more_sig_trends := factor(more_sig_trends, levels = 
                                         c("\u2264 0.01", "\u2264 0.05", 
                                           "\u2264 0.1", "\u2264 1"))]

evap_index[, DCI_all_brk := cut(DCI_all, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
evap_index[DCI_all_brk == "(-1.01,-0.5]", DCI_all_brk := "[-1,-0.5]"]
evap_index[, DCI_all_brk:= factor(DCI_all_brk, levels = c("[-1,-0.5]", 
                                                          "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                          "(0.5,1]"))]


## Save subset of grid data ----
evap_index_sel <- subset(evap_index, select = c("lon", "lat", "DCI_all_brk", "more_sig_trends", "p_val_opposing"))
saveRDS(evap_index_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_trend_stats.rds"))
write.csv(evap_index_sel, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_grid_trend_stats.csv"))


## Area fractions ----
grid_cell_area <- unique(evap_index[, .(lon, lat)]) %>% grid_area() # m2

data_sel <- subset(evap_index, select = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all", "lon", "lat"))
setnames(data_sel, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_melt <- melt(data_sel, 
                      measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"), 
                      id.vars = c("lon", "lat"))

data_sel_melt[, DCI_brk := cut(value, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable)]
data_dci_area[, total_area := sum(DCI_area), variable]
data_dci_area[, fraction := DCI_area/total_area]
data_dci_area[DCI_brk == "(-1.01,-0.5]", DCI_brk := "[-1,-0.5]"]
data_dci_area[, DCI_brk:= factor(DCI_brk, levels = c("[-1,-0.5]", 
                                                     "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                     "(0.5,1]"))]

saveRDS(data_dci_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_area_stats_DCI_all_trends.rds"))


data_sel_trend <- subset(evap_index, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
data_sel_trend  <- grid_cell_area[data_sel_trend, on = .(lon, lat)]

setnames(data_sel_trend, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_trend_melt <- melt(data_sel_trend, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable)] 
data_sel_trend_area[, total_area := sum(trend_area), variable]
data_sel_trend_area[, fraction := trend_area/total_area]

saveRDS(data_sel_trend_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_area_stats_trend_direction.rds"))

evap_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
evap_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
evap_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
evap_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
evap_index[, N_sum_all:= N_pos_all+N_neg_all]

evap_sel <- subset(evap_index, select = c("N_sum_0_01", "N_sum_0_05", 
                                          "N_sum_0_1", "N_sum_0_2", "N_sum_all",
                                          "lat", "lon"))
evap_sel  <- grid_cell_area[evap_sel, on = .(lon, lat)]

setnames(evap_sel , old = c("N_sum_0_01", "N_sum_0_05", 
                            "N_sum_0_1", "N_sum_0_2", "N_sum_all"), 
         new = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_melt <- melt(evap_sel, measure.vars = c("p <= 0.01", " p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_melt[, N_sum_brk := cut(round(value), c(-0.1, 0.9, 4, 7, 11, 14))]
data_melt[N_sum_brk == "(-0.1,0.9]", N_sum_brk := "[0,1)"]
data_melt[N_sum_brk == "(0.9,4]", N_sum_brk := "[1,4]"]

N_sig_area <- data_melt[,.(N_sig_area = sum(area)), .(N_sum_brk, variable)]
N_sig_area[, variable_area := sum(N_sig_area), variable]
N_sig_area[, fraction := N_sig_area/variable_area]
N_sig_area[, N_sum_brk:= factor(N_sum_brk, levels = c("[0,1)", "[1,4]",
                                                      "(4,7]", 
                                                      "(7,11]","(11,14]"))]

saveRDS(N_sig_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_area_stats_significant_trend_count.rds"))
