### ---------------------\\ 
# Script objective:
# Assess performance of all second-stage SOM models select best performing model
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# Import performance metrics of each SOM2 architecture size
df = list.files(here("data/som_files/som2_performance/"), pattern = ".rds", full.names = T) |> 
  map_dfr(readRDS) 
nrow(df)
names(df)[7] = "som_size" # ! MAKE SURE nrc is named to som_size

df$uid = df$som_size*1e4 + df$iter

# import the size differentiation function
size_diff = read_rds(here("data/som2_size_bias.rds"))
size_diff$uid = size_diff$som_size*1e4 + size_diff$iter
size_diff = size_diff |> dplyr::select(c(uid, size_bias))

df = merge(x = df, y = size_diff, by = "uid")

# calculate explained variation so that very small n aren't benefited by SOM metrics
df$unexv = 1-df$varra/100

# global MAD of individual metrics
global_mad_db = df |> group_by(som_size) |> summarise(mad = mad(db_x)) |> pull(mad) |> mean()
global_mad_kl = df |> group_by(som_size) |> summarise(mad = mad(k_l)) |> pull(mad) |> mean()
global_mad_xv = df |> group_by(som_size) |> summarise(mad = mad(unexv)) |> pull(mad) |> mean()

##
### Sensitivity analysis: evaluate how combinations of allowable MAD and incorporation of size bias affect number of groundwaterscapes
## 

sensitivity_df = expand.grid(mad_range = seq(0, 1, length.out = 21),
                             bias_weight = seq(0, 1, length.out = 21),
                             size = rep(NA), iter = rep(NA))
sensitivity_df$mad_range[seq(1, 421, by = 21)] = 0.01

for (jj in 1:nrow(sensitivity_df)) {
  
  # jj = 110
  sensitivity_df$mad_range[jj]
  sensitivity_df$bias_weight[jj]
  
  # removal outliers from this composite score, to make sure relationship between two metrics is reproducible
  df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
  names(df_keep) = names(df)
  
  for (i in unique(df$som_size)) {
    # i = 10
    iter_df = df |> dplyr::filter(som_size == i)
    
    local_mad_db = mad(iter_df$db_x)
    local_mad_kl = mad(iter_df$k_l)
    local_mad_xv = mad(iter_df$unexv)
    
    # allow a larger deviation than for the first-stage SOM (3*MAD instead of 1*MAD as working with much smaller data set with higher variability)
    range_allow_db = 3*min(c(global_mad_db, local_mad_db))
    range_allow_kl = 3*min(c(global_mad_kl, local_mad_kl))
    range_allow_xv = 3*min(c(global_mad_xv, local_mad_xv))
    
    iter_df = iter_df |>
      dplyr::filter(db_x >= median(db_x) - range_allow_db) |>
      dplyr::filter(db_x <= median(db_x) + range_allow_db) |>
      dplyr::filter(k_l >= median(k_l)   - range_allow_kl) |>
      dplyr::filter(k_l <= median(k_l)   + range_allow_kl) |>
      dplyr::filter(unexv >= median(unexv) - range_allow_xv) |>
      dplyr::filter(unexv <= median(unexv) + range_allow_xv) 
    
    df_keep = rbind(df_keep, iter_df) # bind non-outlier iterations
  }
  
  df_keep = df_keep |> drop_na()
  
  # # Now create the combined performance metrics
  df_keep$dbi_scaled = minmaxnorm(df_keep$db_x) # rescale with outliers removed
  df_keep$kl_scaled = minmaxnorm(df_keep$k_l)   # rescale with outliers removed
  df_keep$xv_scaled = minmaxnorm(df_keep$unexv)   # rescale with outliers removed
  
  # SOM performance (a 0-1 score of combined SOM metrics)
  df_keep$SOM_perf = minmaxnorm(df_keep$kl_scaled + df_keep$xv_scaled)
  
  # overall clustering performance (a 0-1 score of SOM + DBI cluster metric)
  df_keep$perf = minmaxnorm(df_keep$SOM_perf + df_keep$dbi_scaled)
  
  # now iterate through different allowable ranges of MAD and incorporation of cluster count preference
  global_mad_perf = df_keep |> group_by(som_size) |> summarise(mad = mad(perf)) |> pull(mad) |> mean()
  
  # set median absolute deviation factor to detect outliers +/- this MAD around the median per SOM size
  mad_x = sensitivity_df$mad_range[jj] 
  
  # removal outliers from this composite score, to make sure relationship between two metrics is reproducible
  df_keepSens = matrix(nrow = 1, ncol = ncol(df_keep)) |>  as.data.frame()
  names(df_keepSens) = names(df_keep)
  
  for (i in unique(df_keep$som_size)) {
    # i = 12
    
    # isolate the som grid size results
    iter_df = df_keep |> dplyr::filter(som_size == i)
    
    # calculate the size-specific MAD
    local_mad_perf = mad(iter_df$perf)
    
    # determine the allowable MAD given the current iteration of mad_x
    range_allow_perf = mad_x * min(c(global_mad_perf, local_mad_perf))
    
    # remove performance outliers
    iter_df = iter_df |>
      dplyr::filter(perf >= median(perf)  - range_allow_perf) |> 
      dplyr::filter(perf <= median(perf)  + range_allow_perf)
    
    df_keepSens = rbind(df_keepSens, iter_df) # bind non-outlier iterations
  }
  
  df_keepSens = df_keepSens |> drop_na()
  
  ##
  ### ------------------- Stage 2: incorporate size bias \\
  ##
  
  df_keepSens$perf_x_size = minmaxnorm(df_keepSens$perf + (sensitivity_df$bias_weight[jj]*df_keepSens$size_bias))
  
  rowno = which.min(df_keepSens$perf_x_size)
  winning_size = df_keepSens[rowno,]
  # winning_size
  sensitivity_df$size[jj] = winning_size$som_size
  sensitivity_df$iter[jj] = winning_size$iter
  message(jj)
  
  plot(df_keepSens$perf_x_size ~ df_keepSens$som_size, 
       main = paste0("mad ", sensitivity_df$mad_range[jj] , " bias ",  sensitivity_df$bias_weight[jj],
                     " win: ", winning_size$som_size[1]))
  
}

hist(sensitivity_df$size)

# Plot geom_tile of results 

ggplot(sensitivity_df, aes(x = mad_range, bias_weight, fill= as.factor(size))) + 
  geom_tile(width = 0.05) +
  # geom_text(aes(label=size)) +
  # geom_text(aes(label=iter)) +
  # MetBrewer::scale_fill_met_c(name = "Redon") +
  scale_fill_manual(values = c("#153962", "#5B859E", "#CEAB3A", "#B38711")) +
  coord_cartesian(expand = 0) +
  cowplot::theme_cowplot(font_size = 10) 

ggsave(plot = last_plot(),
       filename = here("plots/SOM2_sensitivity.png"),
       height = 10,
       width = 10,
       units = "cm",
       dpi = 400)

# we select som size of 15 as it best balances reproducibility, and size bias 

win_size = 15
win_iter = 41 # selected from winning models as hierarchical clusters group nicely for narrative development

file.copy(from = paste0(here("data/som_files/som2_files/som2_nclust_"),
                        win_size, "_iter_", win_iter, ".rds"),
          to = here("data/som_files/som_selections/som2_selection.rds"),
          overwrite = TRUE,
          copy.mode = TRUE)

##
## plot --
##
k_l_scaled
unexv_scaled
dbi_scaled

ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  # geom_point(data = df,     aes(x= som_size, y = sizebias_scaled), col = "grey30", size = 3, alpha = 0.6) +
  # geom_point(data = df_keep_combined,     aes(x= som_size, y = dbi_scaled), col = "grey30", size = 3, alpha = 0.6) +
  # geom_point(data = df_keep_combined,     aes(x= som_size, y = k_l_scaled), col = "grey30", size = 3, alpha = 0.6) +
  # geom_point(data = df_keep_combined,     aes(x= som_size, y = unexv_scaled), col = "grey30", size = 3, alpha = 0.6) +
  geom_point(data = df_keepSens,     aes(x= som_size, y = perf_x_size), col = "grey30", size = 3, alpha = 0.6) +
  coord_cartesian(ylim=c(0, 1), xlim = c(1.5, 30.5), expand = 0, clip = "off") +
  scale_x_continuous(breaks = seq(4, 30, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 
ggsave(plot = last_plot(),
       filename = here("plots/SOM2_element_unexv_scaled.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)