### ---------------------\\ 
# Script objective:
# Assess performance of all SOM models trained on synthetic data and select best performing model
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# Import performance metrics of each SOM1 architecture size
df = readr::read_rds(here("data/som_files/som1_performance_synthetic/som_SYNTHspace_performance_combined.rds"))

# We are interested in clusters that (1) preserve the topography of the data, and (2) have good cluster separation
# plot the topographic error index -- and remove entries that are outliers

# Outliers are defined as being >1 MAD of within-size deviation and median-across-size deviation from median at each size
# ... this approach preferences sizes that have more reproducible performance metrics, and does not give advantage to sizes with large variances

##
### ------------------- Filter stage 1: remove outliers of individual performance metrics 
##

# initialize data frame to keep non-outlier models
df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)

global_mad_db = df |> group_by(som_size) |> summarise(mad = mad(db_x)) |> pull(mad) |> mean()
global_mad_kl = df |> group_by(som_size) |> summarise(mad = mad(k_l)) |> pull(mad) |> mean()

for (i in unique(df$som_size)) {
    # i = 10
    iter_df = df |> dplyr::filter(som_size == i)

    local_mad_db = mad(iter_df$db_x)
    local_mad_kl = mad(iter_df$k_l)

    range_allow_db = min(c(global_mad_db, local_mad_db))
    range_allow_kl = min(c(global_mad_kl, local_mad_kl))

    iter_df = iter_df |>
      dplyr::filter(db_x >= median(db_x) - range_allow_db) |>
      dplyr::filter(db_x <= median(db_x) + range_allow_db) |>
      dplyr::filter(k_l >= median(k_l)   - range_allow_kl) |>
      dplyr::filter(k_l <= median(k_l)   + range_allow_kl)

    df_keep = rbind(df_keep, iter_df) # bind non-outlier iterations
}

df_keep = df_keep[2:nrow(df_keep),]
nrow(df_keep) # 476 kept out of 1020

##
### ------------------- Filter stage 2: remove outliers of combined performance metrics 
##

# create combined performance metric - min-max scaling to range 0-1
df_keep$dbi_scaled = minmaxnorm(df_keep$db_x) 
df_keep$kl_scaled = minmaxnorm(df_keep$k_l)   

df_keep$perf = minmaxnorm(df_keep$dbi_scaled + df_keep$kl_scaled)

df_keepcomb = matrix(nrow = 1, ncol = ncol(df_keep)) |>  as.data.frame()
names(df_keepcomb) = names(df_keep)

global_mad_perf = df_keep |> group_by(som_size) |> summarise(mad = mad(perf)) |> pull(mad) |> mean()

## remove outliers of the combined performance metric
for (i in unique(df_keep$som_size)) {
  # i = 10
  iter_df = df_keep |> dplyr::filter(som_size == i)
  
  local_mad_perf = mad(iter_df$perf)
  
  range_allow_perf = min(c(global_mad_perf, local_mad_perf))
  
  iter_df = iter_df |>
    dplyr::filter(perf >= median(perf)  - range_allow_perf) |> 
    dplyr::filter(perf <= median(perf)  + range_allow_perf) 
  
  df_keepcomb = rbind(df_keepcomb, iter_df) # bind non-outlier iterations
}

df_keepcomb = df_keepcomb[2:nrow(df_keepcomb),]
nrow(df_keepcomb) # 344 kept out of 920

# select the best performing iteration as the nominated iteration to move forward with
plot(df_keepcomb$perf ~ df_keepcomb$som_size, main = "less combined outliers")

##
### ------------------- Stage 3: select best performing model 
##

# determine best-performing SOM among non-outlier iterations
rowno = which.min(df_keepcomb$perf)
winning_size = df_keepcomb[rowno,]
winning_size # this is best-performing SOM from first-stage

### We can observe that the best performing first-stage SOM is:
som_size = 22

iter_index = expand.grid(size_iter = c(1:60),
                         size = som_size)

write_rds(iter_index, file = here("data/som_files/som_derivation_data/00_full_SOM_iteration_sizing_index.rds"))


##
## -- plot
##

best_at_size = df_keepcomb |> 
  group_by(som_size) |> drop_na() |> 
  summarise(
    perf = min(perf, na.rm = T)
  )

ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  geom_point(data = df_keepcomb,     aes(x= som_size, y = perf), col = "grey30", size = 3, alpha = 0.6) +
  geom_point(data = best_at_size, aes(x= som_size, y = perf), col = "black", size = 5) +
  coord_cartesian(ylim=c(0, 1), xlim = c(9.5, 42.5), expand = 0, clip = "off") +
  scale_x_continuous(breaks = seq(4, 42, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 

ggsave(plot = last_plot(),
       filename = here("plots/SOM1_synthetic_best_size_evaluation.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)