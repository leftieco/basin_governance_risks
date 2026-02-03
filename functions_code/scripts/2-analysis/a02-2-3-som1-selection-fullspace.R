library(here); source(here(("scripts/on_button.R")))

### ---------------------\\ 
# Script objective:
# Assess performance of all SOM models trained on full set of input data and select best performing model
### ---------------------\\ 

# Import performance metrics of each SOM1 architecture size
df = readr::read_rds(here("data/som_files/som_performance_full/som_FULLspace_performance_combined.rds"))

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
nrow(df_keep) # 32 kept out of 60

##
### ------------------- Filter stage 2: remove outliers of combined performance metrics
##

# create combined performance metric using min-max scaled metrics (so consistent range)
df_keep$dbi_scaled = minmaxnorm(df_keep$db_x)
df_keep$kl_scaled = minmaxnorm(df_keep$k_l)
df_keep$perf = (df_keep$dbi_scaled + df_keep$kl_scaled)/2

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
nrow(df_keepcomb) # 24 kept out of 60


##
### ------------------- Stage 3: select best performing model
##

plot(df_keepcomb$perf ~ df_keepcomb$som_size, main = "less combined outliers")

# extract best-performing SOM among non-outlier iterations
rowno = which.min(df_keepcomb$perf)
winning_size = df_keepcomb[rowno,]
winning_size # this is best-performing SOM from first-stage

### We can observe that the best performing first-stage SOM is:
som_size = 38
som_iter = 56 
#(w/ CRYO 11.2025 35)
#(w/o CRYO 32)
#(w/o CRYO 2.2.2026 56)