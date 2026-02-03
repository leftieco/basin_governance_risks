### ---------------------\\ 
# Script objective:
# On same compute cluster, merge SOM model results into a single data frame
### ---------------------\\ 

library(readr)
library(tibble)
library(purrr)

# combine the performance files
df = list.files("data/som_files/som1_performance_synthetic", pattern = ".rds", full.names = T) |>
  map_dfr(readRDS)

# write out
write_rds(x = df, file = "data/som_files/som1_performance_synthetic/som_performance_combined.rds")

summary_by_size <- df %>%
  dplyr::group_by(som_size) %>%
  dplyr::summarize(
    mean_quant = mean(quant, na.rm = TRUE),
    mean_topo  = mean(topo, na.rm = TRUE),
    mean_db    = mean(db_x, na.rm = TRUE),
    sd_quant   = sd(quant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(mean_quant)

print(summary_by_size)

