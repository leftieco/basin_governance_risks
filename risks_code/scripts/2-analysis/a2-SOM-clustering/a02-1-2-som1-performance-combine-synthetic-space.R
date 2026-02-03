### ---------------------\\ 
# Script objective:
# On same compute cluster, merge SOM model results into a single data frame
### ---------------------\\ 

library(readr)
library(tibble)
library(purrr)

# combine the performance files
df = list.files("data/som_files/som_performance_synthetic", pattern = ".rds", full.names = T) |>
  map_dfr(readRDS)

# write out
write_rds(x = df, file = "data/som_files/som_performance_synthetic/som_performance_combined.rds")

# library(dplyr)
# library(here)
# 
# # Get all performance files
# perf_files <- list.files("som_performance_synthetic", pattern = "*.rds", full.names = TRUE)
# 
# # Read and combine
# perf_df <- perf_files |>
#   lapply(read_rds) |>
#   bind_rows()
# 
# # Optional: Save full summary for record
# write_csv(perf_df, here("data/synthetic_som1_performance_summary.csv"))


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

best_size <- 12
write_rds(best_size, here("data/best_synthetic_som_size.rds"))
