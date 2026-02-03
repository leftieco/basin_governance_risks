### ---------------------\\ 
# Script objective:
# On same compute cluster, merge SOM model results into a single data frame
### ---------------------\\ 

library(readr)
library(tibble)
library(purrr)

# combine the performance files
df = list.files("./som_performance_full/", pattern = ".rds", full.names = T) |> 
  map_dfr(readRDS) 

# write out
write_rds(x = df, file = "./som_FULLspace_performance_combined.rds")