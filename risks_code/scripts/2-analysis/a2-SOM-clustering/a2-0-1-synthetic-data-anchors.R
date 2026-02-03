### ---------------------\\ 
# Script objective:
# Create a synthetic representation of input data using k-means cluster centers
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

data_stack = c(terra::rast(here("data/input-data-stack-norm.tif")))
write_rds(data_stack |> as_tibble() |> drop_na(), file = here("data/input_features_full_set_norm.rds"))

data_all = readr::read_rds(here("data/input_features_full_set_norm.rds"))
data_all = data_all |> dplyr::select(-id)
data_all[data_all > 2] = 2; data_all[data_all < -2] = -2
write_rds(data_all, file = here("data/som_files/som_derivation_data/02_full_input_data_norm.rds"))

## identify the size of kmeans cluster centers to retain 99% of variance in data
n.sample = 1.2e4

synthetic_anchors = kmeans(x = data_all, centers = n.sample, iter.max= 100)

round(synthetic_anchors$betweenss/synthetic_anchors$totss, 2) # ~99%  
n.sample/nrow(data_all) # 0.5% of data size

anchor_codes = synthetic_anchors$centers |> as_tibble()

write_rds(anchor_codes, 
          file = here("data/som_files/som_derivation_data/01_synthetic_input_data.rds"))

write_rds(synthetic_anchors, 
          file = here("data/som_files/som_derivation_data/01_synthetic_kmeans_all_data.rds"))