### ---------------------\\ 
# Script objective:
# Create a data frame that lists all SOM model grid sizes to iterate across
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

library(readr)
library(here)

input_data <- read_rds(here("data/input/01_synthetic_input_data.rds"))
n_patterns <- nrow(input_data)  # should be 

# Huggins 2024 logic
k_max <- n_patterns ^ 0.4              # 38.34
P_min <- 2 * k_max                     # 76.68
P_max <- 0.15 * n_patterns             # 1365.45
r_min = sqrt(P_min)  # 8.76
r_max = sqrt(P_max) # 36.95
no_cores = 8

r_min <- floor(sqrt(P_min)) + floor(sqrt(P_min)) %% 2  # enforce even size
r_max <- floor(sqrt(P_max)) + floor(sqrt(P_max)) %% 2

# 3. Define SOM grid sizes (even numbers only)
som_sizes <- seq(r_min, r_max, by = 2)  # 8 to 36

# 4. Repeat each size 60 times (for random initialisations)
iter_index <- expand.grid(
  size_iter = 1:60,
  size = som_sizes
) %>%
  mutate(run_id = paste0("nrc_", size, "x", size, "_iter", size_iter))

# 5. Save the iteration grid
write_rds(iter_index, here("data/som_files/som_derivation_data/00_synthetic_SOM_iteration_sizing_index.rds"))