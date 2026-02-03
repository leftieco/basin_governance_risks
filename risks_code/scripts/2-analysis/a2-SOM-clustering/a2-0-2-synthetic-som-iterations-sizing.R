### ---------------------\\ 
# Script objective:
# Create a data frame that lists all SOM model grid sizes to iterate across
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

input_data = read_rds(here("data/som_files/som_derivation_data/01_synthetic_input_data.rds"))
n_patterns = input_data |> nrow() #12,000
k_max = n_patterns ^ (0.4)  # 39.81
P_min = 2*k_max # 79.62
P_max = 0.15 * n_patterns # 1500
r_min = sqrt(P_min)  # 8.92
r_max = sqrt(P_max) # 38.72
no_cores = 8

n_patterns
r_min
r_max

# overwrite r_min and r_max parameters based on justification provided in SI 
r_min = floor(r_min) + floor(r_min) %% 2
r_max = floor(r_max) + floor(r_max) %% 2

# create vector of all SOM size parameters to try:
som_sizes = seq(r_min, r_max,by=2)

iter_index = expand.grid(size_iter = c(1:60),
                         size = som_sizes)

write_rds(iter_index, file = here("data/som_files/som_derivation_data/00_synthetic_SOM_iteration_sizing_index.rds"))