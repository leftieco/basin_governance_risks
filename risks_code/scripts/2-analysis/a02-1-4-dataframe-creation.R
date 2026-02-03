library(terra)
library(dplyr)
library(here)
library(readr)

# 1. Load your final GPKG
v <- vect(here("data/input/water_res_risks_gr_ready.gpkg"))

# 2. Convert to data.frame (drop geometry)
df <- as.data.frame(v)

# --- Select only *_gr normalized columns for SOM ---
cols_gr   <- grep("_gr$", names(df), value = TRUE)

# som_input_df <- df[, cols_gr, drop = FALSE]

som_input_df <- df[, c("HYBAS_ID", cols_gr), drop = FALSE]

# 4. Drop any rows with NAs (should be none at this point, but double-check)
som_input_df <- som_input_df %>% drop_na()

# 5. Save as .rds file
write_rds(som_input_df, here("data/input/02_full_input_data_norm.rds"))
