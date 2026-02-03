library(dplyr)
library(readr)
library(here)

# 1. Load the vector layer (GeoPackage)
watersheds <- vect(here("data/input//water_res_risks_gr_ready.gpkg"))

# 2. Load the CSV file containing prototype and archetype assignments
id_lookup <- read_csv(here("data/output/water_res_risks_archetypes.csv"))

# 3. Convert the SpatVector to a DataFrame
watersheds_df <- as.data.frame(watersheds)

# 4. Merge the new attributes using HYBAS_ID
watersheds_df <- left_join(watersheds_df, id_lookup, by = "HYBAS_ID")

# 5. Check if the number of rows match
stopifnot(nrow(watersheds_df) == nrow(watersheds))

# 6. Add new columns back into the SpatVector
watersheds$prototypeID <- watersheds_df$prototypeID
watersheds$archetypeID <- watersheds_df$archetypeID

# 7. Save the updated layer to a new GeoPackage
writeVector(watersheds, here("data/output/water_res_risks_archetypes.gpkg"), overwrite = TRUE)

