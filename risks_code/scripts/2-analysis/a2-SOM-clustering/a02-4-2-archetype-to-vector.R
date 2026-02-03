library(dplyr)
library(readr)
library(here)

# 1. Load the vector layer (GeoPackage)
watersheds <- vect(here("data/input//water_res_risks_gr_ready.gpkg"))

# 2. Load the CSV file containing prototype and archetype assignments
id_lookup <- read_csv(here("data/output/water_res_archetypes.csv"))

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
writeVector(watersheds, here("data/output/water_res_risk_archetypes.gpkg"), overwrite = TRUE)


## check missing values
# Reload original vector with all HYBAS_IDs
watersheds <- terra::vect(here("data/input/allbutstorage_som_ready.gpkg"))
df <- as.data.frame(watersheds)

# Load the classified HYBAS_IDs
class_map <- readr::read_csv(here("data/output/watersheds_archetypes.csv"))

# Identify HYBAS_IDs with missing archetype assignments
missing_ids <- df$HYBAS_ID[!(df$HYBAS_ID %in% class_map$HYBAS_ID)]

length(missing_ids)   # should return 78
missing_ids |> head() # view a few of them

table(is.na(watersheds_df$archetypeID))  # how many NA vs non-NA?
table(is.na(watersheds_df$prototypeID))

nrow(watersheds_df)
nrow(watersheds)

