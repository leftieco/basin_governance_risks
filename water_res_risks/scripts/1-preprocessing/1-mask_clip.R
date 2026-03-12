library(sf)
library(dplyr)
library(terra)
library(rnaturalearth)
library(here)

# --- Load and ensure EPSG:4326 ---
v <- vect(here("data/input/water_res_risks_noNA_270226.gpkg"))

if (!grepl("EPSG:4326", crs(v, proj=TRUE))) v <- project(v, "EPSG:4326")

# --- Mask 1: remove areas south of 60°S (still in 4326) ---
# Use a rectangular extent crop to avoid vertex-based filtering
v <- crop(v, ext(-180, 180, -60, 90))

# --- Mask 2: remove Greenland with a geodesic 10 km buffer (still in 4326) ---
# Use 1:50m scale to avoid needing rnaturalearthhires
world <- ne_countries(scale = 50, returnclass = "sf")
greenland <- world |> dplyr::filter(admin == "Greenland") |> st_make_valid() |> st_transform(4326)

# Geodesic buffer in meters (requires s2, which is on by default for sf)
sf_use_s2(TRUE)
gr_buf <- st_buffer(greenland, dist = 10000)  # 10 km on the sphere

# Erase buffered Greenland from your terra vector
v <- erase(v, vect(gr_buf))


writeVector(v, out_file, overwrite = TRUE)

# Rename columns
names(v)[names(v) == "runoff_nonstatmean"] <- "runoff_nonstat"
names(v)[names(v) == "bod_mean"] <- "bod"
names(v)[names(v) == "gender_ii_mean"] <- "gender_ineq"
names(v)[names(v) == "species_max"] <- "thrspecies"
names(v)[names(v) == "dewetland_mean"] <- "wetlandloss"
names(v)[names(v) == "tws_nonstat_mean"] <- "tws_nonstat"
names(v)[names(v) == "IHME_DALY_mean"] <- "DALY"
names(v)[names(v) == "corruption_mean"] <- "corruption"
names(v)[names(v) == "fragile_st_mean"] <- "frag_statei"
names(v)[names(v) == "wwthdr_pc_mean"] <- "wwthdr_pc"
names(v)[names(v) == "CSI_mean"] <- "connectivity_s_i"
names(v)[names(v) == "value_mean"] <- "e_flow"
names(v)[names(v) == "bws_raw_mean"] <- "base_water_stress"
names(v)[names(v) == "hydropol_max"] <- "hydropol_int"
names(v)[names(v) == "gini_mean"] <- "gini"


writeVector(v, here("data/input/water_res_risks_masked_renamed.gpkg"), overwrite = TRUE)

### Extra columns added: mismplastic, droughthaz, extremes_damage, extremes_affected
