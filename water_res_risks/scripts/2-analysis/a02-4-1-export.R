
### Link HYBAS basins → SOM1 prototypes → SOM2 archetypes

library(dplyr)
library(readr)
library(terra)
library(here)
library(kohonen)

# -----------------------
# Paths
# -----------------------
gpkg_in  <- here("data/input/water_res_risks_z_ready.gpkg")
gpkg_out <- here("data/output/water_res_risks_archetypes.gpkg")

# -----------------------
# 1) Load the GPKG (already has HYBAS_ID + 17 vars)
# -----------------------
v <- terra::vect(gpkg_in)
v_df <- as.data.frame(v)

if (!("HYBAS_ID" %in% names(v_df))) {
  stop("WARNING: HYBAS_ID not found in the GPKG attribute table.")
}

# -----------------------
# 2) Load training input + SOM models
# -----------------------
input_data <- readr::read_rds(here("data/input/02_full_input_data_norm.rds"))
som1_model <- readr::read_rds(here("data/som_files/som_files_full/som1_nrc_36_iter_4.rds"))
som2_model <- readr::read_rds(here("data/som_files/som_selection/som2_selection.rds"))

# -----------------------
# 3) Sanity checks (row alignment + prototype dimensions)
# -----------------------
if (!("HYBAS_ID" %in% names(input_data))) {
  stop("WARNING: HYBAS_ID not found in 02_full_input_data_norm.rds")
}

if (nrow(input_data) != length(som1_model$unit.classif)) {
  stop(
    "WARNING: Row mismatch: nrow(input_data) = ", nrow(input_data),
    " but length(som1_model$unit.classif) = ", length(som1_model$unit.classif),
    ". This must match to use training assignments."
  )
}

n_prototypes <- nrow(som1_model$codes[[1]])
if (n_prototypes != length(som2_model$unit.classif)) {
  stop(
    "WARNING: prototype mismatch: nrow(som1_model$codes[[1]]) = ", n_prototypes,
    " but length(som2_model$unit.classif) = ", length(som2_model$unit.classif),
    ". SOM2 must classify SOM1 prototypes 1..n."
  )
}

# -----------------------
# 4) Build HYBAS -> prototype -> archetype (from training output)
# -----------------------
hybas_to_arch <- tibble(
  HYBAS_ID    = input_data$HYBAS_ID,
  prototypeID = som1_model$unit.classif
) %>%
  left_join(
    tibble(
      prototypeID = seq_len(n_prototypes),
      archetypeID = som2_model$unit.classif
    ),
    by = "prototypeID"
  )

# -----------------------
# 5) Join onto the GPKG attributes and write out
# -----------------------
v_df2 <- v_df %>%
  left_join(hybas_to_arch, by = "HYBAS_ID")

# Push the new columns back into the SpatVector (keeps all existing cols)
v$prototypeID <- v_df2$prototypeID
v$archetypeID <- v_df2$archetypeID

# Sanity warning if any HYBAS didn't match
if (any(is.na(v$archetypeID))) {
  warning("⚠️ Some polygons have NA archetypeID after join. Check HYBAS_ID matching between GPKG and input_data.")
} else {
  cat("All HYBAS polygons mapped to prototypes + archetypes.\n")
}

dir.create(dirname(gpkg_out), recursive = TRUE, showWarnings = FALSE)
terra::writeVector(
  v, gpkg_out,
  filetype = "GPKG",
  layer = "water_res_risks_archetypes",
  overwrite = TRUE
)

cat("V Wrote:", gpkg_out, "\n")