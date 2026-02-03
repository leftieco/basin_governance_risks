
# =========================================================
# Link HYBAS basins → SOM1 prototypes → SOM2 archetypes
# =========================================================


# =========================================================
# HYBAS → SOM1 prototype → SOM2 archetype mapping
# =========================================================
library(dplyr)
library(readr)
library(terra)
library(here)
library(kohonen)

# ---------------------------------------------------------
# 1. Load SOM models
# ---------------------------------------------------------
som1_model <- read_rds(here("data/som_files/som_files_full/som1_nrc_36_iter_29.rds"))
som2_model <- read_rds(here("data/som_files/som_selection/som2_selection.rds"))

# ---------------------------------------------------------
# 2. Load watershed attributes (same data structure as used for SOM1 training)
# ---------------------------------------------------------
watersheds <- terra::vect(here("data/input/water_res_risks_gr_ready.gpkg"))
df <- as.data.frame(watersheds)

# ---------------------------------------------------------
# 3. Extract variables used in SOM1 training
# ---------------------------------------------------------
som_vars <- colnames(som1_model$data[[1]])   # exactly the 17 training variables

# Check presence
missing_vars <- setdiff(som_vars, names(df))
if (length(missing_vars) > 0) {
  stop("❌ These SOM training variables are missing in your data: ",
       paste(missing_vars, collapse = ", "))
}

# ---------------------------------------------------------
# 4. Prepare the SOM1 input matrix
# ---------------------------------------------------------
input_mat <- df[, som_vars, drop = FALSE]

# Ensure all numeric (handles factors/characters)
input_mat <- data.matrix(input_mat)

# Keep only complete rows
complete_idx <- complete.cases(input_mat)
input_mat <- input_mat[complete_idx, , drop = FALSE]

# Sanity check
cat("✅ Input matrix ready with", nrow(input_mat), "rows and", ncol(input_mat), "variables\n")

# ---------------------------------------------------------
# 5. Map each HYBAS polygon onto SOM1 → get prototype IDs
# ---------------------------------------------------------
# SOM1 was trained on an unnamed list ([[1]]), so use same format:
som_pred <- predict(som1_model, newdata = list(input_mat))

# Extract prototype assignments
som1_unit_classif <- som_pred$unit.classif

cell_to_prot <- tibble(
  HYBAS_ID = df$HYBAS_ID[complete_idx],
  prototypeID = som1_unit_classif
)

# ---------------------------------------------------------
# 6. Map SOM1 prototypes → SOM2 archetypes
# ---------------------------------------------------------
prot_to_arch <- tibble(
  prototypeID = seq_along(som2_model$unit.classif),
  archetypeID = som2_model$unit.classif
)
  
  # ---------------------------------------------------------
  # 7. Merge both mappings: HYBAS → prototype → archetype
  # ---------------------------------------------------------
  hybas_to_arch <- left_join(cell_to_prot, prot_to_arch, by = "prototypeID")
  
  # ---------------------------------------------------------
  # 8. Export final mapping table
  # ---------------------------------------------------------
  out_path <- here("data/output/water_res_risks_archetypes.csv")
  write_csv(hybas_to_arch, out_path)
  cat("✅ Saved:", out_path, "\n")
  
  # ---------------------------------------------------------
  # 9. Optional summary
  # ---------------------------------------------------------
  summary_tbl <- hybas_to_arch %>%
    summarise(
      n_HYBAS = n(),
      n_prototypes = n_distinct(prototypeID),
      n_archetypes = n_distinct(archetypeID)
    )
  print(summary_tbl)
  
  # Sanity check for missing mappings
  if (any(is.na(hybas_to_arch$archetypeID))) {
    warning("⚠️ Some HYBAS polygons have no archetype assignment.")
  } else {
    cat("🎯 All HYBAS polygons successfully mapped to archetypes.\n")
  }                          

  
  
