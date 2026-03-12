library(terra)

base_dir <- "/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/IHME-DALY2021_Unsafe water, sanitation, and handwashing/DALY2021_FINAL"
in_path  <- file.path(base_dir, "World_Countries_Generalized_with_IHME_fix.shp")

# outputs (same folder)
out_shp  <- file.path(base_dir, "World_Countries_Generalized_with_IHME_norm_nat.shp")
out_csv  <- file.path(base_dir, "World_Countries_Generalized_with_IHME_norm_nat.csv")

# ---------- helpers ----------
zscore_clip <- function(x, cap = 2) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  z <- (x - m) / s
  pmin(pmax(z, -cap), cap)
}

# ---------- load ----------
v  <- terra::vect(in_path)
df <- terra::values(v, dataframe = TRUE)

if (!("IHME_DALY" %in% names(df))) stop("Missing field: IHME_DALY")

# ---------- transforms ----------
DALY_nat_log <- log1p(df$IHME_DALY)
DALY_nat_z   <- zscore_clip(DALY_nat_log)

# ---------- bind + save ----------
v_out <- cbind(v, data.frame(
  DALY_nat_log = DALY_nat_log,
  DALY_nat_z   = DALY_nat_z
))

terra::writeVector(v_out, out_shp, filetype = "ESRI Shapefile", overwrite = TRUE)

df_out <- terra::values(v_out, dataframe = TRUE)
write.csv(df_out, out_csv, row.names = FALSE)

cat("✅ Wrote SHP:", out_shp, "\n")
cat("✅ Wrote CSV:", out_csv, "\n")