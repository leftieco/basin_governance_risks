# --- Setup ----
library(terra)

base_dir <- "/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/vector_data_merge"

# ---------- load ----------
in_path  <- file.path(base_dir, "vector_CPI_FSI_GII_aquastat.gpkg")
out_path <- file.path(base_dir, "vector_CPI_FSI_GII_aquastat_nat_norm.gpkg")

v  <- terra::vect(in_path)
df <- terra::values(v, dataframe = TRUE)

# ---------- helper ----------
zscore_clip <- function(x, cap = 2) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  z <- (x - m) / s
  pmin(pmax(z, -cap), cap)
}

# --- make a safe ISO join key (protect Namibia "NA") ---

df$iso_key <- as.character(df$iso)

# Use whichever name field exists in your file
name_field <- if ("country" %in% names(df)) "country" else if ("countryaff" %in% names(df)) "countryaff" else NA
if (is.na(name_field)) stop("No country name field found (expected 'country' or 'countryaff').")

is_namibia <- df[[name_field]] == "Namibia"

# Restore ISO code for Namibia only
df$iso_key[is_namibia & (is.na(df$iso_key) | df$iso_key == "")] <- "NA"

# ---------- country-level (ISO) means, then zscore+clip across countries ----------
country_means <- aggregate(
  df[, c("gender_ii", "wwthdr_pc", "corruption", "fragile_st")],
  by = list(iso = df$iso_key),
  FUN = mean, na.rm = TRUE
)

m <- match(df$iso_key, country_means$iso)

country_means$gender_ineq_nat_z <- zscore_clip(country_means$gender_ii)
country_means$wwthdr_pc_nat_z   <- zscore_clip(country_means$wwthdr_pc)
country_means$corruption_nat_z  <- zscore_clip(country_means$corruption)
country_means$frag_statei_nat_z <- zscore_clip(country_means$fragile_st)



table(df$iso_key, useNA = "ifany")
country_means[country_means$iso == "NA", ]

# ---------- bind + write ----------
v_out <- cbind(v, data.frame(
  frag_statei_nat_z = country_means$frag_statei_nat_z[m],
  corruption_nat_z  = country_means$corruption_nat_z[m],
  wwthdr_pc_nat_z   = country_means$wwthdr_pc_nat_z[m],
  gender_ineq_nat_z = country_means$gender_ineq_nat_z[m]
))

terra::writeVector(v_out, out_path,
                   filetype = "GPKG",
                   layer = "vector_CPI_FSI_GII_aquastat_nat_norm",
                   overwrite = TRUE)

cat("✅ Wrote:", out_path, "\n")

df_out <- terra::values(v_out, dataframe = TRUE)
csv_path <- file.path(base_dir, "vector_CPI_FSI_GII_aquastat_nat_norm.csv")
write.csv(df_out, csv_path, row.names = FALSE)
cat("✅ Wrote:", csv_path, "\n")