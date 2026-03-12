#Index creation using PCA — COUNTRIES

library(terra)
library(dplyr)
library(here)

library(corrplot)
library(ggplot2)
library(patchwork)

library(FactoMineR)
library(factoextra)

# --------------------------
# helper 
# --------------------------
zscore_clip <- function(x, cap = 2) {
  m  <- mean(x, na.rm = TRUE)
  sd <- stats::sd(x, na.rm = TRUE)
  if (is.na(sd) || sd == 0) return(rep(0, length(x)))
  z  <- (x - m) / sd
  pmin(pmax(z, -cap), cap)
}

# --------------------------
# 1) Load normalized COUNTRIES layer (CPI/FSI/GII/Aquastat nat z)
# --------------------------
v <- terra::vect("/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/vector_data_merge/vector_CPI_FSI_GII_aquastat_nat_norm.gpkg")
df_attr <- terra::values(v, dataframe = TRUE)

# Robust join key chooser (handles your aff_iso/AFF_ISO/iso/ISO)
pick_iso <- function(df) {
  for (k in c("AFF_ISO", "aff_iso", "ISO", "iso", "iso_key")) {
    if (k %in% names(df)) return(as.character(df[[k]]))
  }
  stop("No ISO field found (tried AFF_ISO, aff_iso, ISO, iso, iso_key).")
}

df_attr$iso_join <- pick_iso(df_attr)

#Index creation using PCA — COUNTRIES

library(terra)
library(dplyr)
library(here)

library(corrplot)
library(ggplot2)
library(patchwork)

library(FactoMineR)
library(factoextra)

# --------------------------
# helper (same as yours)
# --------------------------
zscore_clip <- function(x, cap = 2) {
  m  <- mean(x, na.rm = TRUE)
  sd <- stats::sd(x, na.rm = TRUE)
  if (is.na(sd) || sd == 0) return(rep(0, length(x)))
  z  <- (x - m) / sd
  pmin(pmax(z, -cap), cap)
}

# --------------------------
# 1) Load normalized COUNTRIES layer (CPI/FSI/GII/Aquastat nat z)
# --------------------------
v <- terra::vect("/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/vector_data_merge/vector_CPI_FSI_GII_aquastat_nat_norm.gpkg")
df_attr <- terra::values(v, dataframe = TRUE)

# Robust join key chooser (handles your aff_iso/AFF_ISO/iso/ISO)
pick_iso <- function(df) {
  for (k in c("AFF_ISO", "aff_iso", "ISO", "iso", "iso_key")) {
    if (k %in% names(df)) return(as.character(df[[k]]))
  }
  stop("No ISO field found (tried AFF_ISO, aff_iso, ISO, iso, iso_key).")
}

df_attr$iso_join <- pick_iso(df_attr)

# --------------------------
# 1b) Ensure DALY_nat_z exists; if not, join it in from IHME file
# --------------------------
if (!("DALY_nat_z" %in% names(df_attr))) {
  
  v_daly  <- terra::vect("/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/IHME-DALY2021_Unsafe water, sanitation, and handwashing/DALY2021_FINAL/World_Countries_Generalized_with_IHME_norm_nat.shp")
  df_daly <- terra::values(v_daly, dataframe = TRUE)
  df_daly$iso_join <- pick_iso(df_daly)
  
  # SAFE join that preserves df_attr row order
  m <- match(df_attr$iso_join, df_daly$iso_join)
  df_attr$DALY_nat_z <- df_daly$DALY_nat_z[m]
  
  # write back to vector attributes (same row count as v)
  terra::values(v) <- df_attr
}

# Quick NA diagnostics for the national PCA inputs
vars_check <- c("DALY_nat_z", "gender_ineq_nat_z", "frag_statei_nat_z", "corruption_nat_z")
missing_vars <- setdiff(vars_check, names(df_attr))
if (length(missing_vars) > 0) stop("Missing expected fields: ", paste(missing_vars, collapse = ", "))

na_counts <- colSums(is.na(df_attr[, vars_check, drop = FALSE]))
print(na_counts)

# Rows (countries) with any NA in those inputs
na_rows <- df_attr[!complete.cases(df_attr[, vars_check, drop = FALSE]), , drop = FALSE]

cat("\nCountries with NA in PCA inputs:", nrow(na_rows), "\n")

# Write them out (CSV) with a readable ID column if available
id_col <- if ("country" %in% names(df_attr)) "country" else
  if ("COUNTRY" %in% names(df_attr)) "COUNTRY" else
    if ("countryaff" %in% names(df_attr)) "countryaff" else
      NA

out_na_csv <- here("data/diagnostics/countries_with_NA_in_nat_inputs.csv")
dir.create(dirname(out_na_csv), recursive = TRUE, showWarnings = FALSE)

if (!is.na(id_col)) {
  na_out <- na_rows[, c(id_col, "iso_join", vars_check), drop = FALSE]
} else {
  na_out <- na_rows[, c("iso_join", vars_check), drop = FALSE]
}

write.csv(na_out, out_na_csv, row.names = FALSE)
cat("✅ Wrote NA rows to:", out_na_csv, "\n")

# Fix df_attr: set Namibia DALY_nat_z manually (no diagnostic file touched)

idx_nm <- which(df_attr$country == "Namibia")
if (length(idx_nm) != 1) stop("Expected exactly 1 Namibia row; found: ", length(idx_nm))

df_attr$DALY_nat_z[idx_nm] <- 1.468320762173018
df_attr$iso_join[idx_nm] <- "NA"
terra::values(v) <- df_attr


# quick check
df_attr[idx_nm, c("country", "iso_join", "DALY_nat_z")]

# write back onto the vector (so downstream cbind/write uses the patched values)
terra::values(v) <- df_attr


# --------------------------
# 2) Equity index (DALY_nat_z and gender_ineq_nat_z)
# --------------------------
X_eq  <- df_attr[, c("DALY_nat_z", "gender_ineq_nat_z")]
cc_eq <- complete.cases(X_eq)

pca_eq <- prcomp(X_eq[cc_eq, ], center = TRUE, scale. = FALSE)
equity_risk_index <- rep(NA_real_, nrow(df_attr))
equity_risk_index[cc_eq] <- as.numeric(pca_eq$x[, 1])

equity_risk_index_z <- zscore_clip(equity_risk_index, cap = 2)

# --------------------------
# 3) Governance risk index (frag_statei_nat_z + corruption_nat_z)
# --------------------------
X_gov  <- df_attr[, c("frag_statei_nat_z", "corruption_nat_z")]
cc_gov <- complete.cases(X_gov)

pca_gov <- prcomp(X_gov[cc_gov, ], center = TRUE, scale. = FALSE)
gov_risk_index <- rep(NA_real_, nrow(df_attr))
gov_risk_index[cc_gov] <- as.numeric(pca_gov$x[, 1])

# orient sign: higher gov_risk_index = higher frag_statei_nat_z (more fragile)
if (cor(gov_risk_index[cc_gov], df_attr$frag_statei_nat_z[cc_gov], use = "complete.obs") < 0) {
  gov_risk_index <- -gov_risk_index
}
gov_risk_index_z <- zscore_clip(gov_risk_index, cap = 2)

# --------------------------
# PCA stats for each index
# --------------------------
eq_imp  <- as.data.frame(t(summary(pca_eq)$importance))
gov_imp <- as.data.frame(t(summary(pca_gov)$importance))

cat("\n=== equity_risk_index PCA (DALY_nat_z + gender_ineq_nat_z) ===\n")
print(eq_imp)
cat("\nLoadings (PC1):\n"); print(pca_eq$rotation[, 1, drop = FALSE])

cat("\n=== gov_risk_index PCA (frag_statei_nat_z + corruption_nat_z) ===\n")
print(gov_imp)
cat("\nLoadings (PC1):\n"); print(pca_gov$rotation[, 1, drop = FALSE])

# --------------------------
# 4) Add indices + save
# --------------------------
v_idx <- cbind(v, data.frame(
  equity_risk_index_nat    = equity_risk_index,
  equity_risk_index_nat_z  = equity_risk_index_z,
  gov_risk_index_nat       = gov_risk_index,
  gov_risk_index_nat_z     = gov_risk_index_z
))

out_path <- "/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks/data/datasets/countries_nat_norm_indices.gpkg"
terra::writeVector(v_idx, out_path,
                   filetype = "GPKG",
                   layer = "countries_nat_norm_indices",
                   overwrite = TRUE)
cat("✅ Written:", out_path, "\n")

###### MERGE IN QGIS amd save as water_res_risks_norm_indices.gpkg##########

# 5) Drop components + save "ready" file
# --------------------------
library(terra)
library(dplyr)
library(here)

v <- terra::vect(here("data/input/water_res_risks_norm_indices.gpkg"))

# 1) keep only *_z columns (plus an ID if you have one)
id_cols <- intersect(c("HYBAS_ID"), names(v))  # keep any that exist
z_cols  <- grep("_z$", names(v), value = TRUE)

# 2) exclude the *_z components you used to build the PCA indices (keep only indices + other z's)
exclude_components <- c("DALY_nat_z", "gender_ineq_nat_z", "frag_statei_nat_z", "corruption_nat_z")
z_cols_keep <- setdiff(z_cols, exclude_components)

# 3) build final "ready" layer
keep_cols <- unique(c(id_cols, z_cols_keep))
v_ready <- v[, keep_cols]

# sanity checks
cat("Kept", length(keep_cols), "columns:\n")
print(keep_cols)
cat("\nDropped *_z components:\n")
print(intersect(z_cols, exclude_components))

# 4) write
out_path_ready <- here("data/input/water_res_risks_z_ready.gpkg")
terra::writeVector(
  v_ready, out_path_ready,
  filetype = "GPKG",
  layer = "water_res_risks_z_ready",
  overwrite = TRUE
)
cat("✅ Wrote:", out_path_ready, "\n")


# #Index creation using PCA
# 
# library(terra)
# library(dplyr)
# library(here)
# 
# library(corrplot)
# library(ggplot2)
# library(patchwork)
# 
# library(FactoMineR)
# library(factoextra)
# 
# 
# # 1) Load normalized GPKG
# # --------------------------
# v <- terra::vect(here("data/input/water_res_risks_norm.gpkg"))
# df_attr <- terra::values(v, dataframe = TRUE)
# 
# # --------------------------
# # 2) Equity index (DALY_nat_z and gender_ineq_nat_z)
# # --------------------------
# X_eq <- df_attr[, c("DALY_nat_z", "gender_ineq_nat_z")]
# cc_eq <- complete.cases(X_eq)
# 
# pca_eq <- prcomp(X_eq[cc_eq, ], center = TRUE, scale. = FALSE)
# equity_risk_index <- rep(NA_real_, nrow(df_attr))
# equity_risk_index [cc_eq] <- as.numeric(pca_eq$x[, 1])
# 
# equity_risk_index_z <- zscore_clip(equity_risk_index, cap = 2)
# 
# # --------------------------
# # 3) Governance risk index (frag_statei_nat_z + corruption_nat_z)
# # --------------------------
# X_gov <- df_attr[, c("frag_statei_nat_z", "corruption_nat_z")]
# cc_gov <- complete.cases(X_gov)
# 
# pca_gov <- prcomp(X_gov[cc_gov, ], center = TRUE, scale. = FALSE)
# gov_risk_index <- rep(NA_real_, nrow(df_attr))
# gov_risk_index [cc_gov] <- as.numeric(pca_gov$x[, 1])
# 
# # orient sign: higher gov_risk_index = higher frag_statei_z (more fragile - as corruption perception is negatively correlated with state fragility)
# if (cor(gov_risk_index[cc_gov], df_attr$frag_statei[cc_gov], use = "complete.obs") < 0) {
#   gov_risk_index <- -gov_risk_index
# }
# 
# gov_risk_index_z <- zscore_clip(gov_risk_index, cap = 2)
# 
# # --------------------------
# # PCA stats for each index
# # --------------------------
# eq_imp  <- as.data.frame(t(summary(pca_eq)$importance))
# gov_imp <- as.data.frame(t(summary(pca_gov)$importance))
# 
# cat("\n=== equity_risk_index PCA (DALY_z + gender_ineq_z) ===\n")
# print(eq_imp)
# cat("\nLoadings (PC1):\n"); print(pca_eq$rotation[, 1, drop = FALSE])
# 
# cat("\n=== gov_risk_index PCA (frag_statei_z + corruption_z) ===\n")
# print(gov_imp)
# cat("\nLoadings (PC1):\n"); print(pca_gov$rotation[, 1, drop = FALSE])
# 
# # --------------------------
# # 4) Add indices + save
# # --------------------------
# v_idx <- cbind(v, data.frame(
#   equity_risk_index = equity_risk_index,
#   equity_risk_index_z = equity_risk_index_z,
#   gov_risk_index     = gov_risk_index,
#   gov_risk_index_z     = gov_risk_index_z
# ))
# 
# out_path <- here("data/input/water_res_risks_norm_indices.gpkg")
# terra::writeVector(v_idx, out_path,
#                    filetype = "GPKG",
#                    layer = "water_res_risks_norm_indices",
#                    overwrite = TRUE)
# cat("✅ Written:", out_path, "\n")
# 
# # --------------------------
# # 5) Drop components + save "ready" file
# # --------------------------
# drop_cols <- c("DALY_z", "gender_ineq_z", "frag_statei_z", "corruption_z")
# 
# v_ready <- v_idx[, !(names(v_idx) %in% drop_cols)]
# 
# out_path_ready <- here("data/input/water_res_risks_z_ready.gpkg")
# terra::writeVector(v_ready, out_path_ready,
#                    filetype = "GPKG",
#                    layer = "water_res_risks_z_ready",
#                    overwrite = TRUE)
# cat("✅ Wrote:", out_path_ready, "\n")

# #Creation of DALY and Gender index
# 
# sub_pca <- prcomp(df[, c("DALY", "gender_ineq")], scale. = TRUE)
# print(sub_pca)
# df$equity_index <- sub_pca$x[,1]   # first PC scores
# 
# # Extract loadings
# loadings <- res_pca$var$coord[, 1:3]  # PC1, PC2, PC3
# df_loadings <- as.data.frame(loadings)
# df_loadings$var <- rownames(loadings)
# 
# # 3D scatter of variable loadings
# plot_ly(df_loadings, x=~Dim.1, y=~Dim.2, z=~Dim.3,
#         text=~var, type="scatter3d", mode="markers+text", textposition="top center")
# 
# library(sf)
# library(dplyr)
# library(FactoMineR)
# library(factoextra)
# library(corrplot)
# library(ggplot2)
# library(patchwork)
# library(here)
# 
# # --------------------------
# # 1) Load and add the index
# # --------------------------
# v  <- st_read(here("data/input/water_res_risks_gaussRank.gpkg"), quiet = TRUE)
# 
# # Compute PC1 from DALY_gr + gender_ineq_gr (on the attributes only)
# X  <- st_drop_geometry(v)[, c("DALY_gr", "gender_ineq_gr")]
# sub_pca <- prcomp(X, scale. = TRUE)
# v$equity_index_gr <- as.numeric(sub_pca$x[, 1])   # add *only* the new column
# 
# # OPTIONAL: drop originals if you want the index to replace them
# # v[c("DALY_gr","gender_ineq_gr")] <- NULL
# 
# # Save a new file with the index
# out_path <- here("data/input/water_res_risks_gaussRank_equityIndex.gpkg")
# st_write(v, out_path, delete_dsn = TRUE, quiet = TRUE)
# cat("✅ Wrote:", out_path, "\n")
# 
# 
# # Drop unwanted columns
# v_out <- v[, !(names(v) %in% c("DALY_gr", "gender_ineq_gr", "frag_statei_gr"))]
# 
# # Save new file
# out_path <- here("data/input/water_res_risks_gr_ready.gpkg")
# st_write(v_out, out_path, delete_dsn = TRUE, quiet = TRUE)
# cat("✅ Wrote:", out_path, "\n")
# 
# 
# # --------------------------
# # 2) Correlation matrix
# # --------------------------
# df_attr <- st_drop_geometry(v)
# cols_gr <- setdiff(
#   grep("_gr$", names(df_attr), value = TRUE),
#   c("frag_statei_gr", "DALY_gr", "gender_ineq_gr")
# )
# 
# 
# set.seed(123)
# idx <- sample(nrow(df_attr), min(9000, nrow(df_attr)))
# df_sample <- df_attr[idx, cols_gr, drop = FALSE]
# 
# cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# png("plots/input_correlation_matrix_equityIndex.png", width = 2000, height = 1600, res = 300)
# corrplot(cor_mat,
#          method="shade", type="upper", diag=TRUE,
#          tl.cex=0.5, tl.col="black", addCoef.col="black", tl.srt=45, number.cex=0.5)
# dev.off()
# 
# # --------------------------
# # 3) Density plots (ranked vars)
# # --------------------------
# plots <- lapply(cols_gr, function(col) {
#   x <- df_attr[[col]]
#   x <- x[is.finite(x)]
#   ggplot(data.frame(x = x), aes(x = x)) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 8),
#       axis.title = element_text(size = 7),
#       axis.text  = element_text(size = 6)
#     ) +
#     labs(title = col, x = col, y = "Density")
# })
# 
# combined <- patchwork::wrap_plots(plots, ncol = 4)
# ggsave(here("plots/normalized_variables_distribution_equityIndex.png"),
#        combined, width = 12, height = 8, dpi = 450)
# 
# # --------------------------
# # 4) PCA on the ranked set
# # --------------------------
# res_pca <- PCA(df_attr[, cols_gr, drop = FALSE], scale.unit = FALSE, graph = FALSE)
# 
# # Scree
# fviz_eig(res_pca)
# 
# # Variable circle (PC1–PC2)
# fviz_pca_var(res_pca,
#              col.var = "contrib",
#              gradient.cols = c("steelblue", "orange", "red"),
#              repel = TRUE)
# 
# # Top contributors
# fviz_contrib(res_pca, choice = "var", axes = 1, top = 10)
# fviz_contrib(res_pca, choice = "var", axes = 2, top = 10)
# 
# slice_sample(n = min(9000, nrow(.)))
# 
# cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # Save correlation plot
# png("plots/input_correlation_matrix_equityIndex.png", width = 2000, height = 1600, res = 300)
# corrplot(cor_mat,
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# dev.off()
# 
# # --------------------------
# # 6. Density plots
# # --------------------------
# plots <- lapply(cols_to_plot, function(col) {
#   x <- df_reduced[[col]]
#   ggplot(data.frame(x = x), aes(x = x)) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 8),
#       axis.title = element_text(size = 7),
#       axis.text  = element_text(size = 6)
#     ) +
#     labs(title = col, x = col, y = "Density")
# })
# 
# combined_plot <- wrap_plots(plots, ncol = 4)
# ggsave(here("plots/normalized_variables_distribution_equityIndex.png"),
#        combined_plot, width = 12, height = 8, dpi = 450)
# 
# # --------------------------
# # 7. PCA on reduced dataset
# # --------------------------
# res_pca <- PCA(df_reduced[, cols_to_plot], scale.unit = FALSE, graph = FALSE)
# 
# # Scree plot
# fviz_eig(res_pca)
# 
# # Variable circle (PC1–PC3)
# fviz_pca_var(res_pca,
#              axes = c(1, 3),
#              col.var = "contrib",
#              gradient.cols = c("steelblue", "orange", "red"),
#              repel = TRUE)
