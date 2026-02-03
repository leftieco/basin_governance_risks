# NORMALISATION


# 
# # 6. Apply Z-score normalization to all selected columns
# df[simplified_cols] <- lapply(df[simplified_cols], function(x) {
#   (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
# })
# 
# # 7. Replace attributes in SpatVector
# v[] <- df
# 
# # 8. Save to GeoPackage
# writeVector(v, here("data/input/allbutstorage_masked_scaled_noNA.gpkg"), overwrite = TRUE)
# 
# # # 9. Reload the normalized GPKG (optional, or just continue using `v`)
# v <- vect(here("data/input/allbutstorage_masked_scaled.gpkg"))
# 
# # # Extract the attribute table
# # df <- as.data.frame(v)
# 
# # Apply clipping to ±2 for those columns
# df[simplified_cols] <- lapply(df[simplified_cols], function(x) {
#   pmax(pmin(x, 2), -2)
# })
# 
# # Assign clipped attributes back to SpatVector
# v[] <- df
# 
# # Save final clipped version
# writeVector(v, here("data/input/allbutstorage_masked_scaled_clipped.gpkg"), overwrite = TRUE)
# 
# 
# # 10. Drop rows with any NA in normalized variables (SOM-ready dataset)
# df <- as.data.frame(v)
# 
# # Drop rows where any simplified (normalized) variable is NA
# v <- v[complete.cases(df[, simplified_cols]), ]
# 
# # Save final version ready for SOM input
# writeVector(v, here("data/input/allbutstorage_som_ready.gpkg"), overwrite = TRUE)
# 
# 
# Plot distributions
# Load libraries
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)


# Load renamed GeoPackage
v <- vect(here("data/input/wfunctions_masked_renamed.gpkg"))
names(v)[names(v) == "cultivland%"] <- "cultivland_pc"
names(v)[names(v) == "snw_pc_cyr_mean"] <- "snowcover"
names(v)[names(v) == "dis_m3_pyr_mean"] <- "discharge"

# Convert to data.frame for filtering
df <- as.data.frame(v)


# Define target columns
target_cols <- c(
  "porosity", "goveff", "biomasswprod", "cropwfoot", "hdi",
  "yield_DIN", "yield_DIP", "snowcover", "permafrost", "slope",
  "flowcessation", "soilwstorcap", "hydropot", "popdensity", "udrinkw",
  "rootzmoistcap", "participdem", "discharge", "cultivland_pc"
)

# Keep only rows where all target columns are complete (no NA)
complete_rows <- complete.cases(df[, target_cols])

# Filter the SpatVector
v <- v[complete_rows, ]

# Save to new GeoPackage
writeVector(v, here("data/input/wfunctions_noNA.gpkg"), overwrite = TRUE)

# Columns you want to plot
cols_to_plot <- c(
  "porosity", "goveff", "biomasswprod", "cropwfoot", "hdi",
  "yield_DIN", "yield_DIP", "snowcover", "permafrost", "slope",
  "flowcessation", "soilwstorcap", "hydropot", "popdensity", "udrinkw",
  "rootzmoistcap", "participdem", "discharge", "cultivland_pc")

# Named titles keyed by column name (spellings fixed, no duplicates)
full_names <- c(
  porosity        = "Soil porosity (%)",
  goveff          = "Governance effectiveness index (−2.5 to 2.5)",
  biomasswprod    = "Gross biomass water productivity",
  cropwfoot       = "Agricultural water footprint",
  hdi             = "Human Development Index (0–1)",
  yield_DIN       = "DIN yield",
  yield_DIP       = "DIP yield",
  snowcover       = "Annual snow cover extent (%)",
  permafrost      = "Permafrost extent (%)",
  slope           = "Terrain slope (degrees ×10)",
  flowcessation   = "Probability of annual flow cessation (0–1)",
  soilwstorcap    = "Soil water storage capacity (mm)",
  hydropot        = "Potential hydropower (Wh/year)",
  popdensity      = "Population density (people/km²)",
  udrinkw         = "Unimproved drinking water (0–1)",
  rootzmoistcap   = "Root-zone moisture capacity (mm)",
  participdem     = "Participatory democracy (0–1)",
  discharge       = "Mean annual discharge (m³/year)",
  cultivland_pc   = "Cultivated & managed land (%)"
)


# Safety: ensure every column has a title
missing_titles <- setdiff(cols_to_plot, names(full_names))
if (length(missing_titles)) stop("Missing titles for: ", paste(missing_titles, collapse=", "))

#####BEFORE NORMALISATION, CHECK THE DATA

library(e1071) # for skewness, kurtosis

summary_stats <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df[[col]])
  x <- x[is.finite(x)]
  data.frame(
    variable = col,
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE),
    pct_zero = mean(x == 0, na.rm = TRUE) * 100,
    n = length(x)
  )
})
summary_stats <- do.call(rbind, summary_stats)

print(summary_stats)

# Build density plots (drops NAs, keeps your xlim)
library(ggplot2)
plots <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df[[col]])
  x <- x[is.finite(x)]
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6) ) +
    labs(title = full_names[[col]], x = col, y = "Density") 
})

library(patchwork)
# Combine plots in tiles using patchwork
combined_plot <- wrap_plots(plots, ncol = 4)

# Display combined plot
print(combined_plot)

# Optionally, save the combined plot as an image
ggsave(here("plots/raw_variables_distribution_nona.png"), combined_plot, width = 12, height = 8, dpi = 450)


## Correlation matrix
library(corrplot)
library(dplyr)

# Prepare data for correlation matrix
colnames_plot <- full_names
names(colnames_plot) <- cols_to_plot

df_sample <- df[, cols_to_plot, drop = FALSE]
df_sample <- df_sample[sample(nrow(df_sample), min(9000, nrow(df_sample))), ]
colnames(df_sample) <- colnames_plot

# Compute Pearson correlation matrix
cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")

# Find top 4 absolute correlations (excluding self-correlations)
cor_vals <- as.data.frame(as.table(cor_mat)) %>%
  filter(Var1 != Var2) %>%
  mutate(abs_val = abs(Freq)) %>%
  arrange(desc(abs_val)) %>%
  distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
  head(4)

# Display values
print(cor_vals)

# Display correlation plot in R session
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)     # shrink numbers inside cells)

dev.off()

# Save correlation plot to PNG
png("plots/input_correlation_matrix_raw_new.png", width = 1200, height = 800, res = 450)
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black")
dev.off()


## Data mutation and normalisation

# ---- Load base data ----
v <- st_read(here("data/input//wfunctions_noNA.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# ---- Helper: Gaussian rank with clipping ----
gauss_rank_z <- function(x, cap = 2) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(is.finite(x))
  z <- qnorm(r / (n + 1))   # Gaussianized ranks
  pmin(pmax(z, -cap), cap)  # clip to [-cap, cap]
}

# ---- Variables to transform ----
rank_sources <- tibble(
  porosity       = df$porosity,
  goveff         = df$goveff,
  biomasswprod   = df$biomasswprod,
  cropwfoot      = df$cropwfoot,
  hdi            = df$hdi,
  yield_DIN      = df$yield_DIN,
  yield_DIP      = df$yield_DIP,
  snowcover      = df$snowcover,
  permafrost     = df$permafrost,
  slope          = df$slope,
  flowcessation  = df$flowcessation,
  soilwstorcap   = df$soilwstorcap,
  hydropot       = df$hydropot,
  popdensity     = df$popdensity,
  udrinkw        = df$udrinkw,
  rootzmoistcap  = df$rootzmoistcap,
  participdem    = df$participdem,
  discharge      = df$discharge,
  cultivland_pc  = df$cultivland_pc
)

# ---- Apply Gaussian rank ----
df_gr <- as.data.frame(lapply(rank_sources, gauss_rank_z))
names(df_gr) <- paste0(names(rank_sources), "_gr")   # suffix for clarity

# ---- Bind back to geometry ----
v_out <- v %>%
  dplyr::bind_cols(df_gr)

# ---- Write new GPKG ----
out_path <- here("data/input/wfunctions_gaussRank.gpkg")
st_write(v_out, out_path, delete_dsn = TRUE)

cat("✅ Gaussian rank scores added and written to:", out_path, "\n")

library(sf)
library(dplyr)
library(car)          # for VIF
library(FactoMineR)   # PCA
library(factoextra)   # PCA plots
library(here)

# --- Load normalized dataset ---
v <- st_read(here("data/input/wfunctions_gaussRank.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# --- Select only *_gr normalized columns ---
cols_gr <- grep("_gr$", names(df), value = TRUE)
df_gr   <- df[, cols_gr]

# --- VIF calculation ---
# Fit linear model with arbitrary response, since we only want VIFs
lm_model <- lm(df_gr[[1]] ~ ., data = df_gr)  
vif_vals <- vif(lm_model)
print(vif_vals)

# --- Correlation matrix ---

# Sample to match your workflow
df_gr_sample <- df_gr %>%
  slice_sample(n = min(18000, nrow(.)))

# Pearson on Gaussian ranks  ≈ Spearman on sources
cor_mat_gr <- cor(df_gr_sample, use = "pairwise.complete.obs", method = "pearson")

# On-screen plot
corrplot(cor_mat_gr, 
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)
png("plots/input_correlation_matrix_gr.png", width = 1200, height = 800, res = 450)


# New distribution density plots

full_names_gr <- c(
  porosity_gr        = "Soil porosity (%)",
  goveff_gr         = "Governance effectiveness index (−2.5 to 2.5)",
  biomasswprod_gr    = "Gross biomass water productivity",
  cropwfoot_gr_gr       = "Agricultural water footprint",
  hdi_gr_gr             = "Human Development Index (0–1)",
  yield_DIN_gr       = "DIN yield",
  yield_DIP_gr       = "DIP yield",
  snowcover_gr       = "Annual snow cover extent (%)",
  permafrost_gr      = "Permafrost extent (%)",
  slope_gr           = "Terrain slope (degrees ×10)",
  flowcessation_gr   = "Probability of annual flow cessation (0–1)",
  soilwstorcap_gr    = "Soil water storage capacity (mm)",
  hydropot_gr        = "Potential hydropower (Wh/year)",
  popdensity_gr      = "Population density (people/km²)",
  udrinkw_gr         = "Unimproved drinking water (0–1)",
  rootzmoistcap_gr   = "Root-zone moisture capacity (mm)",
  participdem_gr     = "Participatory democracy (0–1)",
  discharge_gr       = "Mean annual discharge (m³/year)",
  cultivland_pc_gr   = "Cultivated & managed land (%)"
)

library(ggplot2)
# 2) Build density plots (index title with the current 'col'; fall back to 'col' if missing)
plots <- lapply(cols_gr, function(col) {
  # safer to pull from df_gr, which only has *_gr columns
  x <- as.numeric(df_gr[[col]])
  x <- x[is.finite(x)]
  
  title_here <- if (col %in% names(full_names_gr)) full_names_gr[[col]] else col
  x_label    <- sub("_gr$", "", col)
  
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6)
    ) +
    labs(title = title_here, x = x_label, y = "Density")
})

library(patchwork)
# Combine plots in tiles using patchwork
combined_plot <- wrap_plots(plots, ncol = 4)

# Display combined plot
print(combined_plot)

# Optionally, save the combined plot as an image
ggsave(here("plots/gr_variables_distribution.png"), combined_plot, width = 12, height = 8, dpi = 450)


# --- PCA ---
res_pca <- PCA(df_gr, scale.unit = FALSE, graph = FALSE)

# Scree plot: variance explained
fviz_eig(res_pca)

# Correlation circle: how vars load onto PCs
fviz_pca_var(res_pca,
             col.var = "contrib",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE)

# Contributions to PCs
fviz_contrib(res_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res_pca, choice = "var", axes = 2, top = 10)


### Invert metrics and exclude some

# --- Copy your Gaussian-ranked data ---
df_corr <- df_gr

# --- 1) Invert udrinkw_gr (so higher = better) ---
df_corr$udrinkw_gr <- df_corr$udrinkw_gr * (-1)
df_corr$flowcessation_gr <- df_corr$flowcessation_gr * (-1)

# --- 2) Exclude permafrost_gr and snowcover_gr from correlation analysis ---
exclude_vars <- c("permafrost_gr", "snowcover_gr")

df_corr <- df_corr[, !(names(df_corr) %in% exclude_vars)]

cor_mat <- cor(df_corr, use = "pairwise.complete.obs", method = "spearman")

# On-screen plot
corrplot(cor_mat, 
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)
png("plots/input_correlation_matrix_gr_noSnow.png", width = 1200, height = 800, res = 450)

### P and N

library(sf)
library(here)

# --------------------------
# 1) Load base data
# --------------------------
gpkg_in <- here("data/input/wfunctions_gaussRank.gpkg")
v <- st_read(gpkg_in, quiet = TRUE)

# --------------------------
# 2) Build Nutrient Index (DIN + DIP)
# --------------------------
needed <- c("yield_DIN_gr", "yield_DIP_gr")
missing <- setdiff(needed, names(v))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

X <- st_drop_geometry(v)[, needed, drop = FALSE]

# PCA on the two nutrient variables
sub_pca <- prcomp(X, scale. = TRUE)

# Extract PC1 scores
nutri_pc1 <- as.numeric(sub_pca$x[, 1])

# Optional: make PC1 direction intuitive:
# Ensure PC1 is positively correlated with both inputs (so higher PC1 = higher DIN/DIP)
cor_sign <- sign(cor(nutri_pc1, rowMeans(X, na.rm = TRUE), use = "pairwise.complete.obs"))
nutri_pc1 <- cor_sign * nutri_pc1

# Add the index
v$nutrient_index_gr <- nutri_pc1

# (Optional) Inspect variance explained 
ve <- summary(sub_pca)$importance["Proportion of Variance", 1]
cat(sprintf("Nutrient PC1 variance explained: %.1f%%\n", 100 * ve))

### Nutrient PC1 variance explained: 94.9%

# --------------------------
# 3) Save with the index added
# --------------------------
out_with_index <- here("data/input/wfunctions_gaussRank_nutrientIndex.gpkg")
st_write(v, out_with_index, delete_dsn = TRUE, quiet = TRUE)
cat("✅ Wrote: ", out_with_index, "\n", sep = "")

###CRYO

cryovars <- c("permafrost_gr", "snowcover_gr")
missing <- setdiff(cryovars, names(v))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

X <- st_drop_geometry(v)[, cryovars, drop = FALSE]
cryopca <- prcomp(X, scale. = TRUE)

# Align PC1 so that higher PC1 = more cryosphere (higher means of permafrost/snow)
pc1 <- as.numeric(cryopca$x[, 1])
pc1 <- sign(cor(pc1, rowMeans(X, na.rm = TRUE), use = "pairwise.complete.obs")) * pc1

v$cryosphere_index_gr <- pc1

# Optional: inspect variance explained
ve <- summary(cryopca)$importance["Proportion of Variance", 1]
cat(sprintf("Cryosphere PC1 variance explained: %.1f%%\n", 100 * ve))

### Cryosphere PC1 variance explained: 89.1%

# --------------------------
# 3) Save a file that includes the cryosphere index (keep originals here)
# --------------------------
out_with_index <- here("data/input/wfunctions_gaussRank_indices.gpkg")
st_write(v, out_with_index, delete_dsn = TRUE, quiet = TRUE)
cat("✅ Wrote: ", out_with_index, "\n", sep = "")

# --------------------------
# 4) Build v_ready:
#    - drop permafrost_gr and snowcover_gr
#    - invert & rename udrinkw_gr -> drinkwaccess_gr
#    - invert & rename flowcessation_gr -> continflow_gr
# --------------------------
v <- st_read(here("data/input/wfunctions_gaussRank_indices.gpkg"), quiet = TRUE)
v_ready <- v

# --------------------------
# Drop orginals (DIN/DIP) for a 'ready' dataset
# --------------------------
v_ready <- v_ready[, !(names(v_ready) %in% needed)]

# Drop original cryosphere components
v_ready <- v_ready[, !(names(v_ready) %in% cryovars)]

# Invert & rename udrinkw_gr -> drinkwaccess_gr
if ("udrinkw_gr" %in% names(v_ready)) {
  v_ready$drinkwaccess_gr <- -1 * v_ready$udrinkw_gr
  v_ready$udrinkw_gr <- NULL
} else {
  warning("udrinkw_gr not found; drinkwaccess_gr not created.")
}

# Invert & rename flowcessation_gr -> continflow_gr
if ("flowcessation_gr" %in% names(v_ready)) {
  v_ready$continflow_gr <- -1 * v_ready$flowcessation_gr
  v_ready$flowcessation_gr <- NULL
} else {
  warning("flowcessation_gr not found; continflow_gr not created.")
}

# --------------------------
# 5) Save ready dataset
# --------------------------
out_ready <- here("data/input/wfunctions_gr_ready.gpkg")
st_write(v_ready, out_ready, delete_dsn = TRUE, quiet = TRUE)
cat("✅ Wrote: ", out_ready, "\n", sep = "")


### REANALYSIS
v <- st_read(here("data/input/wfunctions_gr_ready.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# --- Select only *_gr normalized columns ---
cols_gr <- grep("_gr$", names(df), value = TRUE)
df_gr   <- df[, cols_gr]

# --- VIF calculation ---
# Fit linear model with arbitrary response, since we only want VIFs
lm_model <- lm(df_gr[[1]] ~ ., data = df_gr)  
vif_vals <- vif(lm_model)
print(vif_vals)

# --- Correlation matrix ---

# Sample to match your workflow
df_gr_sample <- df_gr %>%
  slice_sample(n = min(18000, nrow(.)))

# Pearson on Gaussian ranks  ≈ Spearman on sources
cor_mat_gr <- cor(df_gr_sample, use = "pairwise.complete.obs", method = "pearson")

summary(cor_mat_gr)

# On-screen plot
corrplot(cor_mat_gr, 
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)
png("plots/input_correlation_matrix_gr.png", width = 1200, height = 800, res = 450)


# New distribution density plots

full_names_gr <- c(
  porosity_gr        = "Soil porosity (%)",
  goveff_gr         = "Governance effectiveness index (−2.5 to 2.5)",
  biomasswprod_gr    = "Gross biomass water productivity",
  cropwfoot_gr       = "Agricultural water footprint",
  hdi_gr             = "Human Development Index (0–1)",
  snowcover_gr       = "Annual snow cover extent (%)",
  permafrost_gr      = "Permafrost extent (%)",
  slope_gr           = "Terrain slope (degrees ×10)",
  continflow_gr     = "Probability of continuous flow (0–1)",
  soilwstorcap_gr    = "Soil water storage capacity (mm)",
  hydropot_gr        = "Potential hydropower (Wh/year)",
  popdensity_gr      = "Population density (people/km²)",
  drinkwaccess_gr   = "Improved drinking water (0–1)",
  rootzmoistcap_gr   = "Root-zone moisture capacity (mm)",
  participdem_gr     = "Participatory democracy (0–1)",
  discharge_gr       = "Mean annual discharge (m³/year)",
  cultivland_pc_gr   = "Cultivated & managed land (%)",
  nutrient_index_gr  = "DIN and DIP yields, PCA",
  cryosphere_index_gr = "Cryosphere extent"
)

library(ggplot2)
# 2) Build density plots (index title with the current 'col'; fall back to 'col' if missing)
plots <- lapply(cols_gr, function(col) {
  # safer to pull from df_gr, which only has *_gr columns
  x <- as.numeric(df_gr[[col]])
  x <- x[is.finite(x)]
  
  title_here <- if (col %in% names(full_names_gr)) full_names_gr[[col]] else col
  x_label    <- sub("_gr$", "", col)
  
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6)
    ) +
    labs(title = title_here, x = x_label, y = "Density")
})

library(patchwork)
# Combine plots in tiles using patchwork
combined_plot <- wrap_plots(plots, ncol = 4)

# Display combined plot
print(combined_plot)

# Optionally, save the combined plot as an image
ggsave(here("plots/gr_variables_distribution_indices.png"), combined_plot, width = 12, height = 8, dpi = 450)


# --- PCA ---
res_pca <- PCA(df_gr, scale.unit = FALSE, graph = FALSE)

# Scree plot: variance explained
fviz_eig(res_pca)

# Correlation circle: how vars load onto PCs
fviz_pca_var(res_pca,
             col.var = "contrib",
             gradient.cols = c("steelblue", "orange", "red"),
             repel = TRUE)

# Contributions to PCs
fviz_contrib(res_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res_pca, choice = "var", axes = 2, top = 10)


# #CORRUPTION and FRAG STATE
# 
# sub_pca <- prcomp(df[, c("corruption_gr", "frag_statei")], scale. = TRUE)
# print(sub_pca)
# df$equity_index <- sub_pca$x[,1]   # first PC scores
# 
# # --- Drop frag_statei from the Gaussian-rank dataset ---
# df_gr_nofrag <- subset(df_gr, select = -frag_statei_gr)
# 
# # --- VIF calculation ---
# # Fit linear model with arbitrary response, since we only want VIFs
# lm_model <- lm(df_gr_nofrag[[1]] ~ ., data = df_gr_nofrag)  
# vif_vals <- vif(lm_model)
# print(vif_vals)
# 
# # --- PCA ---
# res_pca_nofrag <- PCA(df_gr_nofrag, scale.unit = FALSE, graph = FALSE)
# 
# # Scree plot: variance explained
# fviz_eig(res_pca_nofrag)
# 
# # Correlation circle: variables loadings
# fviz_pca_var(res_pca_nofrag,
#              col.var = "contrib",
#              gradient.cols = c("steelblue", "orange", "red"),
#              repel = TRUE)
# 
# # Contributions to first two PCs
# fviz_contrib(res_pca_nofrag, choice = "var", axes = 1, top = 10)
# fviz_contrib(res_pca_nofrag, choice = "var", axes = 2, top = 10)
# 
# # Sample to match your workflow
# df_gr_sample <- df_gr_nofrag %>%
#   slice_sample(n = min(9000, nrow(.)))
# 
# # Pearson on Gaussian ranks  ≈ Spearman on sources
# cor_mat_gr <- cor(df_gr_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # On-screen plot
# corrplot(cor_mat_gr, 
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# 
# 
# library(plotly)
# 
# #DALY and Gender
# 
# sub_pca <- prcomp(df[, c("DALY_gr", "gender_ineq_gr")], scale. = TRUE)
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
# # 3) Density plots (ranked vars; should look ~Normal)
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
#   slice_sample(n = min(9000, nrow(.)))
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
# 




# # ---------- helpers ----------
# zscore_clip <- function(x, cap = 2) {
#   m  <- mean(x, na.rm = TRUE)
#   sd <- stats::sd(x, na.rm = TRUE)
#   z  <- (x - m) / sd
#   z  <- pmin(pmax(z, -cap), cap)        # clip to [-cap, cap]
#   return(z)
# }
# 
# # sign-preserving log transform for signed heavy tails
# slog1p <- function(x) sign(x) * log1p(abs(x))
# 
# # ---------- variable-specific transforms ----------
# normalize_for_archetypes <- function(df) {
#   
#   df %>%
#     mutate(
#       # 1) keep as-is (bounded, reasonably shaped), then z-score
#       runoff_nonstat_z = zscore_clip(runoff_nonstat),
#       tws_nonstat_z    = zscore_clip(tws_nonstat),
#       gender_ineq_z    = zscore_clip(gender_ineq),
#       corruption_z     = zscore_clip(corruption),
#       frag_statei_z    = zscore_clip(frag_statei),
#       gini_z           = zscore_clip(gini),
#       connectedness_z   = zscore_clip(connectedness),
#       
#       # 2) heavy right tails -> log1p
#       DALY_log         = log1p(DALY),
#       wwthdr_pc_log    = log1p(wwthdr_pc),
#       Base_water_stress_log = log1p(Base_water_stress),
#       hydropol_int_log = log1p(hydropol_int),
#       thrspecies_log   = log1p(thrspecies),
#       mismplastic_log      = log1p(mismplastic),
#       droughthaz_log       = log1p(droughthaz),
#       extremes_damage_log  = log1p(extremes_damage),
#       extremes_affected_log = log1p(extremes_affected),
#       
#       DALY_z           = zscore_clip(DALY_log),
#       wwthdr_pc_z      = zscore_clip(wwthdr_pc_log),
#       Base_water_stress_z = zscore_clip(Base_water_stress_log),
#       hydropol_int_z   = zscore_clip(hydropol_int_log),
#       thrspecies_z     = zscore_clip(thrspecies_log),
#       droughthaz_z       = zscore_clip(droughthaz_log),
#       mismplastic_z       = zscore_clip(mismplastic_log),
#       extremes_damage_z       = zscore_clip(extremes_damage_log),
#       extremes_affected_z       = zscore_clip(extremes_affected_log),
#       
#       
#       # 3) signed heavy tails -> sign-preserving log
#       bod_slog         = slog1p(bod),
#       wetlandloss_slog = slog1p(wetlandloss),
#       
#       bod_z            = zscore_clip(bod_slog),
#       wetlandloss_z    = zscore_clip(wetlandloss_slog),
#       
#       # 4) e_flow: mixed signs, bounded; leave as-is then z-score (or
#       #    swap for a custom mapping if you prefer)
#       e_flow_z         = zscore_clip(e_flow)
#     )
# }
# 
# # ---------- run ----------
# # assuming your data.frame is named `df`
# df_norm <- normalize_for_archetypes(df)
# 
# # optional: keep only the normalized features for modeling
# cols_z <- grep("_z$", names(df_norm), value = TRUE)
# archetype_input <- df_norm[, cols_z, drop = FALSE]
# 
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(purrr)
# library(patchwork)   # for arranging plots
# # install.packages("corrplot") if needed
# library(corrplot)
# 
# ## Correlation matrix on normalized (Z) variables
# library(corrplot)
# 
# # Select *_z variables
# cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)
# full_names   <- gsub("_z$", "", cols_to_plot)  # nicer labels
# 
# # Sample rows to keep things fast
# df_sample <- df_norm[, cols_to_plot, drop = FALSE]              # keep only needed columns
# df_sample <- df_sample[sample(nrow(df_sample), 
#                               min(9000, nrow(df_sample))), ]    # sample up to 9000 rows
# colnames(df_sample) <- full_names                               # rename for nice labels
# 
# 
# # Compute Pearson correlation matrix
# cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # Find top 4 absolute correlations (excluding self-correlations)
# cor_vals <- as.data.frame(as.table(cor_mat)) %>%
#   filter(Var1 != Var2) %>%
#   mutate(abs_val = abs(Freq)) %>%
#   arrange(desc(abs_val)) %>%
#   distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
#   head(4)
# 
# print(cor_vals)
# 
# # Plot in R session
# corrplot(cor_mat,
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# 
# # Save to PNG
# png("plots/input_correlation_matrix_z.png", width = 1000, height = 800, res = 300)
# 
# # Build density plots for Z-normalized variables (drops NAs)
# library(ggplot2)
# library(patchwork)
# 
# # Select *_z variables
# cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)
# full_names   <- gsub("_z$", "", cols_to_plot)  # nicer titles
# 
# # Density plots
# plots <- lapply(seq_along(cols_to_plot), function(i) {
#   col <- cols_to_plot[i]
#   name <- full_names[i]
#   
#   x <- as.numeric(df_norm[[col]])
#   x <- x[is.finite(x)]
#   
#   ggplot(data.frame(x = x), aes(x = x)) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 8),
#       axis.title = element_text(size = 7),
#       axis.text  = element_text(size = 6) ) +
#     labs(title = name, x = col, y = "Density")
# })
# 
# # Combine into grid
# combined_plot <- wrap_plots(plots, ncol = 4)
# 
# # Display
# print(combined_plot)
# 
# # Save
# ggsave(here("plots/normalized_variables_distribution_z_gauss.png"), 
#        combined_plot, width = 12, height = 8, dpi = 450)
# 
# 
# # #experiment_robust Z
# # 
# # winsor <- function(x, p = 0.01) { 
# #   lo <- quantile(x, p, na.rm = TRUE); hi <- quantile(x, 1-p, na.rm = TRUE)
# #   x <- pmax(pmin(x, hi), lo)
# #   x
# # }
# # zscore_robust <- function(x, cap = 2, p = 0.01) {
# #   xw <- winsor(x, p)
# #   z  <- (xw - median(xw, na.rm = TRUE)) / (mad(xw, na.rm = TRUE) * 1.4826)
# #   pmin(pmax(z, -cap), cap)
# # }
# # 
# # df_norm <- df_norm %>%
# #   mutate(
# #     bod_z            = zscore_robust(bod_slog),
# #     wetlandloss_z    = zscore_robust(wetlandloss_slog),
# #     Base_water_stress_z = zscore_robust(Base_water_stress_log),
# #     DALY_z           = zscore_robust(DALY_log),
# #     wwthdr_pc_z      = zscore_robust(wwthdr_pc_log),
# #     hydropol_int_z   = zscore_robust(hydropol_int_log),
# #     thrspecies_z     = zscore_robust(thrspecies_log),
# #     connectedness_z  = zscore_robust(100 - connectedness),
# #     e_flow_z         = zscore_robust(e_flow)
# #   )
# # 
# # clip_share <- function(z) mean(abs(z) >= 1.999, na.rm = TRUE)*100
# # sapply(grep("_z$", names(df_norm), value = TRUE), function(v) clip_share(df_norm[[v]]))
# 
# #Gaussian rank
# 
# gauss_rank_z <- function(x, cap = 2) {
#   r <- rank(x, na.last = "keep", ties.method = "average")
#   n <- sum(!is.na(x))
#   z <- qnorm(r/(n + 1))              # ~N(0,1) by ranks
#   pmin(pmax(z, -cap), cap)
# }
# 
# df_norm <- df_norm %>%
#   mutate(
#     # keep your existing *_z for the well-behaved ones…
#     
#     # replace the heavy-tailed z’s with Gaussian-rank versions:
#     bod_z            = gauss_rank_z(bod_slog),
#     wetlandloss_z    = gauss_rank_z(wetlandloss_slog),
#     Base_water_stress_z = gauss_rank_z(Base_water_stress_log),
#     DALY_z           = gauss_rank_z(DALY_log),
#     wwthdr_pc_z      = gauss_rank_z(wwthdr_pc_log),
#     hydropol_int_z   = gauss_rank_z(hydropol_int_log),
#     thrspecies_z     = gauss_rank_z(thrspecies_log),
#     connectedness_z  = gauss_rank_z(connectedness),  # ensure inversion!
#     e_flow_z         = gauss_rank_z(e_flow),
#     mismplastic_z         = gauss_rank_z(mismplastic_log),
#     droughthaz_z         = gauss_rank_z(droughthaz_log),
#     extremes_damage_z         = gauss_rank_z(extremes_damage_log),
#     extremes_affected_z         = gauss_rank_z(extremes_affected_log),
#     
#   )
# 
# library(dplyr)
# library(corrplot)
# 
# ## -------------------------------
# ## 1) Pearson on Z-normalized vars
# ## -------------------------------
# cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)
# full_names   <- gsub("_z$", "", cols_to_plot)
# 
# # Sample rows to keep things fast
# df_sample <- df_norm[, cols_to_plot, drop = FALSE]              # keep only needed columns
# df_sample <- df_sample[sample(nrow(df_sample), 
#                               min(9000, nrow(df_sample))), ]    # sample up to 9000 rows
# colnames(df_sample) <- full_names                               # rename for nice labels
# 
# cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # Top 4 absolute correlations
# cor_vals <- as.data.frame(as.table(cor_mat)) %>%
#   filter(Var1 != Var2) %>%
#   mutate(abs_val = abs(Freq)) %>%
#   arrange(desc(abs_val)) %>%
#   distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
#   head(4)
# print(cor_vals)
# 
# # On-screen plot
# corrplot(cor_mat, 
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# 
# # Save
# png("plots/input_correlation_matrix_z.png", width = 2000, height = 1600, res = 300)
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
# # Build density plots for Z-normalized variables (drops NAs)
# library(ggplot2)
# library(patchwork)
# 
# # Select *_z variables
# cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)
# full_names   <- gsub("_z$", "", cols_to_plot)  # nicer titles
# 
# # Density plots
# plots <- lapply(seq_along(cols_to_plot), function(i) {
#   col <- cols_to_plot[i]
#   name <- full_names[i]
#   
#   x <- as.numeric(df_norm[[col]])
#   x <- x[is.finite(x)]
#   
#   ggplot(data.frame(x = x), aes(x = x)) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 8),
#       axis.title = element_text(size = 7),
#       axis.text  = element_text(size = 6) ) +
#     labs(title = name, x = col, y = "Density")
# })
# 
# # Combine into grid
# combined_plot <- wrap_plots(plots, ncol = 4)
# 
# # Display
# print(combined_plot)
# 
# ## ----------------------------------------------------------
# ## 2) Pearson on Gaussian-rank scores (≈ Spearman correlation)
# ## ----------------------------------------------------------
# 
# # Helper: Gaussian-rank transform (no clipping)
# gauss_rank <- function(x) {
#   r <- rank(x, na.last = "keep", ties.method = "average")
#   n <- sum(is.finite(x))
#   qnorm(r / (n + 1))
# }
# 
# # Build the *source* inputs for ranks.
# # Use the same pre-Z transforms you used:
# # - bounded vars: use as-is (or with your inversion for connectedness)
# # - log/ signed-log vars: use those transformed columns
# rank_sources <- dplyr::tibble(
#   runoff_nonstat      = df$runoff_nonstat,
#   tws_nonstat         = df$tws_nonstat,
#   gender_ineq         = df$gender_ineq,
#   corruption          = df$corruption,
#   frag_statei         = df$frag_statei,
#   gini                = df$gini,
#   DALY                = df_norm$DALY_log,                 # log
#   wwthdr_pc           = df_norm$wwthdr_pc_log,            # log
#   Base_water_stress   = df_norm$Base_water_stress_log,    # log
#   hydropol_int        = df_norm$hydropol_int_log,         # log
#   thrspecies          = df_norm$thrspecies_log,           # log
#   bod                 = df_norm$bod_slog,                 # signed log
#   wetlandloss         = df_norm$wetlandloss_slog,         # signed log
#   connectedness       = df$connectedness,           # invert CSI
#   e_flow              = df$e_flow,                          # as-is (or your preferred transform)
#   mismplastic         = df_norm$mismplastic_log,
#   droughthaz         = df_norm$droughthaz_log,
#   extremes_damage         = df_norm$extremes_damage_log,
#   extremes_affected         = df_norm$extremes_affected_log,
# )
# 
# # Apply Gaussian-rank variable-wise
# df_gr <- as.data.frame(lapply(rank_sources, gauss_rank))
# names(df_gr) <- names(rank_sources)
# 
# # Sample to match your workflow
# df_gr_sample <- df_gr %>%
#   slice_sample(n = min(9000, nrow(.)))
# 
# # Pearson on Gaussian ranks  ≈ Spearman on sources
# cor_mat_gr <- cor(df_gr_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # Top 4 absolute correlations (rank-based)
# cor_vals_gr <- as.data.frame(as.table(cor_mat_gr)) %>%
#   filter(Var1 != Var2) %>%
#   mutate(abs_val = abs(Freq)) %>%
#   arrange(desc(abs_val)) %>%
#   distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
#   head(4)
# print(cor_vals_gr)
# 
# # On-screen plot
# corrplot(cor_mat_gr, 
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# # Save
# png("plots/input_correlation_matrix_gaussRank.png", width = 2000, height = 1600, res = 300)
# corrplot(cor_mat_gr,
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
# # Select *_z variables
# cols_to_plot <- names(df_gr)
# full_names   <- cols_to_plot   # or your nicer labels if you have them
# 
# # Density plots
# plots <- lapply(seq_along(cols_to_plot), function(i) {
#   col <- cols_to_plot[i]
#   name <- full_names[i]
#   
#   x <- as.numeric(df_gr[[col]])
#   x <- x[is.finite(x)]
#   
#   ggplot(data.frame(x = x), aes(x = x)) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 8),
#       axis.title = element_text(size = 7),
#       axis.text  = element_text(size = 6) ) +
#     labs(title = name, x = col, y = "Density")
# })
# 
# # Combine into grid
# combined_plot <- wrap_plots(plots, ncol = 4)
# 
# # Display
# print(combined_plot)
# 
# 
# #### NO LOG, only Gaussian ranking
# 
# # ---- Helper: Gaussian rank with clipping ----
# gauss_rank_z <- function(x, cap = 2) {
#   r <- rank(x, na.last = "keep", ties.method = "average")
#   n <- sum(is.finite(x))
#   z <- qnorm(r / (n + 1))   # Gaussianized ranks
#   pmin(pmax(z, -cap), cap)  # clip to [-cap, cap]
# }
# 
# # ---- Build sources (direct values, no log stage) ----
# rank_sources <- dplyr::tibble(
#   runoff_nonstat    = df$runoff_nonstat,
#   tws_nonstat       = df$tws_nonstat,
#   gender_ineq       = df$gender_ineq,
#   corruption        = df$corruption,
#   frag_statei       = df$frag_statei,
#   gini              = df$gini,
#   DALY              = df$DALY,
#   wwthdr_pc         = df$wwthdr_pc,
#   Base_water_stress = df$Base_water_stress,
#   hydropol_int      = df$hydropol_int,
#   thrspecies        = df$thrspecies,
#   bod               = df$bod,
#   wetlandloss       = df$wetlandloss,
#   connectedness     = 100 - df$connectedness,   # invert CSI so higher = more disconnected
#   e_flow            = df$e_flow,
#   mismplastic       = df$mismplastic,
#   droughthaz        = df$droughthaz,
#   extremes_damage   = df$extremes_damage,
#   extremes_affected = df$extremes_affected
# )
# 
# # ---- Apply Gaussian rank with clipping ----
# df_gr <- as.data.frame(lapply(rank_sources, gauss_rank_z))
# names(df_gr) <- names(rank_sources)
# 
# # ---- Density plots for the Gaussian-ranked variables ----
# plots <- lapply(names(df_gr), function(col) {
#   x <- as.numeric(df_gr[[col]])
#   x <- x[is.finite(x)]
#   
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
# print(combined_plot)
# 
# 
# # Sample to match your workflow
# df_gr_sample <- df_gr %>%
#   slice_sample(n = min(9000, nrow(.)))
# 
# # Pearson on Gaussian ranks  ≈ Spearman on sources
# cor_mat_gr <- cor(df_gr_sample, use = "pairwise.complete.obs", method = "pearson")
# 
# # On-screen plot
# corrplot(cor_mat_gr, 
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.5,
#          tl.col = "black",
#          addCoef.col = "black",
#          tl.srt = 45,
#          number.cex = 0.5)
# 
# # SAVE
# # Path to the original gpkg with geometry
# library(sf)
# library(dplyr)
# library(here)
