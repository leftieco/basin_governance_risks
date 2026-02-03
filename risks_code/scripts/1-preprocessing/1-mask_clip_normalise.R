library(sf)
library(dplyr)
library(terra)
library(rnaturalearth)
library(here)

# --- Load and ensure EPSG:4326 ---
v <- vect(here("data/input/water_res_risks_full.gpkg"))

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
names(v)[names(v) == "CSI_mean"] <- "connectedness"
names(v)[names(v) == "value_mean"] <- "e_flow"
names(v)[names(v) == "bws_raw_mean"] <- "Base_water_stress"
names(v)[names(v) == "hydropol_max"] <- "hydropol_int"
names(v)[names(v) == "gini_mean"] <- "gini"


writeVector(v, here("data/input/water_res_risks_masked_renamed.gpkg"), overwrite = TRUE)

simplified_cols <- c(
  "runoff_nonstat","bod","gender_ineq","thrspecies","wetlandloss",
  "tws_nonstat","DALY","corruption","frag_statei","wwthdr_pc",
  "connectedness","e_flow","Base_water_stress","hydropol_int", "gini")

cols_to_plot <- simplified_cols

# # 3. Extract attribute table (no geometry)
# df <- as.data.frame(v)
# 
# # Replace NA with 0 only in selected columns
# cols_to_zero <- c("blue_water_prod", "water_capacity", "intermittent_flow", "crop_footprint", "hydropower")
# # "n_yield", "p_yield" - missing values in Oceania, to be not set to 0
# # crop footprint . missing values where there is no crop production
# # hydropower - NA where there were no HP potential points
# 
# df[cols_to_zero] <- lapply(df[cols_to_zero], function(x) {
#   replace(x, is.na(x), 0)
# })
# 
# # # 4. Drop rows with incomplete data
# # df <- as.data.frame(v)  # Use base data.frame for complete.cases
# # complete_rows <- complete.cases(df)
# # v <- v[complete_rows, ]
# 
# v[] <- df
# 
# writeVector(v, here("data/input/allbutstorage_masked_zeroed.gpkg"), overwrite = TRUE)
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
# # Plot distributions
# # Load libraries
# library(sf)
# library(terra)
# library(dplyr)
# library(ggplot2)
# library(patchwork)
# library(here)
# 
# # Load the final prepared data
# v <- vect(here("data/input/allbutstorage_som_ready.gpkg"))
# df <- as.data.frame(v)
# 
# # Columns to visualize
# cols_to_plot <- c(
#   "porosity", "gov_effectiveness", "udw_dependence", "blue_water_prod",
#   "water_capacity", "intermittent_flow", "hdi", "crop_footprint",
#   "n_yield", "p_yield", "hydropower"
# )
# 
# # Corresponding full metric names for titles
# full_metric_names <- c(
#   "Near-surface porosity",
#   "Governance Effectiveness",
#   "Unimproved Drinking Water Dependency",
#   "Gross Biomass Water Productivity",
#   "Available Soil Water Storage Capacity",
#   "Probability of Flow Intermittence",
#   "Human Development Index (HDI)",
#   "Combined Crop Water Footprint",
#   "Nitrogen Retention",
#   "Phosphorus Retention",
#   "Hydropower Potential"
# )
# 
# # Create individual plots using ggplot
# plots <- mapply(function(col, title) {
#   ggplot(df, aes(x = .data[[col]])) +
#     geom_density(fill = "steelblue", alpha = 0.6) +
#     theme_minimal() +
#     labs(title = title, x = "Normalised grid cell distribution", y = "Density") +
#     xlim(-2, 2)
# }, cols_to_plot, full_metric_names, SIMPLIFY = FALSE)
# 
# # Combine plots in tiles using patchwork
# combined_plot <- wrap_plots(plots, ncol = 3)
# 
# # Display combined plot
# print(combined_plot)
# 
# # Optionally, save the combined plot as an image
# ggsave(here("plots/normalized_variables_distribution.png"), combined_plot, width = 12, height = 8, dpi = 450)
# 
# 
# ## Correlation matrix
# library(corrplot)
# 
# # Prepare data for correlation matrix
# colnames_plot <- full_metric_names
# names(colnames_plot) <- cols_to_plot
# 
# df_sample <- df %>%
#   select(all_of(cols_to_plot)) %>%
#   slice_sample(n = min(4556, nrow(.))) %>%
#   setNames(colnames_plot)
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
# # Display values
# print(cor_vals)
# 
# # Display correlation plot in R session
# corrplot(cor_mat,
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.9,
#          tl.col = "black",
#          addCoef.col = "black")
# 
# # Save correlation plot to PNG
# png("assets/input_correlation_matrix.png", width = 1000, height = 800, res = 300)
# corrplot(cor_mat,
#          method = "shade",
#          type = "upper",
#          diag = TRUE,
#          tl.cex = 0.9,
#          tl.col = "black",
#          addCoef.col = "black")
# dev.off()
# 
