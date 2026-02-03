
library(sf)
library(dplyr)
library(terra)
library(rnaturalearth)
library(here)

# --- Load and ensure EPSG:4326 ---
v <- vect(here("data/input/wfunctions_NEW_full_raw.gpkg"))

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

out_file <- here::here("data/input/wfunctions_masked.gpkg")
writeVector(v, out_file, overwrite = TRUE)

# Rename new columns
names(v)[names(v) == "porosity_mean"] <- "porosity"
names(v)[names(v) == "goveffindex_min"] <- "goveff"
names(v)[names(v) == "grossbiowaterprod_mean"] <- "biomasswprod"
names(v)[names(v) == "cropprodwfootprint_mean"] <- "cropwfoot"
names(v)[names(v) == "hdi_min"] <- "hdi"
names(v)[names(v) == "c0Yld_DIN_max"] <- "yield_DIN"
names(v)[names(v) == "c0Yld_DIP_max"] <- "yield_DIP"
names(v)[names(v) == "snw_pc_cyr_mean"] <- "snowcover"
names(v)[names(v) == "prm_pc_cse_mean"] <- "permafrost"
names(v)[names(v) == "slp_dg_cav_mean"] <- "slope"
names(v)[names(v) == "predprob1_mean"] <- "flowcessation"
names(v)[names(v) == "soilwaterstorcap_min"] <- "soilwstorcap"
names(v)[names(v) == "kWh_year_1_sum"] <- "hydropot"
names(v)[names(v) == "popdensity_mean"] <- "popdensity"
names(v)[names(v) == "udw_raw_max"] <- "udrinkw"
names(v)[names(v) == "rootzmoiststorcap_mean"] <- "rootzmoistcap"
names(v)[names(v) == "v2x_partipdem_min"] <- "participdem"
names(v)[names(v) == "dis_m3_pyr_mean"] <- "discharge"
names(v)[names(v) == "glc_pc_c16_mean"] <- "cultivland_pc"


writeVector(v, here("data/input/wfunctions_masked_renamed.gpkg"), overwrite = TRUE)

simplified_cols <- c(
  "porosity", "goveff", "biomasswprod", "cropwfoot", "hdi",
  "yield_DIN", "yield_DIP", "snowcover", "permafrost", "slope",
  "flowcessation", "soilwstorcap", "hydropot", "popdensity", "udrinkw",
  "rootzmoistcap", "participdem", "discharge", "cultivland%")

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
