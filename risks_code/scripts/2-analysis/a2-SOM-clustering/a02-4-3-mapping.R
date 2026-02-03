# library(terra)
# ### ---------------------\\ 
# # Script objective:
# # Map second-stage SOM clusters back to geographic space
# ### ---------------------\\
# library(here); source(here(("scripts/on_button.R")))
# ###
# 
# # load classifications
# som2_prot_to_arch = readr::read_rds(here("data/som_files/som_selections/som2_selection.rds"))
# som1_anch_to_prot = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_.rds"))
# full_data_df = readr::read_rds(here("data/input_features_full_set_norm.rds"))
# 
# # create prototype to archetype dictionary
# prot_to_arch_dict = data.frame(
#   archetypeID = som2_prot_to_arch$unit.classif,
#   prototypeID = seq(1, length(som2_prot_to_arch$unit.classif))
# )
# 
# # create synthetic anchor point to prototype dictionary
# cell_to_prot_dict = data.frame(
#   prototypeID =  som1_anch_to_prot$unit.classif,
#   cellID = full_data_df$id
# )
# 
# # # combine archetype with prototype
# # vertical_dictionary = merge(x = prot_to_arch_dict,
# #                             y = anch_to_prot_dict,
# #                             by.x = "prototypeID",
# #                             by.y = "prototypeID")
# #
# # # link cell IDs with their synthetic anchor point (derived using kmeans)
# # main_reclass_dictionary = data.frame(cellID      = full_data_df$id,
# #                                      anchorpt_ID = cell_to_anch$cluster)
# 
# main_reclass_dictionary = merge(x = cell_to_prot_dict,
#                                 y = prot_to_arch_dict,
#                                 by.x = "prototypeID",
#                                 by.y = "prototypeID")
# 
# 
# ##
# ####### RECLASSIFY TO MAPS ----------------------------------\
# ##
# 
# # now reclassify grid cell ID raster to prototypes, archetypes, and mask
# grid_id_raster = terra::rast(here("data/id_rast.tif"))
# 
# prototypes_map = rasterDT::subsDT(x = raster(grid_id_raster$id),
#                                   dict = data.frame(from = main_reclass_dictionary$cellID,
#                                                     to   = main_reclass_dictionary$prototypeID),
#                                   filename = here("data/groundwaterscapes-prototypes_currentiter.tif"),
#                                   overwrite = TRUE)
# 
# archetypes_map = rasterDT::subsDT(x = raster(grid_id_raster$id),
#                                   dict = data.frame(from = main_reclass_dictionary$cellID,
#                                                     to   = main_reclass_dictionary$archetypeID),
#                                   filename = here("data/groundwaterscapes-currentiter.tif"),
#                                   overwrite = TRUE)
# 
# groundwaterscapes_for_Borealis = terra::rast(archetypes_map)
# names(groundwaterscapes_for_Borealis) = "GWscape_ID"
# 
# writeRaster(x = groundwaterscapes_for_Borealis,
#             filename = here("data/groundwaterscapes-NoMetaData.tif"),
#             overwrite = T)
# 
# # Now create groundwaterscape descriptions csv file
# 
# 
# 
# 
# 
# # # perform 3x3 modal smoothing over archetypes
# # archetypes_map_3x3 = terra::focal(x = rast(archetypes_map), w = 3, fun = "modal", expand = FALSE, na.rm = T,
# #                                   filename = here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"),
# #                                   overwrite = T)
# #
# # archetypes_map_3x3[is.na(archetypes_map |> rast())] = NA
# # writeRaster(x = archetypes_map_3x3,
# #             filename = here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"),
# #             overwrite = T)

library(here)
library(terra)
library(dplyr)

# 1. Load the watershed polygons (with function metrics + HYBAS_ID + archetypeID already merged)
archetypes <- terra::vect(here("data/output/water_res_risk_archetypes.gpkg"))

# 2. Convert to a data.frame
df <- as.data.frame(archetypes)

# 3. Make sure you have the archetype ID column
# If you don't yet, you might need to merge it first with watersheds_archetypes.csv

# 4. List of function metrics
function_vars <- c(
  "runoff_nonstat_gr",
  "tws_nonstat_gr",
  "corruption_gr",
  "gini_gr",
  "wwthdr_pc_gr",
  "Base_water_stress_gr",
  "hydropol_int_gr",
  "thrspecies_gr",
  "bod_gr",
  "wetlandloss_gr",
  "connectedness_gr",
  "e_flow_gr",
  "mismplastic_gr",
  "droughthaz_gr",
  "extremes_damage_gr",
  "extremes_affected_gr",
  "equity_index_gr"
)

# 5. Calculate mean values for each archetype
archetype_summary <- df %>%
  group_by(archetypeID) %>%
  reframe(across(all_of(function_vars), ~ mean(.x, na.rm = TRUE)))

print(archetype_summary)

# Save summary to CSV in output folder
output_path <- here::here("data/output/archetype_summary.csv")
write.csv(archetype_summary, output_path, row.names = FALSE)

cat("✅ Saved archetype summary to:", output_path, "\n")

### Colour scheme for achetypes

watersheds <- terra::vect(here::here("data/output/water_res_risk_26archetypes_5cluster.gpkg"))
df <- as.data.frame(watersheds)

# ------------------------------
# 2. Compute Governance–Ecology Index (GEI)
# ------------------------------
df <- df %>%
  mutate(
    GEI = ((corruption_gr + equity_index_gr) -
             (Base_water_stress_gr + e_flow_gr +
                wetlandloss_gr + bod_gr) / 4) / 2,
    GEI_scaled = rescale(GEI, to = c(0.3, 1))
  )

# ------------------------------
# 3. Define base colours by cluster (resilience logic)
# ------------------------------
base_colours <- c(
  "1" = "#1f78b4",  # Cluster 1 – Persistent (cool blue)
  "2" = "#e66101",  # Cluster 2 – Transformative (orange-red)
  "3" = "#4daf4a",  # Cluster 3 – Adaptive (green)
  "4" = "#ffd92f",  # Cluster 4 – Persistent–Adaptive (yellow-gold)
  "5" = "#984ea3"   # Cluster 5 – Persistent (cohesive purple)
)
# 
# 🌊 Cluster 1 – Persistent (Hydro-climatic volatility)
# Cool blues suggesting stability and absorption of stress
# "#08306B""#08519C""#2171B5""#4292C6""#6BAED6"
# 🔥 Cluster 2 – Transformative (Engineered dependence)
# Warm oranges-to-reds for high modification and pressure
# 
# "#7F2704" "#A63603" "#D94801" "#F16913" "#FD8D3C""#FDAE6B""#FDD0A2""#FFE5B5""#FFC77F""#FF9F33"
# 🌿 Cluster 3 – Adaptive (Agrarian adaptive systems)
# Balanced greens for renewal, learning, and ecological flexibility
# "#00441B""#006D2C""#238B45""#41AB5D""#74C476" "#A1D99B" "#C7E9C0"
# 🌾 Cluster 4 – Persistent–Adaptive (Arid stability)
# Distinct gold-ochre tone — single but prominent
# "#FFD92F"
# 💜 Cluster 5 – Persistent (Cohesive resilience)
# Rich purples for socially cohesive, low-risk basins
# "#3F007D""#6A51A3""#807DBA"

# ------------------------------
# 4. Apply intra-cluster lightness scaling
# ------------------------------
# make sure the package is installed
install.packages("colorspace")
library(colorspace)

df <- df %>%
  mutate(
    cluster5 = as.character(cluster5),
    colour = colorspace::lighten(base_colours[cluster5], amount = 1 - GEI_scaled)
  )

# ------------------------------
# 5. Attach colours back to spatial object
# ------------------------------
watersheds$colour <- df$colour[match(watersheds$archetypeID, df$archetypeID)]

# Convert to sf for ggplot
watersheds_sf <- sf::st_as_sf(watersheds)

# ------------------------------
# 6. Plot the coloured archetype map
# ------------------------------
ggplot(watersheds_sf) +
  geom_sf(aes(fill = I(colour)), color = "NA", size = 0) +
  scale_fill_identity() +
  theme_minimal() +
  labs(
    title = "Water Resilience Risk Archetypes (26 types, 5 clusters)",
    subtitle = "Hue = cluster type; brightness = governance–ecology balance (GEI)",
    caption = "Blue = volatile–persistent | Orange = transformative | Green = adaptive | Teal = arid stable | Purple = cohesive"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    panel.grid = element_blank()
  )

ggsave(here("plots/archetypes_resilience_palette_map.png"), width = 9, height = 7, dpi = 300)


# library(fmsb)
# 
# # Plot nicely
# radarchart(radar_data,
#            axistype = 1,
#            pcol = "blue", pfcol = scales::alpha("blue", 0.3), plwd = 3,
#            cglcol = "grey40", cglty = 1, axislabcol = "black",
#            caxislabels = c("-2", "-1", "0", "+1", "+2"),
#            vlcex = 0.9,
#            title = "Archetype 1")    # Title directly
# 
# 
#Plotting function

library(fmsb)
library(dplyr)
library(scales)
library(MetBrewer) # beautiful palettes
library(ggplot2)   # for saving plots if needed
library(here)


  # Choose a beautiful palette (11 colors for 11 archetypes)
  colors <- met.brewer("Hokusai1", n = 11)

  # New color groups
  color_groups <- c(
    "#FFD700",   # Archetypes 1-3: Yellows
    "#D73027",            # Archetypes 4-5: Reds
    "#8B4513",          # Archetypes 6-7: Browns
    "#5E4FA2",             # Archetypes 8-9: Blues
    "#1B9E77")
  
  color_groups <- c(

    "#B2182B", 
    "#F4A300", "#FFB000",
    "#313695", "#4575B4", "#74ADD1",
    "#FFD700",
    "#ABD9E9",
    "#D6604D",
    "#FFE885", "#E1C451",
    "#E0F3F8",
    "#CBB742",
    "#C2A5CF", 
    "#C37226", "#A85D24", "#8C4C20",
    "#F3722C",
    "#006D2C", 
    "#3F00FF",
    "#31A354", 
    "#FFBBC7", 
    "#5E4FA2",
    "#74C476",
    "#F7A6C5",
    "#F7AD78")
    
    
  #   "#D6604D", "#F4A582", "#FDDBC7", "#F7A6C5",
  #   # Group II – Oranges / Yellows
    # "#F4A300", "#FFB000", "#FFD700", "#FFE885", "#E1C451", "#CBB742",
    # Group III – Blues / Purples
  #   "#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#5E4FA2",
  #   # Group IV – Yellowish / Brownish transition
  #   "#8B4513", "#B88504",
  #   # Group V – Greens
  #   "#00441B", "#006D2C", "#238B45", "#41AE76", "#66C2A4", "#99D8C9"
  #   
  #   color_groups <- c(
  #     # --- 5 Pink–Reds ---
  #     "#F94144", "#F3722C", "#F8961E", "#F9844A", "#F9C74F",
  #     
      # --- 10 Yellow–Orange–Browns ---
      # "#F7B538", "#E9A126", "#D98825", "#C37226", "#A85D24",
      # "#8C4C20", "#A97C50", "#C59E75", "#E0C092", "#F2DDB3",
  #     
  #     # --- 7 Blue–Purples ---
  #     "#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#C2A5CF", "#9970AB", "#762A83",
  #     
  #     # --- 3 Greens ---
  #     "#006D2C", "#31A354", "#74C476",
  #     
  #     # --- 1 Ultramarine ---
  #     "#3F00FF"
  #   )
  # )
  

  # Make sure archetype_summary is a tibble
  archetype_summary <- as_tibble(archetype_summary)

  # Loop through each archetype
plot_archetype_radar <- function(archetype_summary, save_plots = FALSE, output_dir = "plots/") {

  archetype_summary <- as_tibble(archetype_summary)

  for (i in sort(unique(archetype_summary$archetypeID))) {

    arch <- archetype_summary %>%
      dplyr::filter(archetypeID == i) %>%
      dplyr::select(-archetypeID)

    radar_data <- rbind(
      rep(2, ncol(arch)),    # Max
      rep(-2, ncol(arch)),   # Min
      arch
    )

    rownames(radar_data) <- c("Max", "Min", paste0("Archetype ", i))

    # Save as file if needed
    if (save_plots) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      png(filename = file.path(output_dir, paste0("archetype_", i, ".png")),
      #png(filename = file.path(output_dir, paste0("notitle_archetype_", i, ".png")),
          width = 600, height = 600, res = 150)

      #par(mar = c(1, 1, 1, 1))  # <-- THIS CONTROLS MARGINS (bottom, left, top, right)
       par(mar = c(1, 1, 2, 1))  # <-- THIS CONTROLS MARGINS (bottom, left, top (extra for title), right)
    }


    # Plot with empty variable names ("") to silence labels
    radarchart(radar_data,
               axistype = 1,
               pcol = color_groups[i],
               pfcol = alpha(color_groups[i], 0.6),
               plwd = 6,
               plty = 1,
               cglcol = "grey60",
               cglty = 1,
               cglwd = 0.8,
               axislabcol = "grey20",
               seg = 2,
               caxislabels = c(NA, NA, NA),
              # caxislabels = c(NA, "-1", NA, "+1.", NA),      # Only 2 labels
               vlcex = 1)                            # Silence variable names
              # vlabels = rep("", ncol(arch)))         # <- This is key: EMPTY vlabels
               #title = paste("Archetype", i))

    if (save_plots) {
      dev.off() 
    }
  }
}

# Just plot
plot_archetype_radar(archetype_summary)

# OR plot + save to folder
plot_archetype_radar(archetype_summary, save_plots = TRUE, output_dir = here("plots/"))

# 
# #FOR SOM 
# 
# # 1. Load the SOM2 prototypes
# prototypes <- readr::read_rds(here("data/som_files/som_selection/som2_selection_.rds"))
# prototypes_df <- prototypes$codes[[1]] |> as_tibble()
# 
# # 2. Add archetype IDs manually (1 to 26)
# prototypes_df$archetypeID <- 1:nrow(prototypes_df)
# 
# # 3. Group by archetypeID and take mean (but actually each codebook vector already corresponds to an archetype)
# # So you probably don't even need to group — each row = one archetype centroid
# 
# # 4. Final format
# archetype_summary_centroids <- prototypes_df
# 
# print(archetype_summary_centroids)
# 
# # Now you can reuse the same plotting function!
# plot_archetype_radar(archetype_summary_centroids, save_plots = TRUE, output_dir = here("plots/centroid_radars")) 
# 
# 
# ### For illustration
# 
# library(fmsb)
# library(dplyr)
# library(scales)
# library(MetBrewer)
# 
# # 1. Full real names for each metric
# full_metric_names <- c(
#   "Near-surface porosity",                # porosity
#   "Governance Effectiveness",              # gov_effectiveness
#   "Unimproved Drinking Water Dependency",  # udw_dependence
#   "Gross Biomass Water Productivity",      # blue_water_prod
#   "Available Soil Water Storage Capacity", # water_capacity
#   "Probability of Flow Intermittence",     # intermittent_flow
#   "Human Development Index (HDI)",          # hdi
#   "Combined Crop Water Footprint",         # crop_footprint
#   "Nitrogen Retention",                    # n_yield
#   "Phosphorus Retention",                  # p_yield
#   "Hydropower Potential"                   # hydropower
# )
# 
# # 2. Pick Watershed/Archetype 4
# arch4 <- archetype_summary_centroids %>%
#   as_tibble() %>%
#   filter(archetypeID == 4) %>%
#   select(-archetypeID)
# 
# # 3. Prepare radar chart data
# radar_data <- rbind(
#   rep(2, ncol(arch4)),    # Max scale (+2σ)
#   rep(-2, ncol(arch4)),   # Min scale (-2σ)
#   arch4
# )
# 
# rownames(radar_data) <- c("Max", "Min", "Watershed 4")
# 
# # 4. Make radar plot
# par(mar = c(4, 4, 4, 4))  # more space around
# radarchart(radar_data,
#            axistype = 1,
#            vlabels = NA,     # Real full names
#            pcol = "#D73027",
#            pfcol = alpha("#D73027", 0.4),
#            plwd = 6,
#            cglcol = "grey50",
#            cglty = 1,
#            cglwd = 0.8,
#            axislabcol = "black",
#            seg = 2,
#            #caxislabels = c("-1.5σ", NA, "+1.5σ"),
#            caxislabels = c(NA, NA, NA),
#            vlcex = 1         # Slightly smaller text so labels fit
#            #title = "Functional Signature of Watershed 4"
# )

