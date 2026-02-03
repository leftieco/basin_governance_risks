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


library(terra)
library(dplyr)

# 1. Load the watershed polygons (with function metrics + HYBAS_ID + archetypeID already merged)
watersheds <- terra::vect(here("data/output/wfunctions_archetypes.gpkg"))

# 2. Convert to a data.frame
df <- as.data.frame(watersheds)

# 3. Make sure you have the archetype ID column
# If you don't yet, you might need to merge it first with watersheds_archetypes.csv

# --- Select only *_gr normalized columns for SOM ---
cols_gr   <- grep("_gr$", names(df), value = TRUE)


# 4. List of function metrics
function_vars <- grep("_gr$", names(df), value = TRUE)

# 5. Calculate mean values for each archetype
archetype_summary <- df %>%
  group_by(archetypeID) %>%
  reframe(across(all_of(function_vars), ~ mean(.x, na.rm = TRUE)))

print(archetype_summary)

# Save summary to CSV in output folder
output_path <- here::here("data/output/wfunctions_archetype_summary.csv")
write.csv(archetype_summary, output_path, row.names = FALSE)


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


#Plotting function

library(fmsb)
library(dplyr)
library(scales)
library(MetBrewer) # beautiful palettes
library(ggplot2)   # for saving plots if needed
library(here)

  
  # # Choose a beautiful palette (11 colors for 11 archetypes)
  # colors <- met.brewer("Hokusai1", n = 11)
  
  
  
  # # New color groups
  # color_groups <- c(
  #   "#FFD700", "#FFB000",             # Archetypes 1,2: Yellows
  #   "#D73027", "#DB93B0",             # Archetypes 3-4: Reds
  #   "#8B4513", "#F0AA78",             # Archetypes 5-6: Browns
  #   "#FFA000",                        # Archetypes 7: Yellows
  #   "#1B1F3B", "#4575B4",             # Archetypes 8: Blues
  #   "#1B9E77", "#9EC5AB"              # Archetypes 9-10: Greens
  #   
  # )

color_groups <- c(
  "#FFD700",             # Archetype 1: Yellows
  "#DB93B0",             # Archetypes 2: Reds
  "#FFA000",             # Archetypes 3: Browns
  "#1B1F3B",             # Archetypes 4: Blues
  "#1B9E77",
  "#FF0000" # Archetypes 5: Greens
)
  
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


#FOR SOM 

# 1. Load the SOM2 prototypes
prototypes <- readr::read_rds(here("data/som_files/som_selection/som2_selection.rds"))
prototypes_df <- prototypes$codes[[1]] |> as_tibble()

# 2. Add archetype IDs manually (1 to 11)
prototypes_df$archetypeID <- 1:nrow(prototypes_df)

# 3. Group by archetypeID and take mean (but actually each codebook vector already corresponds to an archetype)
# So you probably don't even need to group — each row = one archetype centroid

# 4. Final format
archetype_summary_centroids <- prototypes_df

print(archetype_summary_centroids)

# Now you can reuse the same plotting function!
plot_archetype_radar(archetype_summary_centroids, save_plots = TRUE, output_dir = here("plots/centroid_radars")) 


### For illustration

library(fmsb)
library(dplyr)
library(scales)
library(MetBrewer)

# 1. Full real names for each metric
full_metric_names <- c(
  "Near-surface porosity",                # porosity
  "Governance Effectiveness",              # gov_effectiveness
  "Unimproved Drinking Water Dependency",  # udw_dependence
  "Gross Biomass Water Productivity",      # blue_water_prod
  "Available Soil Water Storage Capacity", # water_capacity
  "Probability of Flow Intermittence",     # intermittent_flow
  "Human Development Index (HDI)",          # hdi
  "Combined Crop Water Footprint",         # crop_footprint
  "Nitrogen Retention",                    # n_yield
  "Phosphorus Retention",                  # p_yield
  "Hydropower Potential"                   # hydropower
)

# 2. Pick Watershed/Archetype 4
arch4 <- archetype_summary_centroids %>%
  as_tibble() %>%
  filter(archetypeID == 4) %>%
  select(-archetypeID)

# 3. Prepare radar chart data
radar_data <- rbind(
  rep(2, ncol(arch4)),    # Max scale (+2σ)
  rep(-2, ncol(arch4)),   # Min scale (-2σ)
  arch4
)

rownames(radar_data) <- c("Max", "Min", "Watershed 4")

# 4. Make radar plot
par(mar = c(4, 4, 4, 4))  # more space around
radarchart(radar_data,
           axistype = 1,
           vlabels = NA,     # Real full names
           pcol = "#D73027",
           pfcol = alpha("#D73027", 0.4),
           plwd = 6,
           cglcol = "grey50",
           cglty = 1,
           cglwd = 0.8,
           axislabcol = "black",
           seg = 2,
           #caxislabels = c("-1.5σ", NA, "+1.5σ"),
           caxislabels = c(NA, NA, NA),
           vlcex = 1         # Slightly smaller text so labels fit
           #title = "Functional Signature of Watershed 4"
)

