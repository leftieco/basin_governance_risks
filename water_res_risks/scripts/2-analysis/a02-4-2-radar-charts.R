
library(terra)
library(dplyr)

# 1. Load the watershed polygons (with function metrics + HYBAS_ID + archetypeID already merged)
watersheds <- terra::vect(here("data/output/water_res_risks_archetypes.gpkg"))

# 2. Convert to a data.frame
df <- as.data.frame(watersheds)

# 3. Make sure you have the archetype ID column
# If you don't yet, you might need to merge it first with watersheds_archetypes.csv

# --- Select only *_gr normalized columns for SOM ---
cols_z   <- grep("_z$", names(df), value = TRUE)


# 4. List of function metrics
function_vars <- grep("_z$", names(df), value = TRUE)

# 5. Calculate mean values for each archetype
archetype_summary <- df %>%
  group_by(archetypeID) %>%
  reframe(across(all_of(function_vars), ~ mean(.x, na.rm = TRUE)))

print(archetype_summary)

# Save summary to CSV in output folder
output_path <- here::here("data/output/water_res_risks_archetype_summary.csv")
write.csv(archetype_summary, output_path, row.names = FALSE)

#Plotting function

library(fmsb)
library(dplyr)
library(scales)
library(MetBrewer) # beautiful palettes
library(ggplot2)   # for saving plots if needed
library(here)

# colour_groups <- c(
#   "#FFD700",            
#   "#DB93B0",           
#   "#FFA000",           
#   "#1B1F3B",           
#   "#1B9E77",
#   "#FF0000" 
# )

colour_groups_24 <- c(
  "#FFD700",  # 1  gold
  "#DB93B0",  # 2  rose
  "#FFA000",  # 3  amber
  "#1B1F3B",  # 4  deep navy
  "#1B9E77",  # 5  teal
  "#FF0000",  # 6  red
  
  "#1F78B4",  # 7  blue
  "#33A02C",  # 8  green
  "#6A3D9A",  # 9  purple
  "#FF7F00",  # 10 orange
  "#E31A1C",  # 11 crimson
  "#B15928",  # 12 brown
  
  "#A6CEE3",  # 13 light blue
  "#B2DF8A",  # 14 light green
  "#CAB2D6",  # 15 lavender
  "#FB9A99",  # 16 salmon
  "#FDBF6F",  # 17 peach
  "#8DD3C7",  # 18 aqua
  
  "#80B1D3",  # 19 sky
  "#BC80BD",  # 20 violet
  "#B3DE69",  # 21 lime
  "#FDB462",  # 22 tan
  "#E7298A",  # 23 magenta
  "#666666"   # 24 grey
)


# Make sure archetype_summary is a tibble
archetype_summary <- as_tibble(archetype_summary)

# Loop through each archetype
plot_archetype_radar <- function(archetype_summary, save_plots = FALSE, output_dir = "plots/archetype_radars") {
  
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
               pcol = colour_groups_24[i],                
               pfcol = alpha(colour_groups_24[i], 0.6),   
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

# Save to folder
plot_archetype_radar(archetype_summary, save_plots = TRUE, output_dir = here("plots/archetype_radars")) 


#FOR SOM 

# 1. Load the SOM2 prototypes
prototypes <- readr::read_rds(here("data/som_files/som_selection/som2_selection.rds"))
prototypes_df <- prototypes$codes[[1]] |> as_tibble()

# 2. Add archetype IDs manually 
prototypes_df$archetypeID <- 1:nrow(prototypes_df)

# 3. Group by archetypeID and take mean (but actually each codebook vector already corresponds to an archetype)

# 4. Final format
archetype_summary_centroids <- prototypes_df

print(archetype_summary_centroids)

plot_archetype_radar(archetype_summary_centroids, save_plots = TRUE, output_dir = here("plots/som2_centroid_radars")) 

