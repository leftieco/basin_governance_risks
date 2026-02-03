### ---------------------\\ 
# Script objective:
# Map second-stage SOM clusters back to geographic space
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# load classifications
som2_prot_to_arch = readr::read_rds(here("data/som_files/som_selections/som2_selection.rds"))
som1_anch_to_prot = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_37.rds"))
full_data_df = readr::read_rds(here("data/input_features_full_set_norm.rds"))

# create prototype to archetype dictionary
prot_to_arch_dict = data.frame(
  archetypeID = som2_prot_to_arch$unit.classif,
  prototypeID = seq(1, length(som2_prot_to_arch$unit.classif))
)

# create synthetic anchor point to prototype dictionary
cell_to_prot_dict = data.frame(
  prototypeID =  som1_anch_to_prot$unit.classif,
  cellID = full_data_df$id
)

# # combine archetype with prototype 
# vertical_dictionary = merge(x = prot_to_arch_dict,
#                             y = anch_to_prot_dict,
#                             by.x = "prototypeID",
#                             by.y = "prototypeID")
# 
# # link cell IDs with their synthetic anchor point (derived using kmeans)
# main_reclass_dictionary = data.frame(cellID      = full_data_df$id,
#                                      anchorpt_ID = cell_to_anch$cluster)

main_reclass_dictionary = merge(x = cell_to_prot_dict,
                                y = prot_to_arch_dict,
                                by.x = "prototypeID",
                                by.y = "prototypeID")


## 
####### RECLASSIFY TO MAPS ----------------------------------\
##

# now reclassify grid cell ID raster to prototypes, archetypes, and mask 
grid_id_raster = terra::rast(here("data/id_rast.tif"))

prototypes_map = rasterDT::subsDT(x = raster(grid_id_raster$id),
                                  dict = data.frame(from = main_reclass_dictionary$cellID,
                                                    to   = main_reclass_dictionary$prototypeID),
                                  filename = here("data/groundwaterscapes-prototypes_currentiter.tif"),
                                  overwrite = TRUE)

archetypes_map = rasterDT::subsDT(x = raster(grid_id_raster$id),
                                  dict = data.frame(from = main_reclass_dictionary$cellID,
                                                    to   = main_reclass_dictionary$archetypeID), 
                                  filename = here("data/groundwaterscapes-currentiter.tif"),
                                  overwrite = TRUE)

groundwaterscapes_for_Borealis = terra::rast(archetypes_map)
names(groundwaterscapes_for_Borealis) = "GWscape_ID"

writeRaster(x = groundwaterscapes_for_Borealis,
            filename = here("data/groundwaterscapes-NoMetaData.tif"),
            overwrite = T)

# Now create groundwaterscape descriptions csv file





# # perform 3x3 modal smoothing over archetypes
# archetypes_map_3x3 = terra::focal(x = rast(archetypes_map), w = 3, fun = "modal", expand = FALSE, na.rm = T,
#                                   filename = here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"),
#                                   overwrite = T)
# 
# archetypes_map_3x3[is.na(archetypes_map |> rast())] = NA
# writeRaster(x = archetypes_map_3x3,
#             filename = here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"),
#             overwrite = T)