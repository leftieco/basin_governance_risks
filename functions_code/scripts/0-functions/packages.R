### ---------------------\\ 
# Script objective:
# Import packages needed for workflow 
### ---------------------\\ 

#install packages

install_and_load <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
  lapply(pkgs, library, character.only = TRUE)
}

install_and_load(c(
  # general
  "tidyverse", "magrittr",
  
  # stats / clustering
  "Hmisc", "NbClust", "clValid", "clv", "kohonen", "factoextra", "reldist",
  "BAMMtools", "classInt", "parallelDist", "distances", "Rfast", "pdist",
  "overlapping", "corrplot", "cluster", "aweSOM", "clusterSim",
  
  # spatial
  "raster", "terra", "sf", "ncdf4", "rasterDT", "gdalUtilities",
  "whitebox", "elsa", "rasterdiv", "landscapemetrics", "rnaturalearthhires",
  # "rgeos", "rgdal",
  
  # plotting
  "scico", "MetBrewer", "viridisLite", "RColorBrewer", "scales", "ggridges",
  "cowplot", "gridExtra", "ggcorrplot", "GGally", "ggalluvial", "fmsb",
  
  # spatial plotting
  "rnaturalearth", "tmaptools", "tmap"
))

# general
library(tidyverse)
library(magrittr)

# stats / clustering
library(Hmisc)
library(NbClust)
library(clValid)
library(clv)
library(kohonen)
library(factoextra)
library(reldist)
library(BAMMtools)
library(classInt)
library(parallelDist)
library(distances)
library(Rfast)
library(pdist)
library(overlapping)
library(corrplot)
library(cluster)
library(aweSOM)
library(clusterSim)

# spatial
library(raster) 
library(terra) 
library(sf)
library(ncdf4)
library(rasterDT)
#library(rgeos)
#library(rgdal)
library(gdalUtilities)
library(whitebox)
library(rasterDT)
library(elsa)
library(rasterdiv)
library(landscapemetrics)
library(rnaturalearthhires)

# plotting
library(scico) 
library(MetBrewer)
library(viridisLite)
library(RColorBrewer)
library(scales)
library(ggridges)
library(cowplot)
library(gridExtra)
library(ggcorrplot)
library(GGally)
library(ggalluvial)
library(fmsb)

# spatial plotting
library(rnaturalearth)
library(tmaptools)
library(tmap)