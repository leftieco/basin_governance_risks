### ---------------------\\ 
# Script objective:
# On a compute cluster, develop SOM models across range of grid sizes and write outputs to file
### ---------------------\\

library(kohonen)
library(aweSOM)
library(clusterSim)
library(readr)
library(tibble)

# Load inputs
input_data <- read_rds("data/som_files/01_synthetic_input_data.rds")
iter_index <- read_rds("data/som_files/som_derivation_data/00_synthetic_SOM_iteration_sizing_index.rds")
som_input <- as.matrix(input_data)

# input from command line
input_iter_from_shell = as.numeric(commandArgs(trailingOnly = TRUE))

# Create output dirs if not exist
dir.create("som_files_synthetic", showWarnings = FALSE)
dir.create("som_performance_synthetic", showWarnings = FALSE)

# Loop over all rows in the iteration index
for (i in 1:nrow(iter_index)) {
  som_size <- iter_index$size[i]
  iter_no  <- iter_index$size_iter[i]
  
  cat("Running SOM", som_size, "x", som_size, "iteration", iter_no, "...\n")
  
  # Train SOM
  som_model <- kohonen::supersom(som_input,
                                 grid = somgrid(xdim = som_size, ydim = som_size, topo = "hexagonal"),
                                 rlen = 500,  # or: iter_index$rlen[i] if you want variable lengths
                                 alpha = c(0.05, 0.01),
                                 keep.data = TRUE)
  
  # Save model
  model_file <- sprintf("data/som_files/som_files_synthetic/som1_nrc_%dx%d_iter_%d.rds", som_size, som_size, iter_no)
  write_rds(som_model, model_file)
  
  # Evaluate
  som_quality <- aweSOM::somQuality(som_model, traindat = som_input)
  cluster_quality <- clusterSim::index.DB(x = som_input, cl = som_model$unit.classif)
  
  # Save quality summary
  som_quality_df <- tibble(
    quant = round(som_quality$err.quant[1], 5),
    varra = round(som_quality$err.varratio[1], 5),
    k_l   = round(som_quality$err.kaski[1], 5),
    topo  = round(som_quality$err.topo[1], 5),
    db_x  = round(cluster_quality$DB[1], 5),
    som_size = som_size,
    som_iter = iter_no
  )
  
  perf_file <- sprintf("data/som_files/som_performance_synthetic/som1_nrc_%dx%d_iter_%d.rds", som_size, som_size, iter_no)
  write_rds(som_quality_df, perf_file)
}

