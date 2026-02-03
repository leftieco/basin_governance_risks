### ---------------------\\ 
# Script objective:
# On a compute cluster, develop SOM models across range of grid sizes and write outputs to file
### ---------------------\\

library(kohonen)
library(aweSOM)
library(clusterSim)
library(readr)
library(tibble)

library(parallel)
library(doParallel)
library(foreach)

# Load inputs
input_data <- read_rds("data/input/01_synthetic_input_data.rds")
iter_index <- read_rds("data/som_files/som_derivation_data/00_synthetic_SOM_iteration_sizing_index.rds")
som_input <- as.matrix(input_data)

# input from command line
input_iter_from_shell = as.numeric(commandArgs(trailingOnly = TRUE))

# Create output dirs if not exist
dir.create("som_files_synthetic", showWarnings = FALSE)
dir.create("som_performance_synthetic", showWarnings = FALSE)

# --- parallel setup ---
n_workers <- max(1L, parallel::detectCores() - 1L)   # leave 1 core free
cl <- parallel::makeCluster(n_workers)
doParallel::registerDoParallel(cl)

# --- run in parallel ---
foreach(i = seq_len(nrow(iter_index)),
        .packages = c("kohonen", "aweSOM", "clusterSim", "readr", "tibble")
) %dopar% {
  
  som_size <- iter_index$size[i]
  iter_no  <- iter_index$size_iter[i]
  
  # Deterministic per-task seed
  set.seed(100000 + i)
  
  som_model <- kohonen::supersom(
    som_input,
    grid = kohonen::somgrid(xdim = som_size, ydim = som_size, topo = "hexagonal"),
    rlen = 500,
    alpha = c(0.05, 0.01),
    keep.data = TRUE
  )
  
  model_file <- sprintf(
    "data/som_files/som1_files_synthetic/som1_nrc_%dx%d_iter_%d.rds",
    som_size, som_size, iter_no
  )
  readr::write_rds(som_model, model_file)
  
  som_quality <- aweSOM::somQuality(som_model, traindat = som_input)
  cluster_quality <- clusterSim::index.DB(x = som_input, cl = som_model$unit.classif)
  
  som_quality_df <- tibble::tibble(
    quant = round(som_quality$err.quant[1], 5),
    varra = round(som_quality$err.varratio[1], 5),
    k_l   = round(som_quality$err.kaski[1], 5),
    topo  = round(som_quality$err.topo[1], 5),
    db_x  = round(cluster_quality$DB[1], 5),
    som_size = som_size,
    som_iter = iter_no
  )
  
  perf_file <- sprintf(
    "data/som_files/som1_performance_synthetic/som1_nrc_%dx%d_iter_%d.rds",
    som_size, som_size, iter_no
  )
  readr::write_rds(som_quality_df, perf_file)
  
  # return something small (optional)
  c(i = i, som_size = som_size, iter_no = iter_no)
}

# --- cleanup ---
parallel::stopCluster(cl)

