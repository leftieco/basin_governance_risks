### ---------------------\\ 
# Script objective:
# On a compute cluster, develop SOM models trained on full input data across range of grid sizes and write outputs to file 
### ---------------------\\ 

# Libraries
suppressPackageStartupMessages({
  library(kohonen)
  library(aweSOM)
  library(clusterSim)
  library(readr)
  library(tibble)
  library(parallel)     # <-- THIS is critical
  library(doParallel)
})

# input data
input_data = readr::read_rds("data/02_full_input_data_norm.rds")

# iteration index
iter_index = readr::read_rds("data/som_files/som_derivation_data/00_full_SOM_iteration_sizing_index.rds")

input_iter_from_shell <- as.numeric(commandArgs(trailingOnly = TRUE))
if (length(input_iter_from_shell) == 0) {
  message("⚠️  No iteration index supplied — running full set in parallel.")
  input_iter_from_shell <- 1:nrow(iter_index)
}

if (any(input_iter_from_shell > nrow(iter_index))) {
  stop("One or more iteration indices exceed available rows in iter_index.")
}

# -----------------------
# Parallel setup
# -----------------------
ncores <- max(1, detectCores() - 1)
cl <- makeCluster(ncores)
registerDoParallel(cl)

cl
length(cl)
parallel:::clusterCall(cl, function() Sys.getpid())


# -----------------------
# Parallel loop
# -----------------------
foreach(i = input_iter_from_shell,
        .packages = c("kohonen","aweSOM","clusterSim","readr","tibble")) %dopar% {
          
          som_input <- as.matrix(input_data)
          som_size  <- iter_index$size[i]
          iter_no   <- iter_index$size_iter[i]
          
          cat("Running SOM", som_size, "x", som_size, "iteration", iter_no, "...\n")
          
          # Train SOM
          som_iter <- kohonen::supersom(
            som_input,
            grid = somgrid(xdim = som_size, ydim = som_size, topo = "hexagonal"),
            rlen = 500,
            alpha = c(0.05, 0.01),
            keep.data = TRUE
          )
          
          # Save SOM model
          model_file <- paste0("data/som_files/som_files_full/som1_nrc_", som_size, "_iter_", iter_no, ".rds")
          dir.create(dirname(model_file), recursive = TRUE, showWarnings = FALSE)
          readr::write_rds(som_iter, model_file)
          
          # Evaluate SOM quality
          som_quality <- aweSOM::somQuality(som = som_iter, traindat = som_input)
          cluster_quality <- clusterSim::index.DB(x = som_input, cl = som_iter$unit.classif)
          
          # Build performance table
          som_quality_df <- tibble(
            quant = som_quality$err.quant[1] |> as.numeric() |> round(5),
            varra = som_quality$err.varratio[1] |> as.numeric() |> round(5),
            k_l   = som_quality$err.kaski[1] |> as.numeric() |> round(5),
            topo  = som_quality$err.topo[1] |> as.numeric() |> round(5),
            db_x  = cluster_quality$DB[1] |> as.numeric() |> round(5),
            som_size = som_size,
            som_iter = iter_no
          )
          
          # Save performance metrics
          perf_file <- paste0("data/som_files/som_performance_full/som1_nrc_", som_size, "_iter_", iter_no, ".rds")
          dir.create(dirname(perf_file), recursive = TRUE, showWarnings = FALSE)
          readr::write_rds(som_quality_df, perf_file)
          
          NULL
        }

# -----------------------
# Stop cluster
# -----------------------
stopCluster(cl)
message("✅ All SOM runs completed and saved.")

# # input from command line
# input_iter_from_shell = as.numeric(commandArgs(trailingOnly = TRUE))
# 
# # now create the SOM for the given iteration
# som_input = input_data |>  as.matrix()
# som_size = iter_index$size[input_iter_from_shell]
# iter_no = iter_index$size_iter[input_iter_from_shell]
# 
# som_iter = kohonen::supersom(som_input, 
#                              grid = somgrid(xdim = som_size, 
#                                             ydim = som_size, 
#                                             topo="hexagonal"),
#                              rlen = 500, 
#                              alpha = c(0.05, 0.01),
#                              keep.data = TRUE)
# 
# write_rds(x = som_iter,
#           file = paste0("./k))
# 
# som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
# cluster_quality = clusterSim::index.DB(x = som_input, cl = som_iter$unit.classif)
# 
# som_quality_df = tibble(
#   quant = som_quality$err.quant[1] |> as.numeric() |> round(5),
#   varra = som_quality$err.varratio[1] |> as.numeric() |> round(5),
#   k_l   = som_quality$err.kaski[1] |> as.numeric() |> round(5),
#   topo  = som_quality$err.topo[1] |> as.numeric() |> round(5),
#   db_x  = cluster_quality$DB[1] |> as.numeric() |> round(5),
#   som_size = som_size,
#   som_iter = iter_no
# )
# 
# write_rds(x = som_quality_df,
#           file = paste0("data/som_files/som_performance_full/som1_nrc_", som_size, "_iter_", iter_no, ".rds"))
