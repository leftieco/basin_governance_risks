### ---------------------\\ 
# Script objective:
# On a compute cluster, develop SOM models trained on full input data across range of grid sizes and write outputs to file 
### ---------------------\\ 


### COMMANDS FOR TERMINAL
# cd "/Users/petrvesnovskii/My Drive/R/watershed/water_res_risks"
# Rscript "scripts/2-analysis/a002-2-1-som1-iterations-fullspace.R"

suppressPackageStartupMessages({
  library(kohonen)
  library(aweSOM)
  library(clusterSim)
  library(readr)
  library(tibble)
  library(dplyr)
  library(parallel)
  library(doParallel)
  library(doRNG)
})

# Settings
base_seed <- 10000
# base_seed <- 123 #30.03.2026, 31.03.2026

input_file <- "data/input/02_full_input_data_norm.rds"
iter_file  <- "data/som_files/som_derivation_data/00_full_SOM_iteration_sizing_index.rds"

model_dir <- "data/som_files/som_files_full"
perf_dir  <- "data/som_files/som_performance_full"

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

# Load inputs
input_data <- readr::read_rds(input_file)
iter_index <- readr::read_rds(iter_file)

som_vars <- setdiff(names(input_data), "HYBAS_ID")

som_input <- as.matrix(input_data[, som_vars, drop = FALSE])

# Select iterations to run

input_iter_from_shell <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)))

if (length(input_iter_from_shell) == 0 || all(is.na(input_iter_from_shell))) {
  message("No iteration indices supplied; running all rows in iter_index.")
  run_rows <- seq_len(nrow(iter_index))
} else {
  run_rows <- input_iter_from_shell[!is.na(input_iter_from_shell)]
}

if (length(run_rows) == 0) {
  stop("No valid iteration indices supplied.")
}

run_df <- iter_index[run_rows, , drop = FALSE]

message("Iterations to run: ", nrow(run_df))
message("SOM sizes in this batch: ", paste(unique(run_df$size), collapse = ", "))

# Parallel setup
ncores <- max(1L, parallel::detectCores() - 1L)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)
doRNG::registerDoRNG(seed = base_seed)

message("Parallel workers: ", ncores)
message("Base RNG seed: ", base_seed)

# SOM training function
run_one_som <- function(i_row, run_df, som_input, model_dir, perf_dir) {
  som_size <- run_df$size[i_row]
  iter_no  <- run_df$size_iter[i_row]
  
  model_file <- file.path(
    model_dir,
    paste0("som1_nrc_", som_size, "_iter_", iter_no, ".rds")
  )
  
  perf_file <- file.path(
    perf_dir,
    paste0("som1_nrc_", som_size, "_iter_", iter_no, ".rds")
  )
  
  som_model <- kohonen::supersom(
    data = som_input,
    grid = kohonen::somgrid(xdim = som_size, ydim = som_size, topo = "hexagonal"),
    rlen = 500,
    alpha = c(0.05, 0.01),
    keep.data = TRUE
  )
  
  readr::write_rds(som_model, model_file)
  
  som_quality <- aweSOM::somQuality(som = som_model, traindat = som_input)
  cluster_quality <- clusterSim::index.DB(x = som_input, cl = som_model$unit.classif)
  
  perf_tbl <- tibble::tibble(
    quant    = round(as.numeric(som_quality$err.quant[1]), 5),
    varra    = round(as.numeric(som_quality$err.varratio[1]), 5),
    k_l      = round(as.numeric(som_quality$err.kaski[1]), 5),
    topo     = round(as.numeric(som_quality$err.topo[1]), 5),
    db_x     = round(as.numeric(cluster_quality$DB[1]), 5),
    som_size = som_size,
    som_iter = iter_no
  )
  
  readr::write_rds(perf_tbl, perf_file)
  
  tibble::tibble(
    som_size = som_size,
    som_iter = iter_no,
    model_file = model_file,
    perf_file = perf_file,
    quant = perf_tbl$quant,
    topo = perf_tbl$topo,
    k_l = perf_tbl$k_l,
    db_x = perf_tbl$db_x
  )
}


# Run in parallel
results <- foreach::foreach(
  i = seq_len(nrow(run_df)),
  .packages = c("kohonen", "aweSOM", "clusterSim", "readr", "tibble"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
  run_one_som(
    i_row = i,
    run_df = run_df,
    som_input = som_input,
    model_dir = model_dir,
    perf_dir = perf_dir
  )
}

# Disable cluster
parallel::stopCluster(cl)


# Report
if (inherits(results, "error")) {
  stop("Parallel run failed.")
}
if ("simpleError" %in% class(results)) {
  stop(results$message)
}

n_success <- sum(!vapply(results, inherits, logical(1), what = "error"))
message("Completed. Successful runs recorded: ", nrow(results))


message("All requested SOM runs completed.")


# # Libraries
# suppressPackageStartupMessages({
#   library(kohonen)
#   library(aweSOM)
#   library(clusterSim)
#   library(readr)
#   library(tibble)
#   library(parallel)     # <-- THIS is critical
#   library(doParallel)
# })
# 
# # input data
# input_data = readr::read_rds("data/input/02_full_input_data_norm.rds")
# 
# # iteration index
# iter_index = readr::read_rds("data/som_files/som_derivation_data/00_full_SOM_iteration_sizing_index.rds")
# 
# input_iter_from_shell <- as.numeric(commandArgs(trailingOnly = TRUE))
# if (length(input_iter_from_shell) == 0) {
#   message("⚠️  No iteration index supplied — running full set in parallel.")
#   input_iter_from_shell <- 1:nrow(iter_index)
# }
# 
# if (any(input_iter_from_shell > nrow(iter_index))) {
#   stop("One or more iteration indices exceed available rows in iter_index.")
# }
# 
# # -----------------------
# # Parallel setup
# # -----------------------
# ncores <- max(1, detectCores() - 1)
# cl <- makeCluster(ncores)
# registerDoParallel(cl)
# 
# cl
# length(cl)
# parallel:::clusterCall(cl, function() Sys.getpid())
# 
# 
# # -----------------------
# # Parallel loop
# # -----------------------
# foreach(i = input_iter_from_shell,
#         .packages = c("kohonen","aweSOM","clusterSim","readr","tibble")) %dopar% {
#           
#           som_input <- as.matrix(input_data[ , !names(input_data) %in% "HYBAS_ID"])
#           som_size  <- iter_index$size[i]
#           iter_no   <- iter_index$size_iter[i]
#           
#           # Deterministic per-task seed 
#           # set.seed(100000 + i)
#           
#           cat("Running SOM", som_size, "x", som_size, "iteration", iter_no, "...\n")
#           
#           # Train SOM
#           som_iter <- kohonen::supersom(
#             som_input,
#             grid = somgrid(xdim = som_size, ydim = som_size, topo = "hexagonal"),
#             rlen = 500,
#             alpha = c(0.05, 0.01),
#             keep.data = TRUE
#           )
#           
#           # Save SOM model
#           model_file <- paste0("data/som_files/som_files_full/som1_nrc_", som_size, "_iter_", iter_no, ".rds")
#           dir.create(dirname(model_file), recursive = TRUE, showWarnings = FALSE)
#           readr::write_rds(som_iter, model_file)
#           
#           # Evaluate SOM quality
#           som_quality <- aweSOM::somQuality(som = som_iter, traindat = som_input)
#           cluster_quality <- clusterSim::index.DB(x = som_input, cl = som_iter$unit.classif)
#           
#           # Build performance table
#           som_quality_df <- tibble(
#             quant = som_quality$err.quant[1] |> as.numeric() |> round(5),
#             varra = som_quality$err.varratio[1] |> as.numeric() |> round(5),
#             k_l   = som_quality$err.kaski[1] |> as.numeric() |> round(5),
#             topo  = som_quality$err.topo[1] |> as.numeric() |> round(5),
#             db_x  = cluster_quality$DB[1] |> as.numeric() |> round(5),
#             som_size = som_size,
#             som_iter = iter_no
#           )
#           
#           # Save performance metrics
#           perf_file <- paste0("data/som_files/som_performance_full/som1_nrc_", som_size, "_iter_", iter_no, ".rds")
#           dir.create(dirname(perf_file), recursive = TRUE, showWarnings = FALSE)
#           readr::write_rds(som_quality_df, perf_file)
#           
#           NULL
#         }
# 
# # -----------------------
# # Stop cluster
# # -----------------------
# stopCluster(cl)
# message("✅ All SOM runs completed and saved.")


