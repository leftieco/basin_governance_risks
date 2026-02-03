### ---------------------\\ 
# Script objective:
# Develop second-stage SOM models on output codebook vectors from selected first-stage SOM model 
# and write each model and its performance to file
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

## --------------------------- \
# import best-performing first-stage SOM and extract codebook vectors
prototypes = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_37.rds"))
prototypes = prototypes$codes[[1]] |> as.data.frame()

# set range of second-stage SOM following range suggested by Eisenack et al. 2021
r_min = 2
r_max = 30

# convert prototypes to matrix to feed to som function
som_input = prototypes |> as.matrix()

## --------------------------- \
# Loop through second-stage SOM iterations

for (n_clust in seq(r_min, r_max, by = 1)) {
  # n_clust = 4
  message("starting SOM size ", n_clust)
  
  # data frame to store SOM quality metrics for each lattice size
  quality_df = data.frame(
    iter = seq(1:120),  # iterations per SOM lattice
    quant = rep(NA),    # all other parts of this df as same as in first-stage SOM 
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    db_x = rep(NA),
    nrc = rep(n_clust)
  )
  
  # loop through each SOM iteration per lattice size
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1
    
    # create the SOM 
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = n_clust, ydim = 1, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = 8)
    
    ## write out SOM data to recall in case the iteration is thebest performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som_files/som2_files/som2_nclust_"), n_clust, "_iter_", i, ".rds")) 
    
    # calculate SOM quality metrics
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    cluster_quality = clusterSim::index.DB(x = som_input, cl = som_iter$unit.classif)
    
    # assign SOM quality metrics to data frame  
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(5)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(5)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(5)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(5)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(5)
    quality_df$db_x[i] = cluster_quality$DB[1] |> as.numeric() |> round(5)
    message("... ", i, "/", n_clust)
  }
  
  # write the full SOM quality data frame to file for the SOM lattice size
  write_rds(x = quality_df,
            file = paste0(here("data/som_files/som2_performance/som2_nclust_"), n_clust, ".rds"))
  message("SOM iterations for nclust=", n_clust, " has completed")
  
}