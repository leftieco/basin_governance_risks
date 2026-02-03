### ---------------------\\ 
# Script objective:
# Create a synthetic representation of input data using k-means cluster centers
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

library(terra)
library(dplyr)
library(cluster)   # For clara() if needed
library(stats)     # For kmeans()
library(here)
library(readr)

library(doParallel)
library(foreach)
library(doRNG)       # reproducible RNG across workers


# 1. Load vector and extract attributes
v <- st_read(here("data/input/water_res_risks_gr_ready.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# --- Select only *_z normalized columns for SOM ---
cols_gr   <- grep("_gr$", names(df), value = TRUE)

# # drop corruption_z if present
# cols_z   <- setdiff(cols_z, "corruption_z")

input_df <- df[, cols_gr, drop = FALSE]


# 3. Run K-means clustering to generate anchors
# Set k (number of clusters/anchors)
set.seed(123)
k <- round(0.05 * nrow(input_df))  # 5% of sample size

kmeans_model <- kmeans(input_df, centers = k, nstart = 20, iter.max = 50) #iter.max = 100, nstart = 20

# 1) Variance explained (R^2) for base::kmeans result
round(kmeans_model$betweenss / kmeans_model$totss, 3) # 0.935

# 2) Fraction of data kept as anchors
k / nrow(input_df)


round(kmeans_model$betweenss/kmeans_model$totss, 2) # ~99% - but it's 0.94 
k/nrow(input_df) # 0.5% of data size

# 4. Extract the cluster centers = synthetic anchors
anchors <- as.data.frame(kmeans_model$centers)

# 5. Save anchor matrix for use in SOM training
write_rds(anchors, here("data/som_files/01_synthetic_input_data.rds"))


# #Let's try parallel
# library(ClusterR)
# 
# X <- as.matrix(input_df)
# set.seed(123)
# 
# choose_k <- function(X, cand_k, target = 0.99) {
#   res <- lapply(cand_k, function(kk) {
#     fit <- KMeans_rcpp(X, clusters = kk, num_init = 25, max_iters = 100,
#                        initializer = "kmeans++", seed = 123, verbose = FALSE)
#     totss <- sum(scale(X, scale = FALSE)^2)
#     wcss  <- fit$total_SSE
#     data.frame(k = kk, var_exp = 1 - wcss / totss)
#   })
#   do.call(rbind, res)
# }
# 
# cand_k <- c(800, 1200, 1600, 2000, 2500, 3000)
# tab <- choose_k(X, cand_k, target = 0.99)
# tab
# tab[which(tab$var_exp >= 0.99)[1], ]

