### ---------------------\\ 
# Create a synthetic representation of input data using k-means cluster centers (Huggins et al. 2024)
### ---------------------\\
library(here)
library(terra)
library(dplyr)
library(cluster)   # For clara() if needed
library(stats)     # For kmeans()
library(here)
library(readr)

# 1 Load vector and extract attributes
v <- st_read(here("data/input/water_res_risks_z_ready.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# 2 Select only *_z normalized columns for SOM ---
cols_z   <- grep("_z$", names(df), value = TRUE)
input_df <- df[, cols_z, drop = FALSE]

# 3 Run K-means clustering to generate anchors
# Set k (number of clusters/anchors)
set.seed(123)
k <- round(0.05 * nrow(input_df))  # 5% of sample size

kmeans_model <- kmeans(input_df, centers = k, nstart = 20, iter.max = 50) #iter.max = 100, nstart = 20

# 4 Variance explained (R^2) for base::kmeans result
round(kmeans_model$betweenss/kmeans_model$totss, 3) # 0.946

# 5 Extract the cluster centers = synthetic anchors
anchors <- as.data.frame(kmeans_model$centers)

# 6 Save anchor matrix for use in SOM training
write_rds(anchors, here("data/input/01_synthetic_input_data.rds"))