### ---------------------\\ 
# Script objective:
# Create a synthetic representation of input data using k-means cluster centers

library(terra)
library(dplyr)
library(cluster)  
library(stats)     # For kmeans()
library(here)
library(readr)


# 1 Load vector and extract attributes
v <- st_read(here("data/input/wfunctions_gr_ready.gpkg"), quiet = TRUE)
df <- as.data.frame(v)

# 2 Select only *_gr normalized columns for SOM ---
cols_gr   <- grep("_gr$", names(df), value = TRUE)
input_df <- df[, cols_gr, drop = FALSE]

# 3 Run K-means clustering to generate anchors
# Set k (number of clusters/anchors)
set.seed(123)
k <- round(0.05 * nrow(input_df))  # 5% of sample size

kmeans_model <- kmeans(input_df, centers = k, nstart = 20, iter.max = 50) #iter.max = 100, nstart = 20

# 4 Variance explained (R^2) for base::kmeans result
round(kmeans_model$betweenss/kmeans_model$totss, 3) # 0.95 

# 5 Fraction of data kept as anchors
k / nrow(input_df) # 5% of data size

# 6 Extract the cluster centers = synthetic anchors
anchors <- as.data.frame(kmeans_model$centers)

# 7 Save anchor matrix for use in SOM training
write_rds(anchors, here("data/input/01_synthetic_input_data.rds"))
