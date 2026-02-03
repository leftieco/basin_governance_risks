library(terra)
library(dplyr)
library(corrplot)
library(here)

# Load the final clipped and normalized vector file
v <- vect(here("data/input/allbutstorage_som_ready.gpkg"))

# Extract attribute data
df <- as.data.frame(v)

# Select relevant normalized columns (same ones from SOM prep)
cols <- c(
  "porosity", "gov_effectiveness", "udw_dependence", "blue_water_prod",
  "water_capacity", "intermittent_flow", "hdi", "crop_footprint",
  "n_yield", "p_yield", "hydropower"
)

# Optional: Rename for nicer plotting
colnames_plot <- c(
  "Porosity", "Gov.Eff.", "Unimp.Drink.W. ", "Gr.Biom.W.Prod.",
  "Av.Biom.W.Stor.Cap.", "Flow.Intermit.Prob.", "HDI", "CropWaterFootprint",
  "NYield", "PYield", "Hydropower"
)

# Sample all polygons for performance (adjust as needed)
df_sample <- df %>%
  select(all_of(cols)) %>%
  slice_sample(n = min(4556, nrow(.))) %>%
  setNames(colnames_plot)

# Compute Pearson correlation matrix
cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")

# Fix names to match for plotting
rownames(cor_mat) <- colnames_plot
colnames(cor_mat) <- colnames_plot


# Round for display
print(round(cor_mat, 2))


# Plot
pdf_size <- 5
pdf(here("assets/vector_input_correlation.pdf"), width = 2 * pdf_size, height = pdf_size)
corrplot(cor_mat, method = "shade", type = "upper", diag = TRUE,
         bg = "transparent", tl.cex = 0.8, tl.col = "black")
dev.off()

library(corrplot)

# Save correlation plot to PNG
png("assets/input_correlation_matrix.png", width = 1000, height = 800, res = 300)

corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.9,
         tl.col = "black",
         addCoef.col = "black")

dev.off()


#Potential cluster

# Create distance matrix from correlation
d <- as.dist(1 - abs(cor_mat))  # distance = 1 - |correlation|

# Hierarchical clustering
hc <- hclust(d, method = "ward.D2")

# Plot dendrogram
pdf("assets/variable_clustering_dendrogram.pdf", width = 8, height = 6)
plot(hc, main = "Clustering of Input Variables", xlab = "", sub = "")
dev.off()

# Create distance matrix and clustering object
d <- as.dist(1 - abs(cor_mat))
hc <- hclust(d, method = "ward.D2")

# Save dendrogram to PNG
png("assets/variable_clustering_dendrogram.png", width = 800, height = 600, res = 300)

plot(hc, main = "Clustering of Input Variables", xlab = "", sub = "")

dev.off()
