### ---------------------\\ 
# Script objective:
# Use agglomerative clustering to group groundwaterscapes
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# average linkage merges clusters based on the mean distance between all pairs.
# complete linkage merges based on the maximum distance between clusters.
# Also, agnes(..., stand = TRUE) standardizes variables, which could slightly shift clustering compared to raw distance!

archetypes = readr::read_rds(here("data/som_files/som_selection/som2_selection_26.rds"))
archetypes = archetypes$codes[[1]] |> as_tibble()

agn = agnes(x=archetypes, diss = FALSE, stand = TRUE,
            method = "average")
DendAgn =as.dendrogram(agn)
 # plot(DendAgn)

agn_hclust = as.hclust(agn)

pdf_size = 3.5
pdf(here("plots/dendrogram_archetypes_26.pdf"), width=2*pdf_size, height=pdf_size)

### Choose amount of groupings ###
plot(agn_hclust, hang = -1, lwd = 2, bg=NA)
rect.hclust(agn_hclust, k = 5, border = 2:5)

dev.off()

plot(hc, hang = -1, lwd = 2, bg=NA)
rect.hclust(hc, k = 5, border = 2:5)

agn_hclust$method
hc$method

# Basic agnes summary
summary(agn)

str(agn_hclust)

# Cut the tree into 5 clusters
clusters5 <- cutree(agn_hclust, k = 5)

# Add to your archetype summary
# Extract unique archetype IDs and cluster memberships
archetype_clusters <- data.frame(
  archetypeID = unique(df$archetypeID),
  cluster5 = clusters5
)

# Check alignment
head(archetype_clusters)
archetype_clusters

# Combine archetype table + cluster labels
archetype_clusters <- tibble(
  archetypeID = 1:26,
  cluster5 = clusters5
)

# Save this non-spatial table
write.csv(
  archetype_clusters,
  here::here("data/output/archetype_26_cluster_mapping.csv"),
  row.names = FALSE
)

# Load your watershed polygons (182k rows)
watersheds <- terra::vect(here::here("data/output/water_res_risk_archetypes_26.gpkg"))

# Merge by archetypeID
watersheds$cluster5 <- archetype_clusters$cluster5[
  match(watersheds$archetypeID, archetype_clusters$archetypeID)
]

terra::writeVector(
  watersheds,
  here::here("data/output/water_res_risk_26archetypes_5cluster.gpkg"),
  filetype = "GPKG",
  overwrite = TRUE
)


### Let's makes sens of the archetypes
library(readr)
library(here)

# Load the archetype summary table
df <- read_csv(here("data/output/archetype_26_summary.csv"))

# 1️⃣ Select just the numeric metric columns (drop archetypeID)
metrics_mat <- metrics %>%
  dplyr::select(ends_with("_gr")) %>%
  as.data.frame()

# 2️⃣ Run PCA on that matrix
res.pca <- prcomp(metrics_mat, scale. = TRUE)

library(factoextra)

fviz_pca_biplot(res.pca,
                repel = TRUE,
                geom.ind = "point",
                col.var = "black",
                habillage = df$cluster5,  # optional, if cluster info exists
                palette = c("#1f78b4", "#e66101", "#4daf4a", "#ffd92f", "#984ea3"),
                title = "PCA of 26 Archetypes – Water Resilience Metrics")


###LInear Discriminant Analysis
library(MASS)
library(ggplot2)

df <- as.data.frame(df)

lda_fit <- lda(cluster5 ~ ., data = df %>% dplyr::select(cluster5, runoff_nonstat_gr:equity_index_gr))

# Predict cluster membership
lda_pred <- predict(lda_fit)

# Create plotting dataframe
lda_df <- data.frame(lda_pred$x, cluster5 = as.factor(df$cluster5))

#Plot
ggplot(lda_df, aes(LD1, LD2, color = cluster5)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.9) +
  scale_color_manual(values = c("#1f78b4", "#e66101", "#4daf4a", "#ffd92f", "#984ea3")) +
  theme_minimal() +
  labs(
    title = "Linear Discriminant Analysis – Archetype Clusters",
    subtitle = "LD1–LD2: dimensions maximizing inter-cluster separation",
    color = "Cluster"
  )

round(lda_fit$scaling, 3)
table(Predicted = lda_pred$class, Actual = df$cluster5)

### #Multidimentinal scaling

library(vegan)
library(ggplot2)

# 1️⃣ Select metrics (numeric variables only)
metrics_mat <- df %>%
  dplyr::select(ends_with("_gr")) %>%
  as.data.frame()

# 2️⃣ Compute Euclidean distance between archetypes
dist_mat <- dist(scale(metrics_mat))

# 3️⃣ Run classical MDS (cmdscale)
mds <- cmdscale(dist_mat, eig = TRUE, k = 2)

# 4️⃣ Create plotting dataframe
mds_df <- data.frame(
  MDS1 = mds$points[, 1],
  MDS2 = mds$points[, 2],
  cluster5 = as.factor(df$cluster5),
  archetypeID = df$archetypeID
)

# 5️⃣ Plot with ggplot
ggplot(mds_df, aes(MDS1, MDS2, color = cluster5, label = archetypeID)) +
  geom_point(size = 3) +
  geom_text(vjust = -1.2, size = 3) +
  scale_color_manual(values = c(
    "1" = "#1f78b4",  # Persistent
    "2" = "#e66101",  # Transformative
    "3" = "#4daf4a",  # Adaptive
    "4" = "#ffd92f",  # Persistent–Adaptive
    "5" = "#984ea3"   # Cohesive Persistent
  )) +
  theme_minimal() +
  labs(
    title = "Multidimensional Scaling (MDS) of 26 Archetypes",
    subtitle = "Distances reflect overall similarity across all resilience metrics",
    color = "Cluster"
  )

library(ggforce)
ggplot(mds_df, aes(MDS1, MDS2, color = cluster5)) +
  geom_point(size = 3) +
  geom_mark_ellipse(aes(fill = cluster5), alpha = 0.15, show.legend = FALSE) +
  geom_text(aes(label = archetypeID), size = 3, vjust = -1.2) +
  scale_color_manual(values = c("#1f78b4", "#e66101", "#4daf4a", "#ffd92f", "#984ea3")) +
  scale_fill_manual(values = c("#1f78b4", "#e66101", "#4daf4a", "#ffd92f", "#984ea3")) +
  theme_minimal()

vegan::stressplot(metaMDS(dist_mat, k = 2))


# DendAgn
# grp <- cut(DendAgn, k = 4)$lower
# head(grp, n = 4)
# 
# # Euclidean distance
# dist <- dist(archetypes, diag=TRUE)
# 
# # Hierarchical Clustering with hclust
# hc <- hclust(dist)
# 
# # Plot the result
# plot(hc)
# 
# pdf_size = 3.5
# pdf(here("plots/dendrogram_archetypes.pdf"), width=2*pdf_size, height=pdf_size)
# 
# plot(hc, hang = -1, lwd = 2, bg=NA)
# rect.hclust(hc, k = 5, border = 2:5)
# 
# dev.off()
# 
# plot(hc, hang = -1, lwd = 2, bg=NA)
# rect.hclust(hc, k = 5, border = 2:5)
