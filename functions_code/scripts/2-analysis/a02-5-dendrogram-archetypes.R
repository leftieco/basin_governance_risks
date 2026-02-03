### ---------------------\\ 
# Script objective:
# Use agglomerative clustering to group groundwaterscapes
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# average linkage merges clusters based on the mean distance between all pairs.
# complete linkage merges based on the maximum distance between clusters.
# Also, agnes(..., stand = TRUE) standardizes variables, which could slightly shift clustering compared to raw distance!

archetypes = readr::read_rds(here("data/som_files/som_selection/som2_selection.rds"))
archetypes = archetypes$codes[[1]] |> as_tibble()

agn = agnes(x=archetypes, diss = FALSE, stand = TRUE,
            method = "average")
DendAgn =as.dendrogram(agn)
 # plot(DendAgn)

agn_hclust = as.hclust(agn)

pdf_size = 3.5
pdf(here("plots/dendrogram_archetypes.pdf"), width=2*pdf_size, height=pdf_size)

dev.off()

### Choose amount of groupings ###
plot(agn_hclust, hang = -1, lwd = 2, bg=NA)
rect.hclust(agn_hclust, k = 5, border = 2:5)

dev.off()

plot(hc, hang = -1, lwd = 2, bg=NA)
rect.hclust(hc, k = 5, border = 2:5)

agn_hclust$method
hc$method

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
