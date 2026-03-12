library(terra)
library(here)

library(e1071)     # skewness/kurtosis
library(ggplot2)
library(patchwork)
library(corrplot)

# Load the final clipped and normalized vector file
v <- vect(here("data/input/water_res_risks_z_ready.gpkg"))

# Extract attribute data
df <- as.data.frame(v)

## --------------------------
# 1) Columns to plot (ONLY *_z)
# --------------------------
cols_to_plot <- c(
  "runoff_nonstat_z","bod_z","thrspecies_z","wetlandloss_z",
  "tws_nonstat_z","wwthdr_pc_nat_z","connectivity_si_z","e_flow_z",
  "Base_water_stress_z","hydropol_int_z","gini_z","mismplastic_z",
  "droughthaz_z","extremes_damage_z","extremes_affected_z",
  "equity_risk_index_nat_z","gov_risk_index_nat_z"
)

# Safety: ensure columns exist in df
missing_cols <- setdiff(cols_to_plot, names(df))
if (length(missing_cols)) stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))

# --------------------------
# 2) Names for plotting
# --------------------------
full_names <- c(
  runoff_nonstat_z    = "Runoff non-stationarity",
  bod_z               = "Biological oxygen demand change",
  thrspecies_z        = "Threatened freshwater species richness",
  wetlandloss_z       = "Wetland loss",
  tws_nonstat_z       = "TWS non-stationarity",
  wwthdr_pc_nat_z         = "Water withdrawal",
  connectivity_si_z     = "River connectivity (CSI)",
  e_flow_z            = "Environmental flow disruption",
  Base_water_stress_z = "Baseline water stress",
  hydropol_int_z      = "Hydropolitical intensity",
  gini_z              = "GINI",
  mismplastic_z       = "Mismanaged plastic waste",
  droughthaz_z        = "Drought hazard",
  extremes_damage_z   = "Damage from water extremes",
  extremes_affected_z = "People affected by water extremes",
  equity_risk_index_nat_z      = "Equity risk index",
  gov_risk_index_nat_z          = "Governance risk index"
)

# --------------------------
# 3) Density plots
# --------------------------
plots <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df[[col]])
  x <- x[is.finite(x)]
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6)
    ) +
    labs(title = full_names[[col]], x = col, y = "Density")
})

combined_plot <- patchwork::wrap_plots(plots, ncol = 4)
print(combined_plot)

ggsave(here("plots/variables_distribution_z_ready.png"),
       combined_plot, width = 12, height = 8, dpi = 450)

# --------------------------
# 4) Correlation matrix
# --------------------------
set.seed(123)
idx <- sample(nrow(df), min(9000, nrow(df)))

df_sample <- df[idx, cols_to_plot, drop = FALSE]
colnames(df_sample) <- unname(full_names[cols_to_plot])

cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")

# Top 4 absolute correlations (upper triangle only)
ut <- which(upper.tri(cor_mat), arr.ind = TRUE)
top4 <- ut[order(abs(cor_mat[ut]), decreasing = TRUE), , drop = FALSE][1:4, , drop = FALSE]
cor_vals <- data.frame(
  Var1 = rownames(cor_mat)[top4[,1]],
  Var2 = colnames(cor_mat)[top4[,2]],
  r    = cor_mat[top4]
)
print(cor_vals)

# Save correlation plot
png(here("plots/input_correlation_matrix_z_ready.png"), width = 2000, height = 1600, res = 300)
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)
dev.off()

# --------------------------
# 4) Potential cluster
# --------------------------

# Create distance matrix from correlation
d <- as.dist(1 - abs(cor_mat))  # distance = 1 - |correlation|

# Hierarchical clustering
hc <- hclust(d, method = "ward.D2")

# Plot dendrogram
pdf("assets/variable_clustering_dendrogram.pdf", width = 8, height = 6)
plot(hc, main = "Clustering of Input Variables", xlab = "", sub = "")
dev.off()
