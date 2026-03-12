# DATA DISTIRBUTION AND CORRELATION ANALYSIS

# Load libraries
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)


# Load the final prepared data
v <- vect(here("data/input/water_res_risks_masked_renamed.gpkg"))
 df <- as.data.frame(v)
 
 # # Drop rows with incomplete data
 df <- as.data.frame(v)  # Use base data.frame for complete.cases
 complete_rows <- complete.cases(df)
 v <- v[complete_rows, ]
 
 v[] <- df
 
 anyNA(df)
 
 writeVector(v, here("data/input/water_res_risks_noNA.gpkg"), overwrite = TRUE)
 
 # Optional - reload if needed
 
 v <- terra::vect(here("data/input/water_res_risks_noNA_270226.gpkg"))
 df <- terra::values(v, dataframe = TRUE) # Use base data.frame for complete.cases
 
 
# Columns to plot
cols_to_plot <- c(
  "runoff_nonstat","bod","gender_ineq_nat_z","thrspecies","wetlandloss",
  "tws_nonstat","DALY_nat_z","corruption_nat_z","frag_statei_nat_z","wwthdr_pc_nat_z",
  "connectivity_si","e_flow","Base_water_stress","hydropol_int", 
  "gini", "mismplastic", "droughthaz", "extremes_damage", "extremes_affected")

# Named titles keyed by column name 
full_names <- c(
  runoff_nonstat   = "Runoff non-stationarity",
  bod              = "Biological oxygen demand (BOD) change",
  gender_ineq_nat_z      = "Gender inequality (norm)",
  thrspecies       = "Threatened freshwater species richness",
  wetlandloss      = "Wetland loss (%)",
  tws_nonstat      = "TWS non-stationarity",
  DALY_nat_z      = "DALYs (water-related burden) (norm)",
  corruption_nat_z       = "Corruption perception  (norm)",
  frag_statei_nat_z      = "State fragility (norm)",
  wwthdr_pc_nat_z        = "Water withdrawal (%) (norm)",
  connectivity_si    = "Connectivity status  (CSI)",
  e_flow           = "Environmental flow disruption",
  Base_water_stress= "Baseline water stress",
  hydropol_int   = "Hydropolitical intensity",
  gini          = "GINI",
  mismplastic   = "Mismanaged plastic waste",
  droughthaz    = "Drought hazard",
  extremes_damage = "Damage from w extremes, '000 USD adj",
  extremes_affected = "People affected by w extremes"
)


# Safety: ensure every column has a title
missing_titles <- setdiff(cols_to_plot, names(full_names))
if (length(missing_titles)) stop("Missing titles for: ", paste(missing_titles, collapse=", "))

#####CHECK THE DATA

library(e1071) # for skewness, kurtosis

summary_stats <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df[[col]])
  x <- x[is.finite(x)]
  data.frame(
    variable = col,
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE),
    pct_zero = mean(x == 0, na.rm = TRUE) * 100,
    n = length(x)
  )
})
summary_stats <- do.call(rbind, summary_stats)

print(summary_stats)

# Build density plots (drops NAs, keeps your xlim)
library(ggplot2)
plots <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df[[col]])
  x <- x[is.finite(x)]
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6) ) +
    labs(title = full_names[[col]], x = col, y = "Density") 
})

library(patchwork)
# Combine plots in tiles using patchwork
combined_plot <- wrap_plots(plots, ncol = 4)

# Display combined plot
print(combined_plot)

# Optionally, save the combined plot as an image
ggsave(here("plots/variables_distribution_nona.png"), combined_plot, width = 12, height = 8, dpi = 450)


## Correlation matrix
library(corrplot)
library(dplyr)

# Prepare data for correlation matrix
colnames_plot <- full_names
names(colnames_plot) <- cols_to_plot

df_sample <- df[, cols_to_plot, drop = FALSE]
df_sample <- df_sample[sample(nrow(df_sample), min(9000, nrow(df_sample))), ]
colnames(df_sample) <- colnames_plot

# Compute Pearson correlation matrix
cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")

# Find top 4 absolute correlations (excluding self-correlations)
cor_vals <- as.data.frame(as.table(cor_mat)) %>%
  filter(Var1 != Var2) %>%
  mutate(abs_val = abs(Freq)) %>%
  arrange(desc(abs_val)) %>%
  distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
  head(4)

# Display values
print(cor_vals)

# Display correlation plot in R session
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)     # shrink numbers inside cells)

dev.off()

# Save correlation plot to PNG
png("plots/input_correlation_matrix_raw_new.png", width = 1000, height = 800, res = 300)
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black")
dev.off()


