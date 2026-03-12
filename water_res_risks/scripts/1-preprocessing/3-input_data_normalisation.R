# Data wrangling, normalisation and clipping

library(here)


v <- terra::vect(here("data/input/water_res_risks_noNA_270226.gpkg"))
df <- terra::values(v, dataframe = TRUE) # Use base data.frame for complete.cases

# ---------- helpers ----------
zscore_clip <- function(x, cap = 2) {
  m  <- mean(x, na.rm = TRUE)
  sd <- stats::sd(x, na.rm = TRUE)
  z  <- (x - m) / sd
  z  <- pmin(pmax(z, -cap), cap)        # clip to [-cap, cap]
  return(z)
}

# sign-preserving log transform for signed heavy tails
slog1p <- function(x) sign(x) * log1p(abs(x))

# ---------- variable-specific transforms ----------
normalize_for_archetypes <- function(df) {

  df %>%
    mutate(
      # 1) keep as-is (bounded, reasonably shaped), then z-score
      runoff_nonstat_z = zscore_clip(runoff_nonstat),
      tws_nonstat_z    = zscore_clip(tws_nonstat),
#     gender_ineq_z    = zscore_clip(gender_ineq), #normalised at national scale
#     corruption_z     = zscore_clip(corruption),  #normalised at national scale
#     frag_statei_z    = zscore_clip(frag_statei), #normalised at national scale
      gini_z           = zscore_clip(gini),
      connectivity_si_z   = zscore_clip(connectivity_si),
      e_flow_z         = zscore_clip(e_flow),
#     wwthdr_pc_z    = zscore_clip(wwthdr_pc),     #normalised at national scale

      # 2) heavy right tails -> log1p
#     DALY_log         = log1p(DALY),              #normalised at national scale
      Base_water_stress_log = log1p(Base_water_stress),
      hydropol_int_log = log1p(hydropol_int),
      thrspecies_log   = log1p(thrspecies),
      mismplastic_log      = log1p(mismplastic),
      droughthaz_log       = log1p(droughthaz),
      extremes_damage_log  = log1p(extremes_damage),
      extremes_affected_log = log1p(extremes_affected),

#     DALY_z           = zscore_clip(DALY_log),  #normalised at national scale
      Base_water_stress_z = zscore_clip(Base_water_stress_log),
      hydropol_int_z   = zscore_clip(hydropol_int_log),
      thrspecies_z     = zscore_clip(thrspecies_log),
      droughthaz_z       = zscore_clip(droughthaz_log),
      mismplastic_z       = zscore_clip(mismplastic_log),
      extremes_damage_z       = zscore_clip(extremes_damage_log),
      extremes_affected_z       = zscore_clip(extremes_affected_log),


      # 3) signed heavy tails -> sign-preserving log
      bod_slog         = slog1p(bod),
      wetlandloss_slog = slog1p(wetlandloss),

      bod_z            = zscore_clip(bod_slog),
      wetlandloss_z    = zscore_clip(wetlandloss_slog)
      
    )
}

# ---------- Run and save ----------
df_norm <- normalize_for_archetypes(df)
df_z    <- dplyr::select(df_norm, dplyr::ends_with("_z"))

# Columns that already exist in the GPKG (keep existing values in v)
already_in_gpkg <- c(
  "gender_ineq_nat_z",
  "corruption_nat_z",
  "frag_statei_nat_z",
  "DALY_nat_z",
  "wwthdr_pc_nat_z"
)

# Drop the explicit exceptions if present in df_z
df_z2 <- dplyr::select(df_z, -dplyr::any_of(already_in_gpkg))

# Extra safety: drop ANY df_z columns that already exist in v (prevents accidental dupes)
existing_in_v <- names(terra::values(v, dataframe = TRUE))
df_z2 <- df_z2[, setdiff(names(df_z2), existing_in_v), drop = FALSE]

# Bind and write
v_out <- cbind(v, df_z2)

out_path <- here("data/input/water_res_risks_norm.gpkg")
terra::writeVector(
  v_out, out_path,
  filetype = "GPKG",
  layer = "water_res_risks_norm",
  overwrite = TRUE
)

## Correlation matrix on normalized variables

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(patchwork)   # for arranging plots
library(corrplot)


# Select *_z variables
cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)
# Named titles keyed by column name 
full_names_norm <- c(
  runoff_nonstat_z   = "Runoff non-stationarity, norm",
  bod_z              = "Biological oxygen demand (BOD) change, norm",
  gender_ineq_nat_z      = "Gender inequality",
  thrspecies_z       = "Threatened freshwater species richness, norm",
  wetlandloss_z      = "Wetland loss, norm",
  tws_nonstat_z      = "TWS non-stationarity, norm",
  DALY_nat_z             = "DALYs (water-related burden),norm",
  corruption_nat_z       = "Corruption perception, norm",
  frag_statei_nat_z      = "State fragility, norm",
  wwthdr_pc_nat_z        = "Water withdrawal, norm",
  connectivity_si_z    = "River connectivity (CSI), norm",
  e_flow_z           = "Environmental flow disruption,norm",
  Base_water_stress_z = "Baseline water stress, norm",
  hydropol_int_z   = "Hydropolitical intensity, norm",
  gini_z          = "GINI, norm",
  mismplastic_z   = "Mismanaged plastic waste, norm",
  droughthaz_z    = "Drought hazard, norm",
  extremes_damage_z = "Damage from w extremes, norm",
  extremes_affected_z = "People affected by w extremes, norm"
)


# Sample rows to keep things fast
df_sample <- df_norm[, cols_to_plot, drop = FALSE]              # keep only needed columns
df_sample <- df_sample[sample(nrow(df_sample),
                              min(9000, nrow(df_sample))), ]    # sample up to 9000 rows
colnames(df_sample) <- full_names_norm                               # rename for nice labels

# Compute Pearson correlation matrix
cor_mat <- cor(df_sample, use = "pairwise.complete.obs", method = "pearson")

# Find top 4 absolute correlations (excluding self-correlations)
cor_vals <- as.data.frame(as.table(cor_mat)) %>%
  filter(Var1 != Var2) %>%
  mutate(abs_val = abs(Freq)) %>%
  arrange(desc(abs_val)) %>%
  distinct(pmin(Var1, Var2), pmax(Var1, Var2), .keep_all = TRUE) %>%
  head(4)

print(cor_vals)

# Plot
corrplot(cor_mat,
         method = "shade",
         type = "upper",
         diag = TRUE,
         tl.cex = 0.5,
         tl.col = "black",
         addCoef.col = "black",
         tl.srt = 45,
         number.cex = 0.5)

# Save to PNG
png("plots/input_correlation_matrix_z.png", width = 1000, height = 800, res = 300)

# Build density plots for Z-normalized variables (drops NAs)
library(ggplot2)
library(patchwork)

# Select *_z variables
cols_to_plot <- grep("_z$", names(df_norm), value = TRUE)

# Density plots
plots <- lapply(seq_along(cols_to_plot), function(i) {
  col <- cols_to_plot[i]
  name <- full_names[i]

  x <- as.numeric(df_norm[[col]])
  x <- x[is.finite(x)]

  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6) ) +
    labs(title = full_names_norm[[col]], x = col, y = "Density")
})

# Combine into grid
combined_plot <- wrap_plots(plots, ncol = 4)

# Display
print(combined_plot)

# Save
ggsave(here("plots/normalized_variables_distribution_z.png"),
       combined_plot, width = 12, height = 8, dpi = 450)

#Check the normalised stats
library(e1071) # for skewness, kurtosis

summary_stats <- lapply(cols_to_plot, function(col) {
  x <- as.numeric(df_norm[[col]])
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

#Diagnostic for selected variables
cap_rate <- function(z, cap=2) {
  z <- z[is.finite(z)]
  c(pct_negcap = mean(z <= -cap) * 100,
    pct_poscap = mean(z >=  cap) * 100,
    sd = sd(z))
}
sapply(df_norm[c("connectivity_si_z","Base_water_stress_z","bod_z","e_flow_z")], cap_rate)
