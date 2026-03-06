library(tidyverse)


# --- 1. Set Simulation Parameters ---
set.seed(42) # For reproducibility
true_treatment_effect <- 2.0  # RLGPs increase SOC by 2 units
field_variance <- 3.0         # Natural difference between any two random fields
subsample_variance <- 0.5     # Measurement error / spatial variation within the same field
base_soc <- 15.0              # Baseline SOC

# ====================================================================
# SCENARIO A: The Proposed Design (Pseudoreplication)
# 1 Farm, 2 Fields (1 Trt, 1 Ctrl), 4 Subsamples per field
# ====================================================================

# Simulate the two fields
# Because of field_variance, the Control field might randomly start with higher SOC
field_control_effect <- rnorm(1, 0, field_variance)
field_trt_effect <- rnorm(1, 0, field_variance)

# Generate the 4 "replicates" (subsamples) for each field
scenario_A <- data.frame(
  Condition = rep(c("Control", "RLGP"), each = 4),
  Field = rep(c("Field_1_Ctrl", "Field_2_Trt"), each = 4),
  Subsample = rep(1:4, 2)
)

# Calculate simulated SOC (FIXED with rowwise)
scenario_A <- scenario_A |>
  rowwise() |>    # <--- This tells R to calculate row-by-row
  mutate(
    SOC = case_when(
      Condition == "Control" ~ base_soc + field_control_effect + rnorm(1, 0, subsample_variance), # Changed 4 to 1
      Condition == "RLGP" ~ base_soc + true_treatment_effect + field_trt_effect + rnorm(1, 0, subsample_variance) # Changed 4 to 1
    )
  ) |> 
  ungroup()       # <--- Good practice to ungroup after rowwise calculations

# ====================================================================
# SCENARIO B: The Alternative Design (True Replication)
# 4 Farms, 8 Fields (4 Trt, 4 Ctrl), 1 Sample per field
# ====================================================================

scenario_B <- data.frame(
  Condition = rep(c("Control", "RLGP"), each = 4),
  Farm = rep(1:4, times = 2),
  Field = paste0("Farm_", rep(1:4, times = 2), "_", rep(c("Ctrl", "Trt"), each = 4))
)

# Calculate simulated SOC (each field gets its own random field effect)
scenario_B <- scenario_B |>
  rowwise() |>
  mutate(
    Field_Effect = rnorm(1, 0, field_variance),
    SOC = case_when(
      Condition == "Control" ~ base_soc + Field_Effect + rnorm(1, 0, subsample_variance),
      Condition == "RLGP" ~ base_soc + true_treatment_effect + Field_Effect + rnorm(1, 0, subsample_variance)
    )
  ) |> ungroup()

# --- 3. The Analysis (The Proof) ---

# Test Scenario A: A simple t-test will be heavily skewed by the random field effect
t_test_A <- t.test(SOC ~ Condition, data = scenario_A)

# Test Scenario B: Testing across independent fields
t_test_B <- t.test(SOC ~ Condition, data = scenario_B)

# Print the results
cat("=== SCENARIO A (Pseudoreplication: 4 subsamples in 1 farm) ===\n")
cat("Estimated Treatment Effect (RLGP - Control):", diff(rev(t_test_A$estimate)), "\n")
cat("P-value (highly deceptive due to pseudoreplication):", t_test_A$p.value, "\n\n")

cat("=== SCENARIO B (True Replication: 1 sample across 4 farms) ===\n")
cat("Estimated Treatment Effect (RLGP - Control):", diff(rev(t_test_B$estimate)), "\n")
cat("P-value (reflects true statistical power):", t_test_B$p.value, "\n")
