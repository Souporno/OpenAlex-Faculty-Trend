# ============================================================
# Script 03: Latent Profile Analysis - 2025
# ============================================================

# Run LPA solutions 1 through 5
lpa_results <- df_2025 %>%
  select(pub_2025) %>%
  estimate_profiles(1:5)

# Compare solutions
compare_solutions(lpa_results)

# BIC winner: 2 classes
# 3, 4, 5 class solutions produced convergence warnings
# Likely due to small sample size (n=34)
# Expected to find 3 classes at larger sample sizes

# Extract 2-class solution
model_2class <- df_2025 %>%
  select(pub_2025) %>%
  estimate_profiles(2)

class_assignments <- get_data(model_2class)

df_2025_with_class <- bind_cols(df_2025, class_assignments)

# Group summary
df_2025_with_class %>%
  group_by(Class) %>%
  summarise(
    n        = n(),
    mean_pub = round(mean(pub_2025...4), 2),
    min_pub  = min(pub_2025...4),
    max_pub  = max(pub_2025...4)
  ) %>%
  arrange(mean_pub)