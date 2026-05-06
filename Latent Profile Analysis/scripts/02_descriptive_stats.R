# ============================================================
# Script 02: Descriptive Statistics - 2025
# ============================================================

# Extract 2025 data
df_2025 <- df %>%
  select(
    faculty_name = `Faculty Name`,
    institution  = Institution,
    rank_2025    = `2025...32`,
    pub_2025     = `2025...49`
  ) %>%
  filter(!is.na(pub_2025))

# Summary statistics
summary(df_2025$pub_2025)
cat("\nStandard deviation:", sd(df_2025$pub_2025))
cat("\nNumber of faculty:", nrow(df_2025))

# Distribution plot
hist(df_2025$pub_2025,
     main   = "Publication Count Distribution - 2025",
     xlab   = "Annual Publications",
     col    = "steelblue",
     breaks = 10)