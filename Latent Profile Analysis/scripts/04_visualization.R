# ============================================================
# Script 04: Visualization - LPA 2025
# ============================================================

df_2025_with_class <- df_2025_with_class %>%
  mutate(profile_label = case_when(
    Class == 1 ~ "Standard Output",
    Class == 2 ~ "High Output"
  ))

ggplot(df_2025_with_class,
       aes(x = profile_label, y = pub_2025...4, fill = profile_label)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2.5) +
  scale_fill_manual(values = c("Standard Output" = "steelblue",
                                "High Output"     = "#DC143C")) +
  labs(
    title    = "Publication Output Profiles - 2025",
    subtitle = "LPA 2-class solution (n = 34)",
    x        = "Profile Group",
    y        = "Annual Publication Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save figure
ggsave("outputs/figures/lpa_2025_boxplot.png", 
       width = 8, height = 6, dpi = 300)