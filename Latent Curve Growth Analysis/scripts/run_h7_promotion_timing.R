# =============================================================================
# run_h7_promotion_timing.R
#
# Tests H7: Faculty in higher-growth or higher-output LCGA trajectory classes
# reach promotion to associate professor sooner than faculty in lower-output
# trajectory classes.
#
# Retained 4-class LCGA solution (original class labels):
#   Class 1  =  Very low-output            (n = 28)
#   Class 4  =  Low-to-moderate rising     (n = 25)
#   Class 2  =  Moderate-to-high rising    (n = 55)
#   Class 3  =  Small high-growth          (n = 6)
#
# Analyses:
#   1. Kruskal-Wallis test on years_to_promotion (promoted faculty only)
#   2. Pairwise Wilcoxon tests, BH correction
#   3. Cox proportional hazards (all eligible faculty; non-promoted = censored)
#   4. Log-rank test
#   5. Proportional-hazards assumption check (cox.zph)
#
# Inputs:
#   lcga_data_long.csv          -- long-format faculty-year observations
#   lcga_4class_assignments.csv -- retained B=50 4-class assignments
#
# Outputs:
#   h7_faculty_level_dataset.csv
#   h7_class_descriptives.csv
#   h7_kruskal_results.txt
#   h7_pairwise_wilcox.csv
#   h7_cox_model_results.csv
#   h7_survival_diagnostics.txt
#   h7_boxplot_years_to_promotion.pdf
#   h7_km_plot.pdf
#   h7_results_summary.txt
#
# Usage:
#   Rscript run_h7_promotion_timing.R
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
})

# Optional packages — load if available
has_survminer <- requireNamespace("survminer", quietly = TRUE)
has_broom     <- requireNamespace("broom",     quietly = TRUE)
if (has_survminer) library(survminer)
if (has_broom)     library(broom)

cat("=============================================================\n")
cat("H7: Promotion Timing by LCGA Trajectory Class\n")
cat("=============================================================\n\n")

# ---- 1. Load data ------------------------------------------------------------

long_raw   <- read.csv("lcga_data_long.csv",          stringsAsFactors = FALSE)
class_raw  <- read.csv("lcga_4class_assignments.csv",  stringsAsFactors = FALSE)

cat(sprintf("Long-format rows loaded  : %d\n", nrow(long_raw)))
cat(sprintf("Faculty in class file    : %d\n", nrow(class_raw)))

# ---- 2. Clean long-format data -----------------------------------------------

long <- long_raw %>%
  filter(!is.na(faculty_id), !is.na(career_year), !is.na(pub_count)) %>%
  mutate(
    faculty_id  = as.integer(faculty_id),
    career_year = as.numeric(career_year),
    pub_count   = as.numeric(pub_count)
  ) %>%
  filter(career_year >= 1)

# Handle promoted_this_year
if (!"promoted_this_year" %in% names(long)) {
  stop("'promoted_this_year' column not found in lcga_data_long.csv. Cannot proceed.")
}

n_missing_prom <- sum(is.na(long$promoted_this_year))
if (n_missing_prom > 0) {
  cat(sprintf(
    "WARNING: %d rows have missing promoted_this_year. Treating as 0.\n",
    n_missing_prom))
}
long <- long %>%
  mutate(promoted_this_year = as.integer(replace_na(as.numeric(promoted_this_year), 0)))

# Check for never_asst_prof flag
has_never_asst <- "never_asst_prof" %in% names(long)
if (!has_never_asst) {
  cat("WARNING: 'never_asst_prof' column not found. ",
      "Cannot exclude non-Asst-Prof faculty from primary analysis.\n\n")
} else {
  long <- long %>%
    mutate(never_asst_prof = as.logical(never_asst_prof))
}

# ---- 3. Build faculty-level promotion dataset --------------------------------

fac <- long %>%
  group_by(faculty_id) %>%
  summarise(
    faculty_name            = first(faculty_name),
    max_observed_career_year = max(career_year),
    mean_pub_per_year       = mean(pub_count, na.rm = TRUE),
    ever_promoted           = as.integer(any(promoted_this_year == 1, na.rm = TRUE)),
    first_promotion_career_year = {
      promo_yrs <- career_year[promoted_this_year == 1]
      if (length(promo_yrs) > 0) min(promo_yrs) else NA_real_
    },
    never_asst_prof = if (has_never_asst) first(never_asst_prof) else NA,
    .groups = "drop"
  ) %>%
  mutate(
    years_to_promotion         = first_promotion_career_year,
    years_elapsed_to_promotion = first_promotion_career_year - 1,
    event_promoted             = ever_promoted,
    time_to_event              = if_else(
      !is.na(first_promotion_career_year),
      first_promotion_career_year,
      max_observed_career_year
    ),
    censored = if_else(ever_promoted == 1L, 0L, 1L)
  )

# ---- 4. Merge LCGA class assignments -----------------------------------------

class_df <- class_raw %>%
  select(faculty_id, class_assignment) %>%
  mutate(faculty_id = as.integer(faculty_id))

fac <- fac %>%
  left_join(class_df, by = "faculty_id")

n_unmatched <- sum(is.na(fac$class_assignment))
if (n_unmatched > 0) {
  cat(sprintf("WARNING: %d faculty could not be matched to a class assignment.\n",
              n_unmatched))
}

# ---- 5. Trajectory labels and ordered factor ---------------------------------

# Original class labels from retained LCGA solution
traj_labels <- c(
  "1" = "Very low-output",
  "2" = "Moderate-to-high rising",
  "3" = "Small high-growth",
  "4" = "Low-to-moderate rising"
)

traj_order <- c(
  "Very low-output",
  "Low-to-moderate rising",
  "Moderate-to-high rising",
  "Small high-growth"
)

fac <- fac %>%
  mutate(
    trajectory_label   = traj_labels[as.character(class_assignment)],
    trajectory_ordered = factor(trajectory_label, levels = traj_order, ordered = TRUE)
  )

# ---- 6. Exclusions for primary H7 analysis -----------------------------------

h7_data_full <- fac %>% filter(!is.na(class_assignment))

if (has_never_asst) {
  n_never_asst <- sum(fac$never_asst_prof == TRUE, na.rm = TRUE)
  cat(sprintf("Excluding %d faculty flagged never_asst_prof = TRUE.\n", n_never_asst))
  h7_data <- h7_data_full %>%
    filter(is.na(never_asst_prof) | never_asst_prof == FALSE)
} else {
  n_never_asst <- 0
  h7_data <- h7_data_full
}

cat(sprintf("H7 analytic sample: %d faculty (%d excluded as never-Asst-Prof).\n\n",
            nrow(h7_data), n_never_asst))

# Promoted-only subset for Kruskal-Wallis and boxplot
h7_promoted <- h7_data %>% filter(ever_promoted == 1)
cat(sprintf("Promoted faculty (used for KW test): %d of %d (%.1f%%)\n\n",
            nrow(h7_promoted), nrow(h7_data),
            100 * nrow(h7_promoted) / nrow(h7_data)))

# ---- 7. Save faculty-level dataset -------------------------------------------

write.csv(h7_data, "h7_faculty_level_dataset.csv", row.names = FALSE)
cat("Saved: h7_faculty_level_dataset.csv\n\n")

# ---- 8. Class-level descriptive statistics -----------------------------------

class_desc <- h7_data %>%
  group_by(trajectory_ordered, trajectory_label, class_assignment) %>%
  summarise(
    n_faculty           = n(),
    n_promoted          = sum(ever_promoted),
    pct_promoted        = round(100 * mean(ever_promoted), 1),
    mean_career_length  = round(mean(max_observed_career_year), 1),
    mean_pub_per_year   = round(mean(mean_pub_per_year), 2),
    mean_yrs_to_promo   = round(mean(years_to_promotion[ever_promoted == 1],   na.rm = TRUE), 1),
    median_yrs_to_promo = round(median(years_to_promotion[ever_promoted == 1], na.rm = TRUE), 1),
    sd_yrs_to_promo     = round(sd(years_to_promotion[ever_promoted == 1],     na.rm = TRUE), 2),
    iqr_yrs_to_promo    = round(IQR(years_to_promotion[ever_promoted == 1],    na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(trajectory_ordered)

cat("--- Class-level descriptives ---\n")
print(as.data.frame(class_desc), row.names = FALSE)
cat("\n")

write.csv(class_desc, "h7_class_descriptives.csv", row.names = FALSE)
cat("Saved: h7_class_descriptives.csv\n\n")

# Flag counter-intuitive patterns
lowest_class_pct <- class_desc %>%
  filter(trajectory_ordered == "Very low-output") %>%
  pull(pct_promoted)

highest_pct <- max(class_desc$pct_promoted, na.rm = TRUE)
highest_class <- class_desc %>%
  filter(pct_promoted == highest_pct) %>%
  pull(trajectory_label)

if ("Very low-output" %in% highest_class) {
  cat("FLAG: Very low-output class has the highest promotion rate (%.1f%%).\n",
      highest_pct)
  cat("      This is counter to the simple H7 expectation.\n\n")
}

# ---- 9. Kruskal-Wallis test (promoted faculty only) -------------------------

cat("--- Kruskal-Wallis test: years_to_promotion ~ trajectory_ordered ---\n")
cat(sprintf("N (promoted faculty) = %d\n\n", nrow(h7_promoted)))

kw_test <- kruskal.test(years_to_promotion ~ trajectory_ordered,
                        data = h7_promoted)

# Epsilon-squared effect size: (H - k + 1) / (n - k)
k_groups <- length(unique(h7_promoted$trajectory_ordered[
  !is.na(h7_promoted$years_to_promotion)]))
n_kw     <- sum(!is.na(h7_promoted$years_to_promotion))
H_stat   <- kw_test$statistic
eps_sq   <- (H_stat - k_groups + 1) / (n_kw - k_groups)
eps_sq   <- max(0, eps_sq)   # floor at 0

cat(sprintf("Kruskal-Wallis H(%d) = %.3f, p = %.4f\n",
            kw_test$parameter, H_stat, kw_test$p.value))
cat(sprintf("Epsilon-squared = %.4f  (small ≈ .01, medium ≈ .06, large ≈ .14)\n\n",
            eps_sq))

# Pairwise Wilcoxon (save regardless of KW significance)
cat("--- Pairwise Wilcoxon tests (BH correction) ---\n")
pairwise_result <- pairwise.wilcox.test(
  x          = h7_promoted$years_to_promotion,
  g          = h7_promoted$trajectory_ordered,
  p.adjust.method = "BH",
  exact      = FALSE
)

# Convert pairwise matrix to long-format data frame
pw_mat <- pairwise_result$p.value
pw_df  <- as.data.frame(as.table(pw_mat)) %>%
  rename(group1 = Var1, group2 = Var2, p_adj_BH = Freq) %>%
  filter(!is.na(p_adj_BH)) %>%
  mutate(significant = p_adj_BH < 0.05) %>%
  arrange(p_adj_BH)

cat("Pairwise p-values (BH-adjusted):\n")
print(pw_df, row.names = FALSE)
cat("\n")
write.csv(pw_df, "h7_pairwise_wilcox.csv", row.names = FALSE)
cat("Saved: h7_pairwise_wilcox.csv\n\n")

# Write Kruskal-Wallis results
sink("h7_kruskal_results.txt")
cat("H7: Kruskal-Wallis Test — Years to Promotion by LCGA Trajectory Class\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(rep("=", 60), "\n\n", sep = "")
cat(sprintf("Analytic sample (promoted faculty only): N = %d\n\n", nrow(h7_promoted)))
cat("Class order (ascending output level):\n")
cat("  1 = Very low-output\n")
cat("  2 = Low-to-moderate rising\n")
cat("  3 = Moderate-to-high rising\n")
cat("  4 = Small high-growth\n\n")
cat("Descriptives by class (promoted faculty):\n")
print(as.data.frame(class_desc %>%
  select(trajectory_label, class_assignment, n_promoted,
         mean_yrs_to_promo, median_yrs_to_promo,
         sd_yrs_to_promo, iqr_yrs_to_promo)),
  row.names = FALSE)
cat("\n")
cat(sprintf("Kruskal-Wallis H(%d) = %.3f, p = %.4f\n",
            kw_test$parameter, H_stat, kw_test$p.value))
cat(sprintf("Epsilon-squared = %.4f\n\n", eps_sq))
cat("Pairwise Wilcoxon (BH-adjusted):\n")
print(pw_df, row.names = FALSE)
sink()
cat("Saved: h7_kruskal_results.txt\n\n")

# ---- 10. Survival analysis: Cox PH + log-rank --------------------------------

cat("--- Survival analysis (all eligible faculty; non-promoted = censored) ---\n")
cat(sprintf("N total = %d  |  promoted = %d  |  censored = %d\n\n",
            nrow(h7_data), sum(h7_data$event_promoted),
            sum(h7_data$censored)))

# Reference level: Very low-output
h7_data <- h7_data %>%
  mutate(trajectory_factor = relevel(
    factor(trajectory_label, levels = traj_order),
    ref = "Very low-output"
  ))

# Cox model
cox_fit <- coxph(
  Surv(time_to_event, event_promoted) ~ trajectory_factor,
  data = h7_data
)

cat("Cox PH model summary:\n")
print(summary(cox_fit))
cat("\n")

# Log-rank test
logrank_test <- survdiff(
  Surv(time_to_event, event_promoted) ~ trajectory_factor,
  data = h7_data
)
cat("Log-rank test:\n")
print(logrank_test)
cat("\n")

# PH assumption check
ph_check <- cox.zph(cox_fit)
cat("Proportional hazards assumption (cox.zph):\n")
print(ph_check)
cat("\n")

# Save Cox results
if (has_broom) {
  cox_tidy <- broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) %>%
    rename(hazard_ratio = estimate, HR_low95 = conf.low, HR_high95 = conf.high) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
} else {
  cx_sum <- summary(cox_fit)$coefficients
  cox_tidy <- data.frame(
    term         = rownames(cx_sum),
    hazard_ratio = round(exp(cx_sum[, "coef"]), 4),
    std.error    = round(cx_sum[, "se(coef)"], 4),
    statistic    = round(cx_sum[, "z"], 4),
    p.value      = round(cx_sum[, "Pr(>|z|)"], 4),
    HR_low95     = round(exp(cx_sum[, "coef"] - 1.96 * cx_sum[, "se(coef)"]), 4),
    HR_high95    = round(exp(cx_sum[, "coef"] + 1.96 * cx_sum[, "se(coef)"]), 4),
    stringsAsFactors = FALSE
  )
}
write.csv(cox_tidy, "h7_cox_model_results.csv", row.names = FALSE)
cat("Saved: h7_cox_model_results.csv\n\n")

# Save survival diagnostics
sink("h7_survival_diagnostics.txt")
cat("H7: Survival Analysis Diagnostics\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(rep("=", 60), "\n\n", sep = "")
cat(sprintf("N total = %d  |  promoted = %d  |  censored = %d\n\n",
            nrow(h7_data), sum(h7_data$event_promoted), sum(h7_data$censored)))
cat("Reference class: Very low-output (original Class 1)\n\n")
cat("--- Cox PH Model ---\n")
print(summary(cox_fit))
cat("\n--- Log-rank test ---\n")
print(logrank_test)
cat("\n--- Proportional hazards assumption (cox.zph) ---\n")
print(ph_check)
cat("\nInterpretation: p > .05 for each term indicates PH assumption is not violated.\n")
sink()
cat("Saved: h7_survival_diagnostics.txt\n\n")

# ---- 11. Plots ---------------------------------------------------------------

# Colour palette (ordered light to dark)
class_colours <- c(
  "Very low-output"        = "#9ecae1",
  "Low-to-moderate rising" = "#4393c3",
  "Moderate-to-high rising"= "#2166ac",
  "Small high-growth"      = "#053061"
)

# -- Plot 1: Boxplot + jitter of years_to_promotion (promoted faculty) --------
pdf("h7_boxplot_years_to_promotion.pdf", width = 8, height = 5.5)

p_box <- ggplot(
  h7_promoted %>% filter(!is.na(trajectory_ordered)),
  aes(x = trajectory_ordered, y = years_to_promotion,
      fill = trajectory_ordered, colour = trajectory_ordered)
) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.5, linewidth = 0.6) +
  geom_jitter(width = 0.15, height = 0, size = 2, alpha = 0.7) +
  scale_fill_manual(values = class_colours)   +
  scale_colour_manual(values = class_colours) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(
    title    = "Years to Promotion by LCGA Trajectory Class",
    subtitle = sprintf("Promoted faculty only (N = %d)", nrow(h7_promoted)),
    x        = "Trajectory class (ascending output level)",
    y        = "Career year of first promotion",
    caption  = "Boxes: IQR; horizontal line: median; points: individual faculty."
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 9),
    plot.title      = element_text(face = "bold"),
    plot.caption    = element_text(size = 8, colour = "grey50")
  )

print(p_box)
dev.off()
cat("Saved: h7_boxplot_years_to_promotion.pdf\n\n")

# -- Plot 2: Kaplan-Meier plot (all eligible faculty) -------------------------

km_fit <- survfit(
  Surv(time_to_event, event_promoted) ~ trajectory_factor,
  data = h7_data
)

pdf("h7_km_plot.pdf", width = 8.5, height = 6)

if (has_survminer) {

  p_km <- ggsurvplot(
    km_fit,
    data          = h7_data,
    fun           = "event",          # show cumulative promotion probability
    palette       = unname(class_colours),
    legend.labs   = traj_order,
    legend.title  = "Trajectory class",
    xlab          = "Career year",
    ylab          = "Cumulative probability of promotion",
    title         = "Kaplan-Meier: Time to Promotion by LCGA Trajectory Class",
    ggtheme       = theme_classic(base_size = 12),
    conf.int      = TRUE,
    conf.int.alpha= 0.10,
    risk.table    = TRUE,
    risk.table.height = 0.25,
    fontsize      = 3.5,
    tables.theme  = theme_cleantable()
  )
  print(p_km)

} else {

  # Base-R fallback
  km_cols   <- unname(class_colours)
  km_ltypes <- c(1, 2, 3, 4)

  plot(km_fit,
       fun     = "event",
       col     = km_cols,
       lty     = km_ltypes,
       lwd     = 1.8,
       xlab    = "Career year",
       ylab    = "Cumulative probability of promotion",
       main    = "Kaplan-Meier: Time to Promotion by LCGA Trajectory Class",
       bty     = "l",
       las     = 1)

  legend("topleft",
         legend = traj_order,
         col    = km_cols,
         lty    = km_ltypes,
         lwd    = 1.8,
         bty    = "n",
         cex    = 0.85)
}

dev.off()
cat("Saved: h7_km_plot.pdf\n\n")

# ---- 12. Results summary -----------------------------------------------------

# Determine H7 support
kw_sig          <- kw_test$p.value < 0.05
logrank_p       <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$obs) - 1)
logrank_sig     <- logrank_p < 0.05
any_pw_sig      <- any(pw_df$significant, na.rm = TRUE)

# Check direction: do higher-output classes have shorter time to promotion?
desc_ord <- class_desc %>%
  arrange(trajectory_ordered) %>%
  select(trajectory_label, mean_yrs_to_promo, pct_promoted)

# Spearman correlation between trajectory rank and mean years to promotion
traj_ranks <- seq_len(nrow(desc_ord))
valid_means <- desc_ord$mean_yrs_to_promo[!is.na(desc_ord$mean_yrs_to_promo)]
valid_ranks <- traj_ranks[!is.na(desc_ord$mean_yrs_to_promo)]
h7_direction_cor <- if (length(valid_means) >= 3)
  cor(valid_ranks, valid_means, method = "spearman") else NA

# Very low class promotion rate vs others
vlo_pct <- class_desc %>%
  filter(trajectory_ordered == "Very low-output") %>%
  pull(pct_promoted)
vlo_flag <- !is.na(vlo_pct) && vlo_pct > mean(class_desc$pct_promoted, na.rm = TRUE)

# Cox HRs
cox_hr_text <- paste(
  apply(cox_tidy, 1, function(row) {
    sprintf("  %s: HR = %s, 95%% CI [%s, %s], p = %s",
            row["term"], row["hazard_ratio"],
            row["HR_low95"], row["HR_high95"], row["p.value"])
  }),
  collapse = "\n"
)

sink("h7_results_summary.txt")

cat("H7 Results Summary\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(rep("=", 65), "\n\n", sep = "")

cat("HYPOTHESIS\n")
cat("H7: Faculty in higher-growth or higher-output LCGA trajectory classes\n")
cat("    reach promotion sooner than faculty in lower-output classes.\n\n")

cat("ANALYTIC SAMPLE\n")
cat(sprintf("  Total eligible faculty : %d\n", nrow(h7_data)))
if (has_never_asst && n_never_asst > 0) {
  cat(sprintf("  Excluded (never Asst Prof): %d\n", n_never_asst))
}
cat(sprintf("  Ever promoted          : %d (%.1f%%)\n",
            sum(h7_data$event_promoted),
            100 * mean(h7_data$event_promoted)))
cat(sprintf("  Promoted-only N (KW)   : %d\n\n", nrow(h7_promoted)))

cat("CLASS-LEVEL PROMOTION SUMMARY\n")
for (i in seq_len(nrow(class_desc))) {
  r <- class_desc[i, ]
  cat(sprintf("  %s (orig. Class %d, n=%d): %.1f%% promoted; ",
              r$trajectory_label, r$class_assignment, r$n_faculty, r$pct_promoted))
  if (!is.na(r$mean_yrs_to_promo)) {
    cat(sprintf("mean years to promo = %.1f (SD=%.2f, Mdn=%.1f)\n",
                r$mean_yrs_to_promo, r$sd_yrs_to_promo, r$median_yrs_to_promo))
  } else {
    cat("no promoted faculty.\n")
  }
}
cat("\n")

if (vlo_flag) {
  cat("FLAG: The Very low-output class has a higher promotion rate than the sample\n")
  cat("      average. This is counter to the simple H7 expectation that\n")
  cat("      higher-output classes should promote at higher rates.\n\n")
}

cat("KRUSKAL-WALLIS TEST (promoted faculty only)\n")
cat(sprintf("  H(%d) = %.3f, p = %.4f\n", kw_test$parameter, H_stat, kw_test$p.value))
cat(sprintf("  Epsilon-squared = %.4f\n", eps_sq))
if (kw_sig) {
  cat("  Result: SIGNIFICANT — promotion timing differs across trajectory classes.\n")
} else {
  cat("  Result: NOT SIGNIFICANT — no detectable difference in promotion timing.\n")
}
cat("\n")

cat("PAIRWISE WILCOXON TESTS (BH-adjusted, promoted faculty only)\n")
if (any_pw_sig) {
  sig_pairs <- pw_df %>% filter(significant)
  cat("  Significant pairs:\n")
  for (i in seq_len(nrow(sig_pairs))) {
    cat(sprintf("    %s vs %s: p_adj = %.4f\n",
                sig_pairs$group1[i], sig_pairs$group2[i], sig_pairs$p_adj_BH[i]))
  }
} else {
  cat("  No pairwise comparisons significant after BH correction.\n")
}
cat("\n")

cat("COX PROPORTIONAL HAZARDS MODEL (all eligible faculty)\n")
cat("  Reference class: Very low-output (original Class 1)\n")
cat(cox_hr_text, "\n\n")

cat("LOG-RANK TEST\n")
cat(sprintf("  Chi-sq = %.3f, df = %d, p = %.4f\n",
            logrank_test$chisq,
            length(logrank_test$obs) - 1,
            logrank_p))
if (logrank_sig) {
  cat("  Result: SIGNIFICANT — overall survival curves differ across classes.\n")
} else {
  cat("  Result: NOT SIGNIFICANT.\n")
}
cat("\n")

cat("PH ASSUMPTION (cox.zph)\n")
ph_df <- as.data.frame(ph_check$table)
for (i in seq_len(nrow(ph_df))) {
  cat(sprintf("  %s: rho = %.3f, chi-sq = %.3f, p = %.4f%s\n",
              rownames(ph_df)[i],
              ph_check$y[i, 1],
              ph_df[i, "chisq"],
              ph_df[i, "p"],
              if (ph_df[i, "p"] < 0.05) "  ** PH VIOLATED **" else ""))
}
cat("\n")

cat("INTERPRETATION\n")
# Build plain-English interpretation
kw_direction_note <- if (!is.na(h7_direction_cor)) {
  if (h7_direction_cor < 0) {
    "The Spearman correlation between trajectory rank and mean years to promotion is negative, indicating that higher-output classes tend to promote sooner, consistent with H7."
  } else if (h7_direction_cor > 0) {
    "The Spearman correlation between trajectory rank and mean years to promotion is positive, indicating that higher-output classes do NOT promote sooner. This is counter to H7."
  } else {
    "No clear directional trend in mean years to promotion across trajectory classes."
  }
} else { "" }

if (kw_sig && logrank_sig) {
  if (!is.na(h7_direction_cor) && h7_direction_cor < 0) {
    cat("  Both the Kruskal-Wallis test and the log-rank test are significant, and\n")
    cat("  the direction of the effect is consistent with H7: higher-output classes\n")
    cat("  show shorter time to promotion. H7 is SUPPORTED.\n")
  } else {
    cat("  Both tests are significant, but the direction of group differences\n")
    cat("  does not straightforwardly support H7. Inspect descriptives.\n")
  }
} else if (kw_sig && !logrank_sig) {
  cat("  Kruskal-Wallis is significant but the log-rank test is not. The KW test\n")
  cat("  uses only promoted faculty while Cox/log-rank includes censored faculty.\n")
  cat("  This discrepancy may reflect differential censoring across classes.\n")
} else if (!kw_sig && logrank_sig) {
  cat("  The log-rank test is significant but Kruskal-Wallis is not. This may\n")
  cat("  reflect the additional information contributed by censored non-promoted\n")
  cat("  faculty in the survival analysis.\n")
} else {
  cat("  Neither the Kruskal-Wallis test nor the log-rank test is significant.\n")
  cat("  H7 is NOT SUPPORTED: no reliable evidence that higher-output trajectory\n")
  cat("  classes reach promotion sooner.\n")
}
cat("\n")
cat(kw_direction_note, "\n\n")

if (vlo_flag) {
  cat("  NOTE: The Very low-output class has a higher promotion rate than the\n")
  cat("  sample average. This is counter to the simple H7 expectation and\n")
  cat("  warrants discussion in the manuscript.\n\n")
}

sink()

cat("Saved: h7_results_summary.txt\n\n")
cat("=============================================================\n")
cat("All H7 output files saved.\n")
cat("=============================================================\n")
