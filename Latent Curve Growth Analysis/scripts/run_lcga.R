# =============================================================================
# run_lcga.R
#
# Latent Class Growth Analysis (LCGA) of I/O faculty publication trajectories
# using the lcmm package (hlme function).
#
# Input:  lcga_data_long.csv  (produced by prepare_lcga_data.py)
# Output:
#   lcga_fit_comparison.csv      -- BIC, AIC, entropy, AvePP for 1-11 classes
#   lcga_class_assignments.csv   -- per-faculty class membership + posteriors
#   lcga_descriptives_by_class.csv -- mean pub count, career length, % promoted
#   lcga_trajectories.pdf        -- trajectory plots for best model
#   lcga_bic_plot.pdf            -- elbow plot of BIC across class solutions
#   lcga_model_summary.txt       -- detailed output for best model
#
# Usage (from terminal):
#   Rscript run_lcga.R
#
# Required packages (install once):
#   install.packages(c("lcmm", "ggplot2", "dplyr", "tidyr", "scales"))
# =============================================================================

# ---- 0. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(lcmm)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

set.seed(2025)

INPUT_FILE          <- "lcga_data_long.csv"
MAX_CLASSES         <- 11
SMALL_CLASS_PCT     <- 0.05   # flag classes smaller than 5% of sample
N_RANDOM_STARTS     <- 50     # random starting values to avoid local optima

cat("=============================================================\n")
cat("LCGA: I/O Faculty Publication Trajectories\n")
cat("=============================================================\n\n")

# ---- 1. Load data ------------------------------------------------------------

df_raw <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)
cat(sprintf("Rows loaded: %d\n", nrow(df_raw)))
cat(sprintf("Faculty loaded: %d\n", length(unique(df_raw$faculty_id))))

# Drop rows where pub_count is missing (year held position but no OpenAlex data)
# Also drop career_year < 1: some faculty held a non-Asst Prof role before
# their first Asst Prof year; LCGA starts at career year 1 by design.
df <- df_raw %>%
  filter(pub_count != "" & !is.na(pub_count)) %>%
  mutate(
    pub_count      = as.numeric(pub_count),
    cumulative_pub = as.numeric(cumulative_pub),
    career_year    = as.numeric(career_year),
    faculty_id     = as.integer(faculty_id)
  ) %>%
  filter(career_year >= 1)

n_dropped_neg <- nrow(df_raw) - nrow(df %>% bind_rows(
  df_raw %>%
    filter(pub_count == "" | is.na(pub_count)) %>%
    mutate(career_year = as.numeric(career_year))
))
neg_cy_rows <- df_raw %>%
  mutate(career_year = as.numeric(career_year)) %>%
  filter(!is.na(career_year) & career_year < 1)
if (nrow(neg_cy_rows) > 0) {
  cat(sprintf("Dropped %d rows with career_year < 1 (pre-Asst-Prof observations).\n",
              nrow(neg_cy_rows)))
  cat(sprintf("  Affected faculty: %s\n\n",
              paste(unique(neg_cy_rows$faculty_name), collapse = ", ")))
}

# Remove faculty with fewer than 3 non-missing observations after filtering
obs_per_fac <- df %>%
  group_by(faculty_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)

df <- df %>% filter(faculty_id %in% obs_per_fac$faculty_id)

n_total             <- length(unique(df$faculty_id))
MIN_OBS_PER_CLASS   <- max(2, floor(n_total * SMALL_CLASS_PCT))
cat(sprintf("Small-class warning threshold: %d (%.0f%% of n=%d)\n",
            MIN_OBS_PER_CLASS, SMALL_CLASS_PCT * 100, n_total))

cat(sprintf("Faculty with ≥3 non-missing pub_count observations: %d\n",
            length(unique(df$faculty_id))))
cat(sprintf("Career year range: %d – %d\n",
            min(df$career_year), max(df$career_year)))
cat(sprintf("Publication count range: %d – %d (mean = %.2f)\n\n",
            min(df$pub_count), max(df$pub_count), mean(df$pub_count)))

# ---- 2. Transformation -------------------------------------------------------
# Publication counts are right-skewed integers.
# We use sqrt transformation to approximate normality (better than log+1 for
# counts that include true zeros, and more interpretable than log scale).
# Results are modeled on sqrt scale; plots show back-transformed values.

df <- df %>% mutate(pub_sqrt = sqrt(pub_count))

cat("Using sqrt(pub_count) as the modeled outcome.\n\n")

# ---- 3. Polynomial degree check (on 1-class model) --------------------------
# Compare linear vs quadratic fit before deciding the growth function.

cat("--- Checking functional form (1-class models) ---\n")

m_linear <- hlme(
  fixed    = pub_sqrt ~ career_year,
  subject  = "faculty_id",
  ng       = 1,
  data     = df,
  verbose  = FALSE
)

m_quad <- hlme(
  fixed    = pub_sqrt ~ career_year + I(career_year^2),
  subject  = "faculty_id",
  ng       = 1,
  data     = df,
  verbose  = FALSE
)

cat(sprintf("Linear   — BIC: %.1f  AIC: %.1f\n",
            m_linear$BIC, m_linear$AIC))
cat(sprintf("Quadratic — BIC: %.1f  AIC: %.1f\n",
            m_quad$BIC, m_quad$AIC))

# Choose the functional form with lower BIC
if (m_quad$BIC < m_linear$BIC) {
  POLY_FORM <- "quadratic"
  FIXED_FORMULA    <- pub_sqrt ~ career_year + I(career_year^2)
  MIXTURE_FORMULA  <- ~ career_year + I(career_year^2)
  cat("Selected: quadratic growth function.\n\n")
} else {
  POLY_FORM <- "linear"
  FIXED_FORMULA    <- pub_sqrt ~ career_year
  MIXTURE_FORMULA  <- ~ career_year
  cat("Selected: linear growth function.\n\n")
}

# ---- 4. Helper: compute entropy from posterior probabilities -----------------

compute_entropy <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp <- as.matrix(model$pprob[, -(1:2)])  # drop id and class columns
  n  <- nrow(pp)
  K  <- ncol(pp)
  pp_safe <- pmax(pp, 1e-10)
  raw_entropy <- -sum(pp_safe * log(pp_safe)) / (n * log(K))
  round(1 - raw_entropy, 4)   # 1 = perfect separation, 0 = random
}

compute_ave_pp <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp    <- as.matrix(model$pprob[, -(1:2)])
  class <- model$pprob[, 2]
  ave   <- mean(sapply(seq_len(model$ng),
                       function(k) mean(pp[class == k, k])))
  round(ave, 4)
}

# ---- 5. Fit LCGA for 1 through MAX_CLASSES -----------------------------------

cat(sprintf("--- Fitting LCGA: 1 to %d classes (%s growth) ---\n",
            MAX_CLASSES, POLY_FORM))

models  <- list()
fit_tbl <- data.frame(
  n_classes = integer(),
  log_lik   = numeric(),
  AIC       = numeric(),
  BIC       = numeric(),
  entropy   = numeric(),
  ave_pp    = numeric(),
  n_params  = integer(),
  stringsAsFactors = FALSE
)

# 1-class model (no random starts needed)
cat("  Fitting 1-class model … ")
models[[1]] <- hlme(
  fixed   = FIXED_FORMULA,
  subject = "faculty_id",
  ng      = 1,
  data    = df,
  verbose = FALSE
)
cat(sprintf("BIC = %.1f\n", models[[1]]$BIC))

# NULL-safe helper: extract scalar from model field
mval <- function(x) {
  if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x[1])
}

fit_tbl <- rbind(fit_tbl, data.frame(
  n_classes = 1,
  log_lik   = mval(models[[1]]$loglik),
  AIC       = mval(models[[1]]$AIC),
  BIC       = mval(models[[1]]$BIC),
  entropy   = NA_real_,
  ave_pp    = NA_real_,
  n_params  = mval(models[[1]]$npm)
))

# 2+ class models with random starts
for (k in 2:MAX_CLASSES) {
  cat(sprintf("  Fitting %d-class model (B=%d random starts) … ",
              k, N_RANDOM_STARTS))

  m_k <- gridsearch(
    rep     = N_RANDOM_STARTS,
    maxiter = 30,
    minit   = models[[1]],
    hlme(
      fixed   = FIXED_FORMULA,
      mixture = MIXTURE_FORMULA,
      subject = "faculty_id",
      ng      = k,
      data    = df,
      verbose = FALSE
    )
  )

  models[[k]] <- m_k

  ent  <- compute_entropy(m_k)
  avpp <- compute_ave_pp(m_k)

  cat(sprintf("BIC = %.1f  entropy = %.3f  AvePP = %.3f\n",
              m_k$BIC, ifelse(is.na(ent), 0, ent),
              ifelse(is.na(avpp), 0, avpp)))

  # Warn if any class has very few members
  class_sizes <- table(m_k$pprob[, 2])
  small <- class_sizes[class_sizes < MIN_OBS_PER_CLASS]
  if (length(small) > 0) {
    cat(sprintf("    Warning: class(es) with < %d members: %s\n",
                MIN_OBS_PER_CLASS, paste(names(small), collapse=", ")))
  }

  fit_tbl <- rbind(fit_tbl, data.frame(
    n_classes = k,
    log_lik   = mval(m_k$loglik),
    AIC       = mval(m_k$AIC),
    BIC       = mval(m_k$BIC),
    entropy   = ent,
    ave_pp    = avpp,
    n_params  = mval(m_k$npm)
  ))
}

# ---- 6. Select best model ----------------------------------------------------

cat("\n--- Model fit comparison ---\n")
print(fit_tbl, digits = 4, row.names = FALSE)

# BIC-based selection (primary criterion)
best_k_bic <- fit_tbl$n_classes[which.min(fit_tbl$BIC)]

# Also flag where BIC improvement drops below 10 (plateau)
bic_diff <- diff(fit_tbl$BIC)
plateau_k <- which(abs(bic_diff) < 10)
if (length(plateau_k) > 0) {
  plateau_at <- fit_tbl$n_classes[plateau_k[1]]
  cat(sprintf("\nBIC plateaus at %d classes (delta < 10).\n", plateau_at))
}

cat(sprintf("BIC-optimal solution: %d classes.\n", best_k_bic))

# Flag if entropy < 0.80 for best model
best_entropy <- fit_tbl$entropy[fit_tbl$n_classes == best_k_bic]
if (!is.na(best_entropy) && best_entropy < 0.80) {
  cat(sprintf("Note: entropy = %.3f < 0.80 for %d-class solution.\n",
              best_entropy, best_k_bic))
  cat("Consider reporting the next lower class solution as an alternative.\n")
}

best_model <- models[[best_k_bic]]

# ---- 7. Save fit comparison table --------------------------------------------

write.csv(fit_tbl, "lcga_fit_comparison.csv", row.names = FALSE)
cat("\nFit table saved to 'lcga_fit_comparison.csv'.\n")

# ---- 8. BIC elbow plot -------------------------------------------------------

pdf("lcga_bic_plot.pdf", width = 7, height = 5)
ggplot(fit_tbl, aes(x = n_classes, y = BIC)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_point(color = "#2c7bb6", size = 3) +
  geom_vline(xintercept = best_k_bic, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  annotate("text", x = best_k_bic + 0.2, y = max(fit_tbl$BIC),
           label = sprintf("Selected: %d classes", best_k_bic),
           hjust = 0, color = "red", size = 3.5) +
  scale_x_continuous(breaks = 1:MAX_CLASSES) +
  labs(title = "LCGA Model Selection: BIC by Number of Classes",
       x = "Number of Classes", y = "BIC") +
  theme_bw(base_size = 13)
dev.off()
cat("BIC elbow plot saved to 'lcga_bic_plot.pdf'.\n")

# ---- 9. Class assignments ----------------------------------------------------

pprob_df <- best_model$pprob
colnames(pprob_df)[1:2] <- c("faculty_id", "class_assignment")

# Add posterior probability columns with clear names
if (ncol(pprob_df) > 2) {
  pp_cols <- ncol(pprob_df) - 2
  colnames(pprob_df)[3:ncol(pprob_df)] <-
    paste0("posterior_class_", seq_len(pp_cols))
}

# Merge with faculty metadata (name, university)
meta <- df %>%
  distinct(faculty_id, faculty_name, university,
           career_start_year, never_asst_prof) %>%
  mutate(
    career_length   = sapply(faculty_id, function(id)
      nrow(df[df$faculty_id == id, ])),
    n_promotions    = sapply(faculty_id, function(id)
      sum(df$promoted_this_year[df$faculty_id == id])),
    mean_pub_count  = sapply(faculty_id, function(id)
      mean(df$pub_count[df$faculty_id == id], na.rm = TRUE))
  )

assignments <- merge(pprob_df, meta, by = "faculty_id")
assignments <- assignments[order(assignments$class_assignment,
                                 assignments$faculty_name), ]

write.csv(assignments, "lcga_class_assignments.csv", row.names = FALSE)
cat("Class assignments saved to 'lcga_class_assignments.csv'.\n")

# ---- 10. Descriptives by class -----------------------------------------------

desc <- assignments %>%
  group_by(class_assignment) %>%
  summarise(
    n_faculty          = n(),
    pct_of_sample      = round(n() / nrow(assignments) * 100, 1),
    mean_career_length = round(mean(career_length), 1),
    mean_pub_per_year  = round(mean(mean_pub_count), 2),
    pct_ever_promoted  = round(mean(n_promotions > 0) * 100, 1),
    pct_never_asst     = round(mean(never_asst_prof) * 100, 1),
    .groups = "drop"
  )

cat("\n--- Descriptives by class ---\n")
print(desc, row.names = FALSE)
write.csv(desc, "lcga_descriptives_by_class.csv", row.names = FALSE)
cat("Descriptives saved to 'lcga_descriptives_by_class.csv'.\n")

# ---- 11. Trajectory plots ----------------------------------------------------

# Predict trajectories by extracting class-specific coefficients directly.
# This avoids predictY's formula-parsing bug when formulas are stored as
# variables rather than written inline in the hlme() call.

career_year_seq <- seq(min(df$career_year), max(df$career_year), by = 0.5)

# Print parameter names so they are visible in the console for reference
cat("\nModel parameters (best model):\n")
param_names <- names(best_model$best)
print(data.frame(parameter = param_names,
                 estimate  = round(best_model$best, 4)),
      row.names = FALSE)

extract_class_pred <- function(model, cy_seq, poly_form) {
  ng    <- model$ng
  coefs <- model$best
  cnames <- names(coefs)
  rows  <- list()

  for (k in seq_len(ng)) {
    # lcmm names class-specific params as "term class k" (with space)
    if (ng == 1) {
      int_pat   <- "^intercept$"
      slope_pat <- "^career_year$"
      quad_pat  <- "career_year.2"
    } else {
      int_pat   <- paste0("intercept class", k, "$")
      slope_pat <- paste0("(^|\\s)career_year class", k, "$")
      quad_pat  <- paste0("career_year.2.*class", k)
    }

    int_idx   <- grep(int_pat,   cnames)
    slope_idx <- grep(slope_pat, cnames)

    if (length(int_idx) == 0 || length(slope_idx) == 0) {
      # Fallback: positional extraction (intercept first, slope second per class)
      base <- (k - 1) * ifelse(poly_form == "quadratic", 3, 2)
      int_idx   <- base + 1
      slope_idx <- base + 2
    }

    b0 <- coefs[int_idx[1]]
    b1 <- coefs[slope_idx[1]]

    if (poly_form == "quadratic") {
      quad_idx <- grep(quad_pat, cnames)
      b2 <- if (length(quad_idx) > 0) coefs[quad_idx[1]] else 0
      pred_sqrt <- b0 + b1 * cy_seq + b2 * cy_seq^2
    } else {
      pred_sqrt <- b0 + b1 * cy_seq
    }

    rows[[k]] <- data.frame(
      career_year   = cy_seq,
      pub_sqrt_pred = pred_sqrt,
      class         = paste("Class", k)
    )
  }
  do.call(rbind, rows)
}

pred_long <- extract_class_pred(best_model, career_year_seq, POLY_FORM) %>%
  mutate(pub_count_pred = pmax(pub_sqrt_pred, 0)^2)   # back-transform, floor 0

# Merge class sizes for legend labels
class_n <- assignments %>%
  count(class_assignment) %>%
  mutate(class = paste("Class", class_assignment),
         label = paste0(class, " (n=", n, ")"))

pred_long <- pred_long %>%
  left_join(class_n %>% select(class, label), by = "class")

# Color palette (colorblind-friendly)
palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
             "#FF7F00", "#A65628", "#F781BF")[seq_len(best_k_bic)]

pdf("lcga_trajectories.pdf", width = 9, height = 6)

p <- ggplot(pred_long, aes(x = career_year, y = pub_count_pred,
                            color = label, group = label)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = setNames(palette,
                                       unique(pred_long$label[
                                         order(pred_long$class)]))) +
  scale_x_continuous(breaks = seq(1, max(df$career_year), by = 2)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title   = sprintf("LCGA Publication Trajectories (%d Classes, %s growth)",
                      best_k_bic, POLY_FORM),
    x       = "Career Year",
    y       = "Predicted Annual Publications",
    color   = "Trajectory Class",
    caption = "Predictions back-transformed from sqrt scale."
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 9, color = "grey50"))

print(p)

# Also plot observed mean trajectories per class alongside predicted
obs_means <- df %>%
  left_join(assignments %>% select(faculty_id, class_assignment),
            by = "faculty_id") %>%
  group_by(class_assignment, career_year) %>%
  summarise(obs_mean = mean(pub_count, na.rm = TRUE),
            n        = n(),
            .groups  = "drop") %>%
  mutate(class = paste("Class", class_assignment)) %>%
  left_join(class_n %>% select(class, label), by = "class")

p2 <- ggplot() +
  geom_point(data = obs_means,
             aes(x = career_year, y = obs_mean, color = label),
             size = 1.5, alpha = 0.5) +
  geom_line(data = pred_long,
            aes(x = career_year, y = pub_count_pred,
                color = label, group = label),
            linewidth = 1.2) +
  scale_color_manual(values = setNames(palette,
                                       unique(pred_long$label[
                                         order(pred_long$class)]))) +
  scale_x_continuous(breaks = seq(1, max(df$career_year), by = 2)) +
  labs(
    title   = "Observed Means (points) vs. LCGA Predicted Trajectories (lines)",
    x       = "Career Year",
    y       = "Annual Publications",
    color   = "Trajectory Class"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")

print(p2)
dev.off()
cat("Trajectory plots saved to 'lcga_trajectories.pdf'.\n")

# ---- 12. Model summary text --------------------------------------------------

sink("lcga_model_summary.txt")
cat("=============================================================\n")
cat(sprintf("LCGA Model Summary — %d Classes, %s growth\n",
            best_k_bic, POLY_FORM))
cat("=============================================================\n\n")
cat("Fit comparison across all solutions:\n")
print(fit_tbl, row.names = FALSE)
cat("\n\nBest model coefficients:\n")
print(summary(best_model))
cat("\n\nClass descriptives:\n")
print(desc, row.names = FALSE)
sink()
cat("Model summary saved to 'lcga_model_summary.txt'.\n")

cat("\n=============================================================\n")
cat("LCGA complete.\n")
cat(sprintf("Selected model: %d classes (%s growth)\n", best_k_bic, POLY_FORM))
cat(sprintf("BIC = %.1f   Entropy = %.3f\n",
            fit_tbl$BIC[fit_tbl$n_classes == best_k_bic],
            ifelse(is.na(best_entropy), NA, best_entropy)))
cat("=============================================================\n")
