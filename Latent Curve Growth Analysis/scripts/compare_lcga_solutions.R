# =============================================================================
# compare_lcga_solutions.R
#
# Fits and fully describes the 4-, 5-, and 6-class LCGA solutions separately.
# For each solution, produces:
#   lcga_{k}class_assignments.csv     -- per-faculty class + posteriors
#   lcga_{k}class_descriptives.csv    -- n, %, career length, pub rate, promotions
#   lcga_{k}class_parameters.csv      -- intercepts, slopes, p-values per class
#   lcga_{k}class_trajectories.pdf    -- trajectory plots (capped + faceted)
#
# Input:  lcga_data_long.csv  (produced by prepare_lcga_data.py)
#
# Usage:
#   Rscript compare_lcga_solutions.R
#
# Required packages:
#   install.packages(c("lcmm", "ggplot2", "dplyr", "tidyr", "scales"))
# =============================================================================

suppressPackageStartupMessages({
  library(lcmm)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

set.seed(2025)

INPUT_FILE      <- "lcga_data_long.csv"
SOLUTIONS       <- c(4, 5, 6)   # class counts to compare
N_RANDOM_STARTS <- 50

# Colorblind-friendly palettes per solution size
PALETTES <- list(
  `4` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
  `5` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
  `6` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
)

cat("=============================================================\n")
cat("Comparing LCGA Solutions: 4, 5, and 6 Classes\n")
cat("=============================================================\n\n")

# ---- 1. Load and clean data --------------------------------------------------

df_raw <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

df <- df_raw %>%
  filter(pub_count != "" & !is.na(pub_count)) %>%
  mutate(
    pub_count      = as.numeric(pub_count),
    cumulative_pub = as.numeric(cumulative_pub),
    career_year    = as.numeric(career_year),
    faculty_id     = as.integer(faculty_id)
  ) %>%
  filter(career_year >= 1)

obs_per_fac <- df %>%
  group_by(faculty_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)
df <- df %>% filter(faculty_id %in% obs_per_fac$faculty_id)
df <- df %>% mutate(pub_sqrt = sqrt(pub_count))

n_total     <- length(unique(df$faculty_id))
y_cap       <- quantile(df$pub_count, 0.95)   # cap plots at 95th percentile
career_seq  <- seq(1, max(df$career_year), by = 0.5)

cat(sprintf("Faculty in analysis: %d\n", n_total))
cat(sprintf("Career year range: %d – %d\n",
            min(df$career_year), max(df$career_year)))
cat(sprintf("Plot y-axis cap: %.0f publications (95th pct of observed data)\n\n",
            y_cap))

# ---- 2. Fit 1-class base model (needed for random starts) -------------------

cat("Fitting 1-class base model...\n")
m1 <- hlme(
  fixed   = pub_sqrt ~ career_year,
  subject = "faculty_id",
  ng      = 1,
  data    = df,
  verbose = FALSE
)
cat(sprintf("  BIC = %.1f\n\n", m1$BIC))

# ---- 3. Helper functions -----------------------------------------------------

# ---------------------------------------------------------------------------
# Parameter layout in hlme for K classes, linear growth:
#   Positions  1  to  K-1  : class-membership log-odds (NOT trajectory params)
#   Positions  K  to  2K-1 : longitudinal intercepts (one per class)
#   Positions  2K to  3K-1 : longitudinal slopes     (one per class)
#   Position   3K           : residual standard error
#
# Using positional indexing avoids the grep ambiguity that arises because
# lcmm uses the same label ("intercept class1") for both the membership
# and trajectory intercept of class 1.
# ---------------------------------------------------------------------------

# Extract class-specific trajectory predictions
get_predictions <- function(model, cy_seq) {
  K     <- model$ng
  coefs <- model$best
  rows  <- list()
  for (k in seq_len(K)) {
    int_pos   <- K + (k - 1)       # longitudinal intercept for class k
    slope_pos <- 2 * K + (k - 1)   # longitudinal slope     for class k
    b0   <- coefs[int_pos]
    b1   <- coefs[slope_pos]
    pred <- pmax(b0 + b1 * cy_seq, 0)^2   # back-transform from sqrt, floor 0
    rows[[k]] <- data.frame(
      career_year    = cy_seq,
      pub_count_pred = pred,
      class_num      = k
    )
  }
  do.call(rbind, rows)
}

# Extract parameter table with SE, Wald z, p-value, significance stars.
# summary(model)$coef fails in newer lcmm versions (returns atomic vector),
# so we reconstruct everything from model$best and model$V directly.
get_param_table <- function(model) {
  K  <- model$ng
  est <- model$best
  np  <- length(est)

  # model$V stores the lower triangle of the variance-covariance matrix
  V_mat <- matrix(0, np, np)
  V_mat[lower.tri(V_mat, diag = TRUE)] <- model$V
  V_mat <- V_mat + t(V_mat) - diag(diag(V_mat))
  ses   <- sqrt(pmax(diag(V_mat), 0))   # pmax guards tiny numerical negatives

  wald  <- est / ses
  pvals <- 2 * (1 - pnorm(abs(wald)))

  # Longitudinal parameters only: intercepts (K..2K-1) + slopes (2K..3K-1)
  long_idx <- K:(3 * K - 1)

  tbl <- data.frame(
    parameter          = names(est)[long_idx],
    coef               = round(est[long_idx],   5),
    Se                 = round(ses[long_idx],   5),
    wald_z             = round(wald[long_idx],  3),
    p_value            = round(pvals[long_idx], 5),
    stringsAsFactors   = FALSE,
    row.names          = NULL
  )

  tbl$sig <- ifelse(tbl$p_value < 0.001, "***",
              ifelse(tbl$p_value < 0.01,  "**",
              ifelse(tbl$p_value < 0.05,  "*",
              ifelse(tbl$p_value < 0.10,  ".",  ""))))

  # Intercepts back-transformed to count scale (approx starting pubs/year)
  tbl$count_scale_approx <- ifelse(
    grepl("^intercept", tbl$parameter),
    round(pmax(tbl$coef, 0)^2, 2),
    NA
  )

  tbl
}

# Compute entropy
compute_entropy <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp <- as.matrix(model$pprob[, -(1:2)])
  n  <- nrow(pp); K <- ncol(pp)
  round(1 - (-sum(pmax(pp, 1e-10) * log(pmax(pp, 1e-10))) / (n * log(K))), 4)
}

compute_ave_pp <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp    <- as.matrix(model$pprob[, -(1:2)])
  class <- model$pprob[, 2]
  round(mean(sapply(seq_len(model$ng),
                    function(k) mean(pp[class == k, k]))), 4)
}

# ---- 4. Loop over solutions --------------------------------------------------

for (k in SOLUTIONS) {

  cat(sprintf("============================================================\n"))
  cat(sprintf("  SOLUTION: %d CLASSES\n", k))
  cat(sprintf("============================================================\n"))

  # --- 4a. Fit model ----------------------------------------------------------
  cat(sprintf("  Fitting %d-class model (%d random starts)...\n",
              k, N_RANDOM_STARTS))

  mk <- gridsearch(
    rep     = N_RANDOM_STARTS,
    maxiter = 30,
    minit   = m1,
    hlme(
      fixed   = pub_sqrt ~ career_year,
      mixture = ~ career_year,
      subject = "faculty_id",
      ng      = k,
      data    = df,
      verbose = FALSE
    )
  )

  ent  <- compute_entropy(mk)
  avpp <- compute_ave_pp(mk)
  pct_threshold <- floor(n_total * 0.05)

  cat(sprintf("  BIC = %.1f | Entropy = %.3f | AvePP = %.3f\n",
              mk$BIC, ent, avpp))

  # Small class check
  class_sizes <- table(mk$pprob[, 2])
  small <- class_sizes[class_sizes < pct_threshold]
  if (length(small) > 0) {
    cat(sprintf("  Warning: class(es) below 5%% threshold (n<%d): %s\n",
                pct_threshold,
                paste(paste0("Class ", names(small),
                             " (n=", small, ")"), collapse = ", ")))
  } else {
    cat("  All classes meet the 5% threshold.\n")
  }

  # --- 4b. Class assignments --------------------------------------------------
  pprob_df <- mk$pprob
  colnames(pprob_df)[1:2] <- c("faculty_id", "class_assignment")
  if (ncol(pprob_df) > 2) {
    colnames(pprob_df)[3:ncol(pprob_df)] <-
      paste0("posterior_class_", seq_len(ncol(pprob_df) - 2))
  }

  meta <- df %>%
    distinct(faculty_id, faculty_name, university,
             career_start_year, never_asst_prof) %>%
    mutate(
      career_length  = sapply(faculty_id, function(id)
        nrow(df[df$faculty_id == id, ])),
      n_promotions   = sapply(faculty_id, function(id)
        sum(df$promoted_this_year[df$faculty_id == id])),
      mean_pub_count = sapply(faculty_id, function(id)
        mean(df$pub_count[df$faculty_id == id], na.rm = TRUE))
    )

  assignments <- merge(pprob_df, meta, by = "faculty_id") %>%
    arrange(class_assignment, faculty_name)

  assign_file <- sprintf("lcga_%dclass_assignments.csv", k)
  write.csv(assignments, assign_file, row.names = FALSE)
  cat(sprintf("  Assignments saved: %s\n", assign_file))

  # --- 4c. Descriptives -------------------------------------------------------
  desc <- assignments %>%
    group_by(class_assignment) %>%
    summarise(
      n_faculty           = n(),
      pct_of_sample       = round(n() / n_total * 100, 1),
      meets_5pct_threshold = n() >= pct_threshold,
      mean_career_length  = round(mean(career_length), 1),
      sd_career_length    = round(sd(career_length), 1),
      mean_pub_per_year   = round(mean(mean_pub_count), 2),
      sd_pub_per_year     = round(sd(mean_pub_count), 2),
      median_pub_per_year = round(median(mean_pub_count), 2),
      pct_ever_promoted   = round(mean(n_promotions > 0) * 100, 1),
      pct_multi_promoted  = round(mean(n_promotions > 1) * 100, 1),
      pct_never_asst_prof = round(mean(never_asst_prof) * 100, 1),
      .groups = "drop"
    )

  cat(sprintf("\n  Descriptives (%d classes):\n", k))
  print(desc, n = Inf)

  desc_file <- sprintf("lcga_%dclass_descriptives.csv", k)
  write.csv(desc, desc_file, row.names = FALSE)
  cat(sprintf("  Descriptives saved: %s\n", desc_file))

  # --- 4d. Parameters ---------------------------------------------------------
  cat(sprintf("\n  Model parameters (%d classes):\n", k))
  params <- get_param_table(mk)
  print(params, row.names = FALSE)

  param_file <- sprintf("lcga_%dclass_parameters.csv", k)
  write.csv(params, param_file, row.names = FALSE)
  cat(sprintf("  Parameters saved: %s\n", param_file))

  # --- 4e. Trajectory plots ---------------------------------------------------
  pred_df <- get_predictions(mk, career_seq)

  # Merge class sizes and labels
  class_n <- assignments %>%
    count(class_assignment) %>%
    mutate(
      class_num = class_assignment,
      label     = sprintf("Class %d (n=%d)", class_assignment, n)
    )

  pred_df <- pred_df %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  # Observed means per class per career year
  obs_means <- df %>%
    left_join(assignments %>% select(faculty_id, class_assignment),
              by = "faculty_id") %>%
    group_by(class_assignment, career_year) %>%
    summarise(obs_mean = mean(pub_count, na.rm = TRUE),
              n_obs    = n(),
              .groups  = "drop") %>%
    mutate(class_num = class_assignment) %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  palette <- PALETTES[[as.character(k)]]
  class_labels <- unique(pred_df$label[order(pred_df$class_num)])
  color_map <- setNames(palette, class_labels)

  pdf_file <- sprintf("lcga_%dclass_trajectories.pdf", k)
  pdf(pdf_file, width = 10, height = 7)

  # --- Plot 1: All classes, capped y-axis -------------------------------------
  p1 <- ggplot() +
    geom_line(data = pred_df,
              aes(x = career_year, y = pub_count_pred,
                  color = label, group = label),
              linewidth = 1.3) +
    geom_point(data = obs_means,
               aes(x = career_year, y = obs_mean,
                   color = label, group = label),
               size = 1.2, alpha = 0.4) +
    scale_color_manual(values = color_map) +
    coord_cartesian(ylim = c(0, y_cap)) +
    scale_x_continuous(breaks = seq(1, max(df$career_year), by = 4)) +
    labs(
      title   = sprintf("LCGA: %d-Class Solution — All Trajectories", k),
      subtitle = sprintf(
        "BIC = %.1f | Entropy = %.3f | AvePP = %.3f | n = %d faculty\nY-axis capped at %.0f (95th pct). Points = observed class means.",
        mk$BIC, ent, avpp, n_total, y_cap),
      x     = "Career Year",
      y     = "Annual Publications",
      color = "Trajectory Class"
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p1)

  # --- Plot 2: Faceted — one panel per class ----------------------------------
  p2 <- ggplot() +
    geom_ribbon(
      data = pred_df,
      aes(x = career_year,
          ymin = 0,
          ymax = pmin(pub_count_pred, y_cap * 1.5),
          fill = label),
      alpha = 0.15
    ) +
    geom_line(data = pred_df,
              aes(x = career_year, y = pub_count_pred, color = label),
              linewidth = 1.2) +
    geom_point(data = obs_means,
               aes(x = career_year, y = obs_mean),
               color = "black", size = 1, alpha = 0.5) +
    facet_wrap(~ label, scales = "free_y") +
    scale_color_manual(values = color_map, guide = "none") +
    scale_fill_manual(values  = color_map, guide = "none") +
    scale_x_continuous(breaks = seq(1, max(df$career_year), by = 8)) +
    labs(
      title    = sprintf("LCGA: %d-Class Solution — Individual Panels", k),
      subtitle = "Line = model-predicted trajectory. Points = observed class means. Y-axis free per panel.",
      x        = "Career Year",
      y        = "Annual Publications"
    ) +
    theme_bw(base_size = 11) +
    theme(plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p2)

  # --- Plot 3: Slope comparison bar chart -------------------------------------
  slope_data <- params %>%
    filter(grepl("^career_year", parameter)) %>%
    mutate(
      class_num = as.integer(gsub(".*class(\\d+)$", "\\1", parameter)),
      lower_ci  = coef - 1.96 * Se,
      upper_ci  = coef + 1.96 * Se,
      direction = ifelse(coef > 0, "Rising", "Flat / Declining"),
      sig_label = ifelse(sig %in% c("*", "**", "***"),
                         paste0(round(coef, 4), sig),
                         paste0(round(coef, 4), " (n.s.)"))
    ) %>%
    left_join(class_n %>% select(class_num, label), by = "class_num") %>%
    filter(!is.na(label))

  p3 <- ggplot(slope_data,
               aes(x = reorder(label, coef), y = coef,
                   fill = direction, color = direction)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.2, color = "black", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    geom_text(aes(label = sig_label,
                  y = ifelse(coef >= 0, upper_ci + 0.005, lower_ci - 0.005)),
              size = 3, hjust = 0.5, vjust = ifelse(slope_data$coef >= 0, 0, 1)) +
    scale_fill_manual(values  = c("Rising" = "#4DAF4A", "Flat / Declining" = "#E41A1C")) +
    scale_color_manual(values = c("Rising" = "#4DAF4A", "Flat / Declining" = "#E41A1C")) +
    coord_flip() +
    labs(
      title    = sprintf("Slope Estimates by Class — %d-Class Solution", k),
      subtitle = "Slope on sqrt(pub_count) scale. Error bars = 95% CI. n.s. = not significant (p > .05).",
      x        = NULL,
      y        = "Slope (√ scale per career year)",
      fill     = NULL, color = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p3)

  dev.off()
  cat(sprintf("  Trajectory plots saved: %s\n\n", pdf_file))
}

cat("=============================================================\n")
cat("Done. Files saved for 4-, 5-, and 6-class solutions.\n")
cat("=============================================================\n")
