# =============================================================================
# sensitivity_lcga.R
#
# Sensitivity checks for the main 4-class LCGA model (sqrt, linear).
# Runs three alternative specifications and produces identical outputs for
# direct comparison:
#
#   (A) sqrt quadratic  — most important: keeps skew-reduction, relaxes linearity
#   (B) raw linear      — drops transformation, keeps straight-line assumption
#   (C) raw quadratic   — drops both constraints
#
# For each specification, outputs:
#   lcga_4class_{spec}_assignments.csv
#   lcga_4class_{spec}_descriptives.csv
#   lcga_4class_{spec}_parameters.csv
#   lcga_4class_{spec}_trajectories.pdf  (3 plots: all, faceted, slope/curve bar)
#
# Final output:
#   lcga_sensitivity_comparison.csv  — side-by-side fit stats for all 4 models
#
# Input:  lcga_data_long.csv  (from prepare_lcga_data.py)
#
# Usage:
#   Rscript sensitivity_lcga.R
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
N_CLASSES       <- 4
N_RANDOM_STARTS <- 50
SMALL_CLASS_PCT <- 0.05

# Main model reference values (from compare_lcga_solutions.R output)
MAIN_MODEL <- data.frame(
  spec        = "main",
  label       = "sqrt(pub), Linear [MAIN]",
  outcome     = "sqrt",
  poly        = "linear",
  BIC         = 4523.2,
  AIC         = 4490.4,
  entropy     = 0.888,
  ave_pp      = 0.937,
  small_class = "None",
  stringsAsFactors = FALSE
)

# Color palette (colorblind-friendly, same as compare script)
PALETTE <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")

cat("=============================================================\n")
cat("LCGA Sensitivity Checks — 4-Class Models\n")
cat("=============================================================\n\n")

# ---- 1. Load and prepare data -----------------------------------------------

df_raw <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

df_base <- df_raw %>%
  filter(pub_count != "" & !is.na(pub_count)) %>%
  mutate(
    pub_count      = as.numeric(pub_count),
    cumulative_pub = as.numeric(cumulative_pub),
    career_year    = as.numeric(career_year),
    faculty_id     = as.integer(faculty_id)
  ) %>%
  filter(career_year >= 1)

obs_per_fac <- df_base %>%
  group_by(faculty_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)
df_base <- df_base %>% filter(faculty_id %in% obs_per_fac$faculty_id)

n_total         <- length(unique(df_base$faculty_id))
MIN_CLASS_SIZE  <- max(2, floor(n_total * SMALL_CLASS_PCT))
y_cap           <- quantile(df_base$pub_count, 0.95)
career_seq      <- seq(1, max(df_base$career_year), by = 0.5)

cat(sprintf("Faculty: %d | Career year range: %d–%d\n",
            n_total, min(df_base$career_year), max(df_base$career_year)))
cat(sprintf("Y-axis cap: %.0f pubs (95th pct) | Small-class threshold: %d\n\n",
            y_cap, MIN_CLASS_SIZE))

# ---- 2. Helper functions -----------------------------------------------------

# --- 2a. Predictions from model coefficients (positional indexing) -----------
# Parameter layout in hlme (K classes):
#   Linear:    [1..K-1] membership | [K..2K-1] intercepts | [2K..3K-1] slopes | [3K] residual
#   Quadratic: [1..K-1] membership | [K..2K-1] intercepts | [2K..3K-1] lin slopes |
#              [3K..4K-1] quad slopes | [4K] residual

get_predictions <- function(model, cy_seq, poly_form) {
  K     <- model$ng
  coefs <- model$best
  rows  <- list()
  for (k in seq_len(K)) {
    b0 <- coefs[K + (k - 1)]           # intercept
    b1 <- coefs[2 * K + (k - 1)]       # linear slope
    if (poly_form == "quadratic") {
      b2  <- coefs[3 * K + (k - 1)]    # quadratic slope
      val <- b0 + b1 * cy_seq + b2 * cy_seq^2
    } else {
      val <- b0 + b1 * cy_seq
    }
    rows[[k]] <- data.frame(
      career_year   = cy_seq,
      pred_outcome  = val,              # on modeled scale (sqrt or raw)
      class_num     = k
    )
  }
  do.call(rbind, rows)
}

# --- 2b. Parameter table from model$best and model$V -------------------------
get_param_table <- function(model, poly_form) {
  K   <- model$ng
  est <- model$best
  np  <- length(est)

  V_mat <- matrix(0, np, np)
  V_mat[lower.tri(V_mat, diag = TRUE)] <- model$V
  V_mat <- V_mat + t(V_mat) - diag(diag(V_mat))
  ses   <- sqrt(pmax(diag(V_mat), 0))

  wald  <- est / ses
  pvals <- 2 * (1 - pnorm(abs(wald)))

  # Longitudinal parameters only
  long_idx <- if (poly_form == "linear") K:(3*K - 1) else K:(4*K - 1)

  tbl <- data.frame(
    parameter = names(est)[long_idx],
    coef      = round(est[long_idx], 5),
    Se        = round(ses[long_idx], 5),
    wald_z    = round(wald[long_idx], 3),
    p_value   = round(pvals[long_idx], 5),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  tbl$sig <- ifelse(tbl$p_value < 0.001, "***",
              ifelse(tbl$p_value < 0.01,  "**",
              ifelse(tbl$p_value < 0.05,  "*",
              ifelse(tbl$p_value < 0.10,  ".",  ""))))
  tbl
}

# --- 2c. Entropy and AvePP ---------------------------------------------------
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

# ---- 3. Sensitivity model specifications ------------------------------------

specs <- list(
  list(
    name    = "sqrtquad",
    label   = "sqrt(pub), Quadratic",
    outcome = "sqrt",
    poly    = "quadratic"
  ),
  list(
    name    = "rawlin",
    label   = "Raw pub, Linear",
    outcome = "raw",
    poly    = "linear"
  ),
  list(
    name    = "rawquad",
    label   = "Raw pub, Quadratic",
    outcome = "raw",
    poly    = "quadratic"
  )
)

# Collect fit stats for final comparison table
fit_rows <- list()

# ---- 4. Loop over sensitivity specifications ---------------------------------

for (spec in specs) {

  cat(sprintf("============================================================\n"))
  cat(sprintf("  SPEC: %s\n", spec$label))
  cat(sprintf("============================================================\n"))

  # --- 4a. Prepare outcome variable ------------------------------------------
  df <- df_base %>%
    mutate(outcome_var = if (spec$outcome == "sqrt") sqrt(pub_count) else pub_count)

  # --- 4b. Build formulas ----------------------------------------------------
  if (spec$poly == "linear") {
    fixed_f   <- outcome_var ~ career_year
    mixture_f <- ~ career_year
  } else {
    fixed_f   <- outcome_var ~ career_year + I(career_year^2)
    mixture_f <- ~ career_year + I(career_year^2)
  }

  # --- 4c. Fit 1-class base model --------------------------------------------
  cat("  Fitting 1-class base model...\n")
  m1 <- hlme(
    fixed   = fixed_f,
    subject = "faculty_id",
    ng      = 1,
    data    = df,
    verbose = FALSE
  )
  cat(sprintf("  Base BIC = %.1f\n", m1$BIC))

  # --- 4d. Fit 4-class model with random starts ------------------------------
  cat(sprintf("  Fitting 4-class model (%d random starts)...\n",
              N_RANDOM_STARTS))

  mk <- gridsearch(
    rep     = N_RANDOM_STARTS,
    maxiter = 30,
    minit   = m1,
    hlme(
      fixed   = fixed_f,
      mixture = mixture_f,
      subject = "faculty_id",
      ng      = N_CLASSES,
      data    = df,
      verbose = FALSE
    )
  )

  ent  <- compute_entropy(mk)
  avpp <- compute_ave_pp(mk)

  cat(sprintf("  BIC = %.1f | AIC = %.1f | Entropy = %.3f | AvePP = %.3f\n",
              mk$BIC, mk$AIC, ent, avpp))

  # Small class check
  class_sizes <- table(mk$pprob[, 2])
  small <- class_sizes[class_sizes < MIN_CLASS_SIZE]
  small_flag <- if (length(small) > 0) {
    msg <- paste(paste0("Class ", names(small), " (n=", small, ")"),
                 collapse = ", ")
    cat(sprintf("  Warning: classes below 5%% threshold: %s\n", msg))
    msg
  } else {
    cat("  All classes meet the 5% threshold.\n")
    "None"
  }

  # Store fit stats
  fit_rows[[spec$name]] <- data.frame(
    spec        = spec$name,
    label       = spec$label,
    outcome     = spec$outcome,
    poly        = spec$poly,
    BIC         = round(mk$BIC, 1),
    AIC         = round(mk$AIC, 1),
    entropy     = ent,
    ave_pp      = avpp,
    small_class = small_flag,
    stringsAsFactors = FALSE
  )

  # --- 4e. Class assignments -------------------------------------------------
  pprob_df <- mk$pprob
  colnames(pprob_df)[1:2] <- c("faculty_id", "class_assignment")
  if (ncol(pprob_df) > 2)
    colnames(pprob_df)[3:ncol(pprob_df)] <-
      paste0("posterior_class_", seq_len(ncol(pprob_df) - 2))

  meta <- df_base %>%
    distinct(faculty_id, faculty_name, university,
             career_start_year, never_asst_prof) %>%
    mutate(
      career_length  = sapply(faculty_id, function(id)
        nrow(df_base[df_base$faculty_id == id, ])),
      n_promotions   = sapply(faculty_id, function(id)
        sum(df_base$promoted_this_year[df_base$faculty_id == id])),
      mean_pub_count = sapply(faculty_id, function(id)
        mean(df_base$pub_count[df_base$faculty_id == id], na.rm = TRUE))
    )

  assignments <- merge(pprob_df, meta, by = "faculty_id") %>%
    arrange(class_assignment, faculty_name)

  assign_file <- sprintf("lcga_4class_%s_assignments.csv", spec$name)
  write.csv(assignments, assign_file, row.names = FALSE)
  cat(sprintf("  Assignments saved: %s\n", assign_file))

  # --- 4f. Descriptives ------------------------------------------------------
  desc <- assignments %>%
    group_by(class_assignment) %>%
    summarise(
      n_faculty           = n(),
      pct_of_sample       = round(n() / n_total * 100, 1),
      meets_5pct          = n() >= MIN_CLASS_SIZE,
      mean_career_length  = round(mean(career_length), 1),
      sd_career_length    = round(sd(career_length), 1),
      mean_pub_per_year   = round(mean(mean_pub_count), 2),
      sd_pub_per_year     = round(sd(mean_pub_count), 2),
      median_pub_per_year = round(median(mean_pub_count), 2),
      pct_ever_promoted   = round(mean(n_promotions > 0) * 100, 1),
      pct_multi_promoted  = round(mean(n_promotions > 1) * 100, 1),
      .groups = "drop"
    )

  cat(sprintf("\n  Descriptives (%s):\n", spec$label))
  print(desc, n = Inf)

  desc_file <- sprintf("lcga_4class_%s_descriptives.csv", spec$name)
  write.csv(desc, desc_file, row.names = FALSE)
  cat(sprintf("  Descriptives saved: %s\n", desc_file))

  # --- 4g. Parameters --------------------------------------------------------
  params <- get_param_table(mk, spec$poly)
  cat(sprintf("\n  Parameters (%s):\n", spec$label))
  print(params, row.names = FALSE)

  param_file <- sprintf("lcga_4class_%s_parameters.csv", spec$name)
  write.csv(params, param_file, row.names = FALSE)
  cat(sprintf("  Parameters saved: %s\n", param_file))

  # --- 4h. Build predictions -------------------------------------------------
  pred_raw <- get_predictions(mk, career_seq, spec$poly)

  # Back-transform to count scale
  pred_df <- pred_raw %>%
    mutate(
      pub_count_pred = if (spec$outcome == "sqrt")
        pmax(pred_outcome, 0)^2
      else
        pmax(pred_outcome, 0)
    )

  # Class labels with n
  class_n <- assignments %>%
    count(class_assignment) %>%
    mutate(
      class_num = class_assignment,
      label     = sprintf("Class %d (n=%d)", class_assignment, n)
    )
  color_map <- setNames(PALETTE, class_n$label)

  pred_df <- pred_df %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  # Observed class means
  obs_means <- df_base %>%
    left_join(assignments %>% select(faculty_id, class_assignment),
              by = "faculty_id") %>%
    group_by(class_assignment, career_year) %>%
    summarise(obs_mean = mean(pub_count, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(class_num = class_assignment) %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  class_labels_ordered <- class_n$label

  # --- 4i. Trajectory plots --------------------------------------------------
  pdf_file <- sprintf("lcga_4class_%s_trajectories.pdf", spec$name)
  pdf(pdf_file, width = 10, height = 7)

  # Plot 1: All trajectories, capped y-axis
  p1 <- ggplot() +
    geom_line(data  = pred_df,
              aes(x = career_year, y = pub_count_pred,
                  color = label, group = label),
              linewidth = 1.3) +
    geom_point(data = obs_means,
               aes(x = career_year, y = obs_mean,
                   color = label, group = label),
               size = 1.2, alpha = 0.4) +
    scale_color_manual(values = color_map) +
    coord_cartesian(ylim = c(0, y_cap)) +
    scale_x_continuous(breaks = seq(1, max(df_base$career_year), by = 4)) +
    labs(
      title    = sprintf("4-Class LCGA — %s", spec$label),
      subtitle = sprintf(
        "BIC = %.1f | AIC = %.1f | Entropy = %.3f | AvePP = %.3f\n%s | Y-axis capped at %.0f (95th pct). Points = observed class means.",
        mk$BIC, mk$AIC, ent, avpp, spec$label, y_cap),
      x     = "Career Year",
      y     = "Annual Publications",
      color = "Trajectory Class"
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p1)

  # Plot 2: Faceted panels, free y-axis
  p2 <- ggplot() +
    geom_ribbon(data = pred_df,
                aes(x = career_year,
                    ymin = 0,
                    ymax = pmin(pub_count_pred, y_cap * 2),
                    fill = label),
                alpha = 0.15) +
    geom_line(data  = pred_df,
              aes(x = career_year, y = pub_count_pred, color = label),
              linewidth = 1.2) +
    geom_point(data = obs_means,
               aes(x = career_year, y = obs_mean),
               color = "black", size = 1, alpha = 0.5) +
    facet_wrap(~ label, scales = "free_y") +
    scale_color_manual(values = color_map, guide = "none") +
    scale_fill_manual(values  = color_map, guide = "none") +
    scale_x_continuous(breaks = seq(1, max(df_base$career_year), by = 8)) +
    labs(
      title    = sprintf("4-Class LCGA — %s — Individual Panels", spec$label),
      subtitle = "Line = predicted trajectory. Points = observed class means. Y-axis free per panel.",
      x = "Career Year", y = "Annual Publications"
    ) +
    theme_bw(base_size = 11) +
    theme(plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p2)

  # Plot 3: Linear slope bar chart (for quadratic also shows linear term only;
  #         the quadratic term is shown separately as text annotation)
  slope_data <- params %>%
    filter(grepl("^career_year class[0-9]+$", parameter)) %>%   # linear term only
    mutate(
      class_num = as.integer(gsub(".*class(\\d+)$", "\\1", parameter)),
      lower_ci  = coef - 1.96 * Se,
      upper_ci  = coef + 1.96 * Se,
      direction = ifelse(coef >= 0, "Rising", "Declining"),
      sig_label = ifelse(sig %in% c("*", "**", "***"),
                         paste0(round(coef, 4), sig),
                         paste0(round(coef, 4), " (n.s.)"))
    ) %>%
    left_join(class_n %>% select(class_num, label), by = "class_num") %>%
    filter(!is.na(label))

  # For quadratic models, annotate the quadratic term on the bars
  if (spec$poly == "quadratic") {
    quad_data <- params %>%
      filter(grepl("career_year.*\\^2.*class|I.career_year", parameter)) %>%
      mutate(
        class_num   = as.integer(gsub(".*class(\\d+)$", "\\1", parameter)),
        quad_label  = sprintf("b₂=%.5f%s", coef, sig)
      ) %>%
      left_join(class_n %>% select(class_num, label), by = "class_num")
    slope_data <- slope_data %>%
      left_join(quad_data %>% select(label, quad_label), by = "label")
    subtitle_text <- "Linear slope (b₁) shown. b₂ = quadratic term annotated. Error bars = 95% CI."
  } else {
    slope_data$quad_label <- ""
    subtitle_text <- "Error bars = 95% CI. n.s. = not significant (p > .05)."
  }

  p3 <- ggplot(slope_data,
               aes(x = reorder(label, coef), y = coef,
                   fill = direction, color = direction)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.2, color = "black", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    geom_text(aes(label = sig_label,
                  y = ifelse(coef >= 0, upper_ci + 0.003,
                             lower_ci - 0.003)),
              size = 3, hjust = 0.5) +
    scale_fill_manual(values  = c("Rising" = "#4DAF4A",
                                  "Declining" = "#E41A1C")) +
    scale_color_manual(values = c("Rising" = "#4DAF4A",
                                  "Declining" = "#E41A1C")) +
    coord_flip() +
    labs(
      title    = sprintf("Linear Slope Estimates — %s", spec$label),
      subtitle = subtitle_text,
      x = NULL, y = "Linear Slope (per career year, on modeled scale)",
      fill = NULL, color = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 9, color = "grey40"))

  # Add quadratic term annotations if applicable
  if (spec$poly == "quadratic" && nrow(quad_data) > 0) {
    p3 <- p3 +
      geom_text(data = slope_data,
                aes(x = label, y = max(upper_ci, na.rm = TRUE) * 0.6,
                    label = quad_label),
                size = 2.8, color = "grey30", hjust = 0)
  }
  print(p3)

  dev.off()
  cat(sprintf("  Trajectories saved: %s\n\n", pdf_file))
}

# ---- 5. Final comparison table ----------------------------------------------

cat("============================================================\n")
cat("  SENSITIVITY COMPARISON TABLE\n")
cat("============================================================\n\n")

comparison <- bind_rows(MAIN_MODEL, bind_rows(fit_rows)) %>%
  mutate(
    delta_BIC_vs_main = round(BIC - MAIN_MODEL$BIC, 1),
    note = case_when(
      spec == "main"     ~ "Primary model",
      spec == "sqrtquad" ~ "Most important check",
      spec == "rawlin"   ~ "Secondary check",
      spec == "rawquad"  ~ "Secondary check",
      TRUE               ~ ""
    )
  )

print(comparison[, c("label", "BIC", "AIC", "entropy", "ave_pp",
                      "delta_BIC_vs_main", "small_class", "note")],
      row.names = FALSE)

write.csv(comparison, "lcga_sensitivity_comparison.csv", row.names = FALSE)
cat("\nComparison table saved: lcga_sensitivity_comparison.csv\n")

cat("\n=============================================================\n")
cat("Sensitivity checks complete.\n")
cat("Files saved: assignments, descriptives, parameters, plots\n")
cat("for sqrtquad, rawlin, rawquad specifications.\n")
cat("=============================================================\n")
