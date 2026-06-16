# =============================================================================
# explore_rawquad.R
#
# Explores raw pub_count with quadratic growth across 1–7 classes.
# Identical structure to run_lcga.R but fixed to: outcome = raw, poly = quadratic.
#
# Outputs:
#   rawquad_fit_comparison.csv        -- BIC, AIC, entropy, AvePP for 1–7 classes
#   rawquad_bic_plot.pdf              -- elbow plot
#   rawquad_{k}class_assignments.csv  -- for every viable k (no n=1 class)
#   rawquad_{k}class_descriptives.csv
#   rawquad_{k}class_parameters.csv
#   rawquad_{k}class_trajectories.pdf
#
# Input:  lcga_data_long.csv
#
# Usage:
#   Rscript explore_rawquad.R
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
MAX_CLASSES     <- 7
N_RANDOM_STARTS <- 50
SMALL_CLASS_PCT <- 0.05
PALETTE         <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                     "#FF7F00", "#A65628", "#F781BF")

cat("=============================================================\n")
cat("Raw pub_count — Quadratic Growth: 1 to", MAX_CLASSES, "Classes\n")
cat("=============================================================\n\n")

# ---- 1. Load data ------------------------------------------------------------

df_raw <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

df <- df_raw %>%
  filter(pub_count != "" & !is.na(pub_count)) %>%
  mutate(
    pub_count      = as.numeric(pub_count),
    career_year    = as.numeric(career_year),
    faculty_id     = as.integer(faculty_id)
  ) %>%
  filter(career_year >= 1)

obs_per_fac <- df %>%
  group_by(faculty_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)
df <- df %>% filter(faculty_id %in% obs_per_fac$faculty_id)

n_total        <- length(unique(df$faculty_id))
MIN_CLASS_SIZE <- max(2, floor(n_total * SMALL_CLASS_PCT))
y_cap          <- quantile(df$pub_count, 0.95)
career_seq     <- seq(1, max(df$career_year), by = 0.5)

cat(sprintf("Faculty: %d | Career years: %d–%d\n",
            n_total, min(df$career_year), max(df$career_year)))
cat(sprintf("Y-axis cap: %.0f (95th pct) | Small-class floor: %d (%.0f%%)\n\n",
            y_cap, MIN_CLASS_SIZE, SMALL_CLASS_PCT * 100))

# ---- 2. Helpers --------------------------------------------------------------

# Parameter layout for K classes, quadratic growth:
#   [1..K-1]  membership log-odds
#   [K..2K-1] longitudinal intercepts
#   [2K..3K-1] linear slopes
#   [3K..4K-1] quadratic slopes
#   [4K]      residual SE

get_predictions <- function(model, cy_seq) {
  K     <- model$ng
  coefs <- model$best
  rows  <- list()
  for (k in seq_len(K)) {
    b0  <- coefs[K + (k - 1)]
    b1  <- coefs[2 * K + (k - 1)]
    b2  <- coefs[3 * K + (k - 1)]
    val <- pmax(b0 + b1 * cy_seq + b2 * cy_seq^2, 0)  # floor at 0
    rows[[k]] <- data.frame(career_year = cy_seq,
                            pub_pred    = val,
                            class_num   = k)
  }
  do.call(rbind, rows)
}

get_param_table <- function(model) {
  K   <- model$ng
  est <- model$best
  np  <- length(est)
  V_mat <- matrix(0, np, np)
  V_mat[lower.tri(V_mat, diag = TRUE)] <- model$V
  V_mat <- V_mat + t(V_mat) - diag(diag(V_mat))
  ses   <- sqrt(pmax(diag(V_mat), 0))
  wald  <- est / ses
  pvals <- 2 * (1 - pnorm(abs(wald)))
  long_idx <- K:(4 * K - 1)   # intercepts + linear slopes + quad slopes
  tbl <- data.frame(
    parameter = names(est)[long_idx],
    coef      = round(est[long_idx], 5),
    Se        = round(ses[long_idx], 5),
    wald_z    = round(wald[long_idx], 3),
    p_value   = round(pvals[long_idx], 5),
    stringsAsFactors = FALSE, row.names = NULL
  )
  tbl$sig <- ifelse(tbl$p_value < 0.001, "***",
              ifelse(tbl$p_value < 0.01,  "**",
              ifelse(tbl$p_value < 0.05,  "*",
              ifelse(tbl$p_value < 0.10,  ".", ""))))
  tbl
}

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

# ---- 3. Fit 1-class base model -----------------------------------------------

cat("Fitting 1-class base model...\n")
m1 <- hlme(
  fixed   = pub_count ~ career_year + I(career_year^2),
  subject = "faculty_id",
  ng      = 1,
  data    = df,
  verbose = FALSE
)
cat(sprintf("  BIC = %.1f\n\n", m1$BIC))

# ---- 4. Fit 1 through MAX_CLASSES --------------------------------------------

models  <- list(m1)
fit_tbl <- data.frame(
  n_classes   = 1L,
  log_lik     = round(m1$loglik, 2),
  AIC         = round(m1$AIC, 1),
  BIC         = round(m1$BIC, 1),
  entropy     = NA_real_,
  ave_pp      = NA_real_,
  small_class = "—",
  viable      = TRUE,
  stringsAsFactors = FALSE
)

for (k in 2:MAX_CLASSES) {
  cat(sprintf("Fitting %d-class model (%d random starts)...\n",
              k, N_RANDOM_STARTS))

  mk <- gridsearch(
    rep     = N_RANDOM_STARTS,
    maxiter = 30,
    minit   = m1,
    hlme(
      fixed   = pub_count ~ career_year + I(career_year^2),
      mixture = ~ career_year + I(career_year^2),
      subject = "faculty_id",
      ng      = k,
      data    = df,
      verbose = FALSE
    )
  )

  models[[k]] <- mk
  ent  <- compute_entropy(mk)
  avpp <- compute_ave_pp(mk)

  # Class size check
  class_sizes <- table(mk$pprob[, 2])
  small       <- class_sizes[class_sizes < MIN_CLASS_SIZE]
  singleton   <- class_sizes[class_sizes == 1]
  viable      <- length(singleton) == 0

  small_flag <- if (length(small) > 0)
    paste(paste0("C", names(small), "(n=", small, ")"), collapse = ", ")
  else "None"

  cat(sprintf("  BIC = %.1f | Entropy = %.3f | AvePP = %.3f | Small: %s%s\n",
              mk$BIC, ent, avpp, small_flag,
              ifelse(!viable, " ← SINGLETON CLASS", "")))

  fit_tbl <- rbind(fit_tbl, data.frame(
    n_classes   = k,
    log_lik     = round(mk$loglik, 2),
    AIC         = round(mk$AIC, 1),
    BIC         = round(mk$BIC, 1),
    entropy     = ent,
    ave_pp      = avpp,
    small_class = small_flag,
    viable      = viable,
    stringsAsFactors = FALSE
  ))
}

# ---- 5. Fit summary ----------------------------------------------------------

cat("\n--- Fit comparison (raw pub_count, quadratic) ---\n")
print(fit_tbl, row.names = FALSE)
write.csv(fit_tbl, "rawquad_fit_comparison.csv", row.names = FALSE)

# BIC deltas within this specification
bic_diffs <- diff(fit_tbl$BIC)
cat("\nBIC deltas (k vs k-1):\n")
for (i in seq_along(bic_diffs)) {
  cat(sprintf("  %d→%d: %.1f\n", i, i + 1, bic_diffs[i]))
}

best_viable_k <- fit_tbl %>%
  filter(viable) %>%
  slice_min(BIC, n = 1) %>%
  pull(n_classes)
cat(sprintf("\nBIC-optimal viable solution: %d classes\n\n", best_viable_k))

# ---- 6. BIC elbow plot -------------------------------------------------------

pdf("rawquad_bic_plot.pdf", width = 8, height = 5)
ggplot(fit_tbl, aes(x = n_classes, y = BIC,
                    shape = viable, color = viable)) +
  geom_line(color = "grey50", linewidth = 0.8) +
  geom_point(size = 4) +
  geom_vline(xintercept = best_viable_k, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  scale_shape_manual(values = c(`FALSE` = 4, `TRUE` = 16),
                     labels = c("Singleton class", "Viable")) +
  scale_color_manual(values = c(`FALSE` = "#E41A1C", `TRUE` = "#377EB8"),
                     labels = c("Singleton class", "Viable")) +
  scale_x_continuous(breaks = 1:MAX_CLASSES) +
  annotate("text", x = best_viable_k + 0.15,
           y = max(fit_tbl$BIC) * 0.99,
           label = sprintf("Best viable: %d classes", best_viable_k),
           hjust = 0, color = "red", size = 3.5) +
  labs(title = "Raw pub_count, Quadratic Growth — BIC by Class Count",
       subtitle = "× = solution with singleton class (n=1), not viable",
       x = "Number of Classes", y = "BIC",
       shape = NULL, color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom")
dev.off()
cat("BIC plot saved: rawquad_bic_plot.pdf\n\n")

# ---- 7. Full outputs for each viable solution --------------------------------

meta <- df %>%
  distinct(faculty_id) %>%
  left_join(
    read.csv(INPUT_FILE, stringsAsFactors = FALSE) %>%
      filter(!is.na(pub_count) & pub_count != "") %>%
      mutate(faculty_id = as.integer(faculty_id),
             career_year = as.numeric(career_year)) %>%
      filter(career_year >= 1) %>%
      distinct(faculty_id, faculty_name, university,
               career_start_year, never_asst_prof) %>%
      group_by(faculty_id) %>% slice(1) %>% ungroup(),
    by = "faculty_id"
  ) %>%
  mutate(
    career_length  = sapply(faculty_id, function(id)
      nrow(df[df$faculty_id == id, ])),
    n_promotions   = sapply(faculty_id, function(id)
      sum(df$promoted_this_year[df$faculty_id == id])),
    mean_pub_count = sapply(faculty_id, function(id)
      mean(df$pub_count[df$faculty_id == id], na.rm = TRUE))
  )

for (k in fit_tbl$n_classes[fit_tbl$viable & fit_tbl$n_classes >= 2]) {

  mk <- models[[k]]
  ent  <- fit_tbl$entropy[fit_tbl$n_classes == k]
  avpp <- fit_tbl$ave_pp[fit_tbl$n_classes == k]

  cat(sprintf("--- Outputs for %d-class solution ---\n", k))

  # Assignments
  pprob_df <- mk$pprob
  colnames(pprob_df)[1:2] <- c("faculty_id", "class_assignment")
  if (ncol(pprob_df) > 2)
    colnames(pprob_df)[3:ncol(pprob_df)] <-
      paste0("posterior_class_", seq_len(ncol(pprob_df) - 2))

  assignments <- merge(pprob_df, meta, by = "faculty_id") %>%
    arrange(class_assignment, faculty_name)

  write.csv(assignments,
            sprintf("rawquad_%dclass_assignments.csv", k),
            row.names = FALSE)

  # Descriptives
  desc <- assignments %>%
    group_by(class_assignment) %>%
    summarise(
      n_faculty           = n(),
      pct_of_sample       = round(n() / n_total * 100, 1),
      meets_5pct          = n() >= MIN_CLASS_SIZE,
      mean_career_length  = round(mean(career_length), 1),
      mean_pub_per_year   = round(mean(mean_pub_count), 2),
      sd_pub_per_year     = round(sd(mean_pub_count), 2),
      median_pub_per_year = round(median(mean_pub_count), 2),
      pct_ever_promoted   = round(mean(n_promotions > 0) * 100, 1),
      pct_multi_promoted  = round(mean(n_promotions > 1) * 100, 1),
      .groups = "drop"
    )

  cat("  Descriptives:\n")
  print(desc, n = Inf)
  write.csv(desc,
            sprintf("rawquad_%dclass_descriptives.csv", k),
            row.names = FALSE)

  # Parameters
  params <- get_param_table(mk)
  cat("  Parameters:\n")
  print(params, row.names = FALSE)
  write.csv(params,
            sprintf("rawquad_%dclass_parameters.csv", k),
            row.names = FALSE)

  # Predictions
  pred_df <- get_predictions(mk, career_seq)
  class_n <- assignments %>%
    count(class_assignment) %>%
    mutate(class_num = class_assignment,
           label = sprintf("Class %d (n=%d)", class_assignment, n))
  palette_k <- PALETTE[seq_len(k)]
  color_map <- setNames(palette_k, class_n$label)

  pred_df <- pred_df %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  obs_means <- df %>%
    left_join(assignments %>% select(faculty_id, class_assignment),
              by = "faculty_id") %>%
    group_by(class_assignment, career_year) %>%
    summarise(obs_mean = mean(pub_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(class_num = class_assignment) %>%
    left_join(class_n %>% select(class_num, label), by = "class_num")

  pdf_file <- sprintf("rawquad_%dclass_trajectories.pdf", k)
  pdf(pdf_file, width = 10, height = 7)

  # Plot 1: All trajectories, capped y-axis
  print(
    ggplot() +
      geom_line(data = pred_df,
                aes(x = career_year, y = pub_pred,
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
        title    = sprintf("Raw pub_count, Quadratic — %d-Class Solution", k),
        subtitle = sprintf(
          "BIC = %.1f | AIC = %.1f | Entropy = %.3f | AvePP = %.3f\nY-axis capped at %.0f (95th pct). Points = observed class means.",
          fit_tbl$BIC[fit_tbl$n_classes == k],
          fit_tbl$AIC[fit_tbl$n_classes == k],
          ent, avpp, y_cap),
        x = "Career Year", y = "Annual Publications", color = NULL
      ) +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom",
            plot.subtitle = element_text(size = 9, color = "grey40"))
  )

  # Plot 2: Faceted, free y-axis
  print(
    ggplot() +
      geom_ribbon(data = pred_df,
                  aes(x = career_year, ymin = 0,
                      ymax = pmin(pub_pred, y_cap * 2), fill = label),
                  alpha = 0.15) +
      geom_line(data = pred_df,
                aes(x = career_year, y = pub_pred, color = label),
                linewidth = 1.2) +
      geom_point(data = obs_means,
                 aes(x = career_year, y = obs_mean),
                 color = "black", size = 1, alpha = 0.5) +
      facet_wrap(~ label, scales = "free_y") +
      scale_color_manual(values = color_map, guide = "none") +
      scale_fill_manual(values  = color_map, guide = "none") +
      scale_x_continuous(breaks = seq(1, max(df$career_year), by = 8)) +
      labs(
        title    = sprintf("Raw pub_count, Quadratic — %d Classes — Individual Panels", k),
        subtitle = "Line = predicted. Points = observed class means. Y-axis free per panel.",
        x = "Career Year", y = "Annual Publications"
      ) +
      theme_bw(base_size = 11) +
      theme(plot.subtitle = element_text(size = 9, color = "grey40"))
  )

  # Plot 3: Trajectory shape summary — peak year per class
  # For each class, find where d(trajectory)/d(t) = 0 → b1 + 2*b2*t = 0 → t = -b1/(2*b2)
  shape_rows <- list()
  for (cls in seq_len(k)) {
    b0 <- mk$best[k + (cls - 1)]
    b1 <- mk$best[2 * k + (cls - 1)]
    b2 <- mk$best[3 * k + (cls - 1)]
    lbl <- class_n$label[class_n$class_num == cls]
    if (abs(b2) > 1e-8) {
      peak_t <- -b1 / (2 * b2)
      shape  <- ifelse(b2 < 0, "Inverted-U (peak & decline)",
                       "U-shape (dip & rise)")
    } else {
      peak_t <- NA
      shape  <- ifelse(b1 > 0, "Linear rising", "Linear declining")
    }
    shape_rows[[cls]] <- data.frame(
      class = lbl, b1 = round(b1, 4), b2 = round(b2, 5),
      shape = shape,
      peak_or_trough_year = round(peak_t, 1),
      stringsAsFactors = FALSE
    )
  }
  shape_df <- do.call(rbind, shape_rows)
  cat("  Trajectory shapes:\n")
  print(shape_df, row.names = FALSE)

  p3 <- ggplot(shape_df, aes(x = reorder(class, b2), y = b2,
                              fill = shape, color = shape)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    geom_text(aes(label = ifelse(!is.na(peak_or_trough_year),
                                 sprintf("peak/trough yr %.0f", peak_or_trough_year),
                                 shape),
                  y = ifelse(b2 >= 0, b2 + max(abs(b2)) * 0.05,
                             b2 - max(abs(b2)) * 0.05)),
              size = 3, hjust = 0.5) +
    scale_fill_manual(values  = c("Inverted-U (peak & decline)" = "#E41A1C",
                                  "U-shape (dip & rise)"        = "#377EB8",
                                  "Linear rising"               = "#4DAF4A",
                                  "Linear declining"            = "#984EA3")) +
    scale_color_manual(values = c("Inverted-U (peak & decline)" = "#E41A1C",
                                  "U-shape (dip & rise)"        = "#377EB8",
                                  "Linear rising"               = "#4DAF4A",
                                  "Linear declining"            = "#984EA3")) +
    coord_flip() +
    labs(
      title    = sprintf("Quadratic Term (b₂) by Class — %d-Class Solution", k),
      subtitle = "Negative b₂ = inverted-U (peak then decline). Positive b₂ = U-shape (dip then rise).\nLabel shows career year of peak (inverted-U) or trough (U-shape).",
      x = NULL, y = "Quadratic term b₂", fill = NULL, color = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 9, color = "grey40"))
  print(p3)

  dev.off()
  cat(sprintf("  Trajectories saved: rawquad_%dclass_trajectories.pdf\n\n", k))
}

cat("=============================================================\n")
cat("Done. Check rawquad_fit_comparison.csv and BIC plot first.\n")
cat("Then open trajectory PDFs for each viable solution.\n")
cat("=============================================================\n")
