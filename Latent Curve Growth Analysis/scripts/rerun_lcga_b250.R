# =============================================================================
# rerun_lcga_b250.R
#
# Re-estimates the two retained 4-class LCGA models using B = 250 random
# starts instead of the original B = 50.  The goal is to confirm that the
# B = 50 solutions were not sitting in local maxima.
#
# Models estimated:
#   1. sqrt(pub_count), linear growth,    4 classes  (primary model)
#   2. sqrt(pub_count), quadratic growth, 4 classes  (closest challenger)
#
# Outputs:
#   b250_stability_comparison.csv  -- B=50 vs B=250 fit for both specs
#   b250_lin4_assignments.csv      -- class assignments from B=250 linear run
#   b250_quad4_assignments.csv     -- class assignments from B=250 quadratic run
#   b250_assignment_changes.txt    -- how many faculty (if any) changed class
#
# Usage:
#   Rscript rerun_lcga_b250.R
# =============================================================================

suppressPackageStartupMessages({
  library(lcmm)
  library(dplyr)
})

set.seed(2025)

INPUT_FILE       <- "lcga_data_long.csv"
PREV_ASSIGN_FILE <- "lcga_4class_assignments.csv"   # 4-class solution from compare_lcga_solutions.R
B_NEW            <- 250
MAXITER_PER_RUN  <- 100    # iterations per random start (original used 30)
SMALL_CLASS_PCT  <- 0.05

cat("=============================================================\n")
cat("LCGA stability check: B = 250 random starts\n")
cat("=============================================================\n\n")

# ---- 1. Load and prepare data (identical to run_lcga.R) ---------------------

df_raw <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

df <- df_raw %>%
  filter(pub_count != "" & !is.na(pub_count)) %>%
  mutate(
    pub_count   = as.numeric(pub_count),
    career_year = as.numeric(career_year),
    faculty_id  = as.integer(faculty_id)
  ) %>%
  filter(career_year >= 1)

obs_per_fac <- df %>%
  group_by(faculty_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)

df <- df %>%
  filter(faculty_id %in% obs_per_fac$faculty_id) %>%
  mutate(pub_sqrt = sqrt(pub_count))

n_total           <- length(unique(df$faculty_id))
MIN_OBS_PER_CLASS <- max(2, floor(n_total * SMALL_CLASS_PCT))

cat(sprintf("Faculty: %d  |  Observations: %d\n", n_total, nrow(df)))
cat(sprintf("Small-class threshold: %d (%.0f%% of n)\n\n",
            MIN_OBS_PER_CLASS, SMALL_CLASS_PCT * 100))

# ---- 2. Helper functions (from run_lcga.R) ----------------------------------

mval <- function(x) {
  if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x[1])
}

compute_entropy <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp  <- as.matrix(model$pprob[, -(1:2)])
  n   <- nrow(pp)
  ent <- -sum(pp * log(pp + 1e-10))
  1 - ent / (n * log(model$ng))
}

compute_ave_pp <- function(model) {
  if (model$ng == 1) return(NA_real_)
  pp    <- as.matrix(model$pprob[, -(1:2)])
  class <- model$pprob[, 2]
  mean(sapply(seq_len(nrow(pp)), function(i) pp[i, class[i]]))
}

summarise_model <- function(model, label) {
  class_sizes <- table(model$pprob[, 2])
  small       <- class_sizes[class_sizes < MIN_OBS_PER_CLASS]
  small_str   <- if (length(small) == 0) "None" else
    paste(paste0("C", names(small), "(n=", small, ")"), collapse = ", ")
  data.frame(
    spec        = label,
    log_lik     = mval(model$loglik),
    BIC         = mval(model$BIC),
    AIC         = mval(model$AIC),
    entropy     = round(compute_entropy(model), 4),
    ave_pp      = round(compute_ave_pp(model), 4),
    small_class = small_str,
    stringsAsFactors = FALSE
  )
}

# ---- 3. Anchor: 1-class model (no random starts needed) ---------------------

cat("--- Fitting 1-class anchor model ---\n")
m1 <- hlme(
  fixed   = pub_sqrt ~ career_year,
  subject = "faculty_id",
  ng      = 1,
  data    = df,
  verbose = FALSE
)
cat(sprintf("1-class BIC = %.1f\n\n", m1$BIC))

# ---- 4. B=250 run: sqrt linear 4-class (primary model) ---------------------

cat(sprintf("--- sqrt(pub) linear, 4 classes, B = %d ---\n", B_NEW))
cat("This may take several minutes …\n")

t_lin_start <- proc.time()
m_lin4 <- gridsearch(
  rep     = B_NEW,
  maxiter = MAXITER_PER_RUN,
  minit   = m1,
  hlme(
    fixed   = pub_sqrt ~ career_year,
    mixture = ~ career_year,
    subject = "faculty_id",
    ng      = 4,
    data    = df,
    verbose = FALSE
  )
)
t_lin <- (proc.time() - t_lin_start)[["elapsed"]]

cat(sprintf("Done in %.1f seconds.\n", t_lin))
cat(sprintf("  log-lik = %.4f  |  BIC = %.1f  |  entropy = %.3f  |  AvePP = %.3f\n",
            mval(m_lin4$loglik), mval(m_lin4$BIC),
            compute_entropy(m_lin4), compute_ave_pp(m_lin4)))
sizes_lin <- table(m_lin4$pprob[, 2])
cat(sprintf("  Class sizes: %s\n\n",
            paste(paste0("C", names(sizes_lin), "=", sizes_lin), collapse = ", ")))

# ---- 5. B=250 run: sqrt quadratic 4-class (sensitivity challenger) ----------

# Need a quadratic 1-class anchor for gridsearch initialisation
cat("--- Fitting quadratic 1-class anchor ---\n")
m1_quad <- hlme(
  fixed   = pub_sqrt ~ career_year + I(career_year^2),
  subject = "faculty_id",
  ng      = 1,
  data    = df,
  verbose = FALSE
)
cat(sprintf("1-class quadratic BIC = %.1f\n\n", m1_quad$BIC))

cat(sprintf("--- sqrt(pub) quadratic, 4 classes, B = %d ---\n", B_NEW))
cat("This may take several minutes …\n")

t_quad_start <- proc.time()
m_quad4 <- gridsearch(
  rep     = B_NEW,
  maxiter = MAXITER_PER_RUN,
  minit   = m1_quad,
  hlme(
    fixed   = pub_sqrt ~ career_year + I(career_year^2),
    mixture = ~ career_year + I(career_year^2),
    subject = "faculty_id",
    ng      = 4,
    data    = df,
    verbose = FALSE
  )
)
t_quad <- (proc.time() - t_quad_start)[["elapsed"]]

cat(sprintf("Done in %.1f seconds.\n", t_quad))
cat(sprintf("  log-lik = %.4f  |  BIC = %.1f  |  entropy = %.3f  |  AvePP = %.3f\n",
            mval(m_quad4$loglik), mval(m_quad4$BIC),
            compute_entropy(m_quad4), compute_ave_pp(m_quad4)))
sizes_quad <- table(m_quad4$pprob[, 2])
cat(sprintf("  Class sizes: %s\n\n",
            paste(paste0("C", names(sizes_quad), "=", sizes_quad), collapse = ", ")))

# ---- 6. Stability comparison table ------------------------------------------

# Previously reported values (from B=50 runs)
prev_lin  <- data.frame(spec="sqrt_linear_4c_B50",  log_lik=-2243.6, BIC=4523.2, AIC=4503.2,
                        entropy=0.888, ave_pp=0.937, small_class="None",
                        stringsAsFactors=FALSE)
prev_quad <- data.frame(spec="sqrt_quad_4c_B50",    log_lik=NA,      BIC=4516.2, AIC=NA,
                        entropy=0.905, ave_pp=0.956, small_class="None",
                        stringsAsFactors=FALSE)

new_lin  <- summarise_model(m_lin4,  "sqrt_linear_4c_B250")
new_quad <- summarise_model(m_quad4, "sqrt_quad_4c_B250")

stability_tbl <- bind_rows(prev_lin, new_lin, prev_quad, new_quad)
write.csv(stability_tbl, "b250_stability_comparison.csv", row.names = FALSE)

cat("=== Stability comparison ===\n")
print(stability_tbl[, c("spec","log_lik","BIC","entropy","ave_pp","small_class")],
      row.names = FALSE)
cat("\n")

# BIC delta summaries
bic_lin_delta  <- mval(m_lin4$BIC)  - prev_lin$BIC
bic_quad_delta <- mval(m_quad4$BIC) - prev_quad$BIC
bic_lin_v_quad <- mval(m_lin4$BIC)  - mval(m_quad4$BIC)

cat(sprintf("Linear   BIC change  (B50 → B250): %+.1f\n", bic_lin_delta))
cat(sprintf("Quadratic BIC change (B50 → B250): %+.1f\n", bic_quad_delta))
cat(sprintf("Linear vs Quadratic BIC gap at B250: %+.1f  (positive = linear higher/worse)\n\n",
            bic_lin_v_quad))

if (abs(bic_lin_delta) < 2) {
  cat(">> Linear model is STABLE: B=250 solution is effectively the same as B=50.\n\n")
} else if (bic_lin_delta < -2) {
  cat(">> Linear model IMPROVED: B=250 found a better solution (lower BIC).\n")
  cat("   Update reported BIC and re-run compare_lcga_solutions.R if class sizes changed.\n\n")
} else {
  cat(">> WARNING: BIC increased — check for convergence issues.\n\n")
}

# ---- 7. Save class assignments from B=250 runs ------------------------------

save_assignments <- function(model, filename) {
  pprob    <- model$pprob
  colnames(pprob)[1:2] <- c("faculty_id", "class_assignment")
  if (ncol(pprob) > 2) {
    pp_cols <- ncol(pprob) - 2
    colnames(pprob)[3:ncol(pprob)] <- paste0("pp_class", seq_len(pp_cols))
  }
  meta <- df %>%
    select(faculty_id, faculty_name) %>%
    distinct()
  out <- merge(pprob, meta, by = "faculty_id") %>%
    arrange(faculty_id)
  write.csv(out, filename, row.names = FALSE)
  invisible(out)
}

assign_lin  <- save_assignments(m_lin4,  "b250_lin4_assignments.csv")
assign_quad <- save_assignments(m_quad4, "b250_quad4_assignments.csv")

# ---- 8. Check whether any faculty changed class vs. original B=50 -----------

sink("b250_assignment_changes.txt")
cat("B=250 Stability Check — Class Assignment Changes\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(rep("=", 55), "\n\n", sep="")

if (file.exists(PREV_ASSIGN_FILE)) {
  prev_assign <- read.csv(PREV_ASSIGN_FILE, stringsAsFactors = FALSE) %>%
    select(faculty_id, class_assignment) %>%
    rename(class_b50 = class_assignment)

  comp <- assign_lin %>%
    select(faculty_id, faculty_name, class_assignment) %>%
    rename(class_b250 = class_assignment) %>%
    left_join(prev_assign, by = "faculty_id")

  # ---- Label-switching alignment ----
  # Mixture-model class numbers are arbitrary and can permute between runs.
  # Build a cross-tab and derive the one-to-one mapping from B=50 → B=250 labels.
  ct <- table(class_b50 = comp$class_b50, class_b250 = comp$class_b250)

  cat("Cross-tabulation of B=50 vs B=250 class labels:\n")
  print(ct)
  cat("\n")

  # For each B=50 class, the matching B=250 class is the one with the most
  # shared members (works perfectly for pure label switching; still robust
  # for minor within-class moves).
  label_map <- apply(ct, 1, which.max)   # named vector: b50_label -> b250_label
  cat("Label mapping (B=50 → B=250):\n")
  for (nm in names(label_map)) {
    cat(sprintf("  B50 Class %s  →  B250 Class %d\n", nm, label_map[nm]))
  }
  cat("\n")

  # Apply mapping: translate B=50 labels into B=250 label space
  comp <- comp %>%
    mutate(class_b50_remapped = label_map[as.character(class_b50)])

  # After remapping, any remaining mismatches are genuine assignment changes
  changed <- comp %>% filter(class_b50_remapped != class_b250)

  n_pure_switch <- sum(comp$class_b50 != comp$class_b250) - nrow(changed)
  cat(sprintf("Faculty with different numeric label (before alignment): %d\n",
              sum(comp$class_b50 != comp$class_b250)))
  cat(sprintf("  Of which pure label switching:          %d\n", n_pure_switch))
  cat(sprintf("  Genuine assignment changes after alignment: %d of %d\n\n",
              nrow(changed), nrow(comp)))

  if (nrow(changed) > 0) {
    cat("Faculty with genuine class changes (after label alignment):\n")
    print(changed %>% select(faculty_id, faculty_name, class_b50, class_b250,
                              class_b50_remapped),
          row.names = FALSE)
    cat("\n>> Genuine assignment changes detected. Re-run compare_lcga_solutions.R\n")
    cat("   using m_lin4 from this script to regenerate plots and descriptives.\n")
  } else {
    cat(">> STABLE: Zero genuine assignment changes after label alignment.\n")
    cat("   All apparent differences were pure class-label switching.\n")
    cat("   Existing descriptives and plots remain valid.\n")
  }
} else {
  cat("Previous assignment file not found; skipping change check.\n")
  cat("(Expected: ", PREV_ASSIGN_FILE, ")\n")
}
sink()

cat("Assignment change report written to: b250_assignment_changes.txt\n\n")

cat("=============================================================\n")
cat("All outputs saved:\n")
cat("  b250_stability_comparison.csv\n")
cat("  b250_lin4_assignments.csv\n")
cat("  b250_quad4_assignments.csv\n")
cat("  b250_assignment_changes.txt\n")
cat("=============================================================\n")
