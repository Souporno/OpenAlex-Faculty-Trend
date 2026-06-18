# Latent Curve Growth Analysis (LCGA)

Publication trajectory modeling for I/O psychology faculty at R1 universities
using `lcmm::hlme` in R.

## Folder structure

```
scripts/   R and Python scripts (in pipeline order)
results/   Model fit statistics, class assignments, descriptives
plots/     BIC curve, trajectory plots (PDF + PNG)
```

## Pipeline order

1. `scripts/fetch_openalex_pubs.py`  — pull raw publication counts from OpenAlex API
2. `scripts/patch_unresolved.py`     — resolve ambiguous author matches
3. `scripts/prepare_lcga_data.py`    — build career-year panel (lcga_data_long.csv)
4. `scripts/run_lcga.R`              — enumerate 1–11 class LCGA solutions (sqrt linear)
5. `scripts/compare_lcga_solutions.R`— compare 4-, 5-, 6-class solutions; produce plots
6. `scripts/rerun_lcga_b250.R`       — B=250 random-start stability check on 4-class solution
7. `scripts/sensitivity_lcga.R`      — sensitivity checks: sqrt-quadratic, raw-linear, raw-quadratic (4-class)
8. `scripts/explore_rawquad.R`       — raw-quadratic 1–7 class enumeration

## Retained solution

**4-class sqrt-linear LCGA** (BIC = 4523.2, entropy = 0.888, AvePP = 0.937)

| Class (substantive order) | n  | Label                   |
|---------------------------|----|-------------------------|
| 1 (orig. C1)              | 28 | Very low-output         |
| 2 (orig. C4)              | 25 | Low-to-moderate rising  |
| 3 (orig. C2)              | 55 | Moderate-to-high rising |
| 4 (orig. C3)              |  6 | Small high-growth       |

B=250 stability check: zero genuine assignment changes (pure label switching only).

## Sensitivity checks

Three alternative specifications were fit at 4 classes and compared against the
retained sqrt-linear solution (see `results/lcga_sensitivity_comparison.csv`):

- **sqrt-quadratic** — closest challenger, BIC = 4516.2 (vs. 4523.2 for linear)
- **raw-linear** — untransformed publication counts, linear growth
- **raw-quadratic** — untransformed publication counts, quadratic growth

The raw-quadratic specification was also enumerated separately across 1–7 classes
(`scripts/explore_rawquad.R`, see `results/rawquad_fit_comparison.csv`). The
quadratic term was non-significant for 96% of the sample, and only 3 classes were
viable under the 5% minimum class-size threshold — this closes the case on
raw-quadratic as a serious alternative to the retained sqrt-linear model.

## Promotion timing

A follow-up analysis examined whether the four publication trajectory classes
differ in how quickly faculty are promoted to Associate Professor. Promotion
timing was compared across classes using non-parametric tests (Kruskal-Wallis,
pairwise Wilcoxon) and a Cox proportional hazards model on time to promotion.

**Finding:** No statistically significant differences in promotion timing were
found across the four trajectory classes. Promotion rates and mean years to
promotion were broadly similar across classes, with one notable exception: the
class with the lowest overall publication output had the highest promotion
rate of any class, the opposite of what a simple output-based expectation
would predict. This pattern is discussed further in the accompanying paper.

## Data note

`lcga_data_long.csv` (faculty-year panel) is excluded from this repository.
Full results, diagnostics, and plots for all analyses are in `results/` and
`plots/`.
