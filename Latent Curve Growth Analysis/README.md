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

## Retained solution

**4-class sqrt-linear LCGA** (BIC = 4523.2, entropy = 0.888, AvePP = 0.937)

| Class (substantive order) | n  | Label                   |
|---------------------------|----|-------------------------|
| 1 (orig. C1)              | 28 | Very low-output         |
| 2 (orig. C4)              | 25 | Low-to-moderate rising  |
| 3 (orig. C2)              | 55 | Moderate-to-high rising |
| 4 (orig. C3)              |  6 | Small high-growth       |

B=250 stability check: zero genuine assignment changes (pure label switching only).

## Data note

`lcga_data_long.csv` (faculty-year panel) is excluded from this repository.
