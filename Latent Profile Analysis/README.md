# Latent Profile Analysis — I/O Psychology Faculty Publication Output (2025)

## What This Is

A prototype Latent Profile Analysis (LPA) run on 2025 publication data for 
34 I/O psychology faculty across 8 R1 universities. This is the first 
analytical step toward understanding publication trajectory archetypes 
across faculty careers.

Publication count is treated as a performance measure — not a predictor 
of promotion. Promotion is an event that may reshape the trajectory. This 
framing follows Feldon et al. (2019).

---

## What I Did

### Step 1 — Loaded and Cleaned Data
Loaded Faculty_Details.xlsx containing 34 faculty with annual publication 
counts from OpenAlex (2014 to 2026) and rank history coded as:
- 1 = Assistant Professor
- 2 = Associate Professor  
- 3 = Professor / Distinguished Professor

Extracted 2025 publication counts. All 34 faculty had complete data for 
this year.

### Step 2 — Descriptive Statistics
Examined the distribution of 2025 publication counts before modeling.

| Statistic | Value |
|-----------|-------|
| Mean      | 5.82  |
| Median    | 4.00  |
| SD        | 6.34  |
| Min       | 0     |
| Max       | 28    |
| n         | 34    |

The distribution is right-skewed — most faculty cluster at 0 to 5 
publications with a long tail extending to 28. High variance (SD = 6.34) 
confirmed that LPA was worth attempting on this data.

See: `outputs/histogram_2025.png`

### Step 3 — Model Selection via BIC
Ran LPA solutions with 1 through 5 classes using the tidyLPA package in R. 
Used Bayesian Information Criterion (BIC) to select the best solution — 
lower BIC means better fit.

| Classes | BIC      | Warnings        |
|---------|----------|-----------------|
| 1       | 228.13   |                 |
| 2       | 220.12   |                 |
| 3       | 227.18   | Convergence     |
| 4       | 234.24   | Convergence     |
| 5       | 241.10   | Convergence     |

**2-class solution won.** BIC was lowest at 2 classes. Solutions with 3 
or more classes produced convergence warnings — the model could not find 
stable group boundaries with only 34 faculty. This is a sample size 
constraint, not a methodological failure.

### Step 4 — LPA Results
Extracted the 2-class solution and examined group membership.

| Profile         | n  | Mean Publications | Range   |
|-----------------|----|-------------------|---------|
| Standard Output | 30 | 3.93              | 0 to 11 |
| High Output     | 4  | 20.00             | 17 to 28|

High Output faculty: Ze Mia Zhu (28), Tammy Allen (18), Chris Wiese (17), 
Louis Tay (17).

See: `outputs/lpa_2025_boxplot.png`

---

## What I Can Infer

**The pipeline works.** tidyLPA runs cleanly on this data structure. 
Column naming, faculty-level rows, and publication counts all fed into 
the model without issues. The toolchain is validated for scaling up.

**Two profiles are real and meaningful.** The separation between High 
Output (17 to 28 papers) and Standard Output (0 to 11 papers) is not 
an artifact — it reflects a genuine and substantive difference in 
annual research productivity.

**The Standard Output group is too heterogeneous.** Faculty publishing 0 
and faculty publishing 11 sit in the same class. With a larger sample, 
this group would likely split into Low (0 to 2) and Medium (3 to 8), 
giving the expected 3-class solution of Low, Medium, and High. The 
2-class result is a sample size finding.

**Rank does not cleanly predict output profile.** The High Output group 
contains faculty at three different ranks — Assistant (Ze Mia Zhu), 
Associate (Chris Wiese), and Professor (Tammy Allen, Louis Tay). This 
immediately motivates the core research question: if rank does not 
determine output profile, what does? And does output trajectory predict 
rank advancement?

**Next step:** expand the faculty sample to 100+ and run LPA on all 
years 2014 to 2025 separately to check whether consistent profile 
structure exists across years before attempting LPTA.

---

## Files
Latent Profile Analysis/
├── README.md
├── scripts/
│   ├── 01_data_loading.R
│   ├── 02_descriptive_stats.R
│   ├── 03_lpa_2025.R
│   └── 04_visualization.R
└── outputs/
├── histogram_2025.png
└── lpa_2025_boxplot.png

## Dependencies

R packages: tidyLPA, tidyverse, readxl

## Author

Soup — MS Information Management, University of Washington  
People Analytics Lab, Dr. Heather Whiteman