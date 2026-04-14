# OpenAlex Faculty Trend

An exploratory data project analyzing publication trends of I/O Psychology faculty at R1 universities, and examining whether publication output correlates with academic promotion.

---

## Methodology

### Step 1 — Identifying R1 Universities with I/O Psychology Programs

The starting universe was all U.S. doctoral I/O Psychology programs. To ensure research quality and meaningful publication output, the list was filtered down using two criteria applied in intersection:

**Criterion A — Carnegie R1 Classification**
Only universities classified as *Research 1: Very High Research Spending and Doctorate Production* were considered, as defined by the American Council on Education's Carnegie Classifications (2025 update).
→ [Carnegie Classifications Directory](https://carnegieclassifications.acenet.edu/institutions/?inst&research2025%5B0%5D=1)

**Criterion B — SIOP Graduate Training Program Directory**
Only programs listed in the Society for Industrial and Organizational Psychology (SIOP) Graduate Training Program (GTP) portal were considered. This directory is maintained by SIOP and serves as a field-recognized index of active doctoral I/O programs.
→ [SIOP GTP Portal](https://portal.siop.org/graduate-training-program)

**The intersection** of these two criteria produced a refined set of institutions that are both research-intensive (R1) and field-recognized (SIOP-listed) I/O Psychology doctoral programs.

---

### Step 2 — Faculty Selection

From the institutions in the intersection, faculty details were manually collected from each program's official department page. Collection continued until a statistically meaningful sample of **at least 30 faculty** was reached. This threshold was chosen as a practical minimum for trend analysis.

The final dataset contains **34 faculty members** across **8 institutions**, with the following fields recorded for each:

- Institution and abbreviation
- Faculty name and current rank
- Email and source URL
- Google Scholar profile (where available)
- Year-by-year academic rank/title (2014–2026), encoded numerically as:
  - `1` = Assistant Professor
  - `2` = Associate Professor
  - `3` = Full Professor

This data is stored in `Faculty Details Completed.xlsx`.

---

### Step 3 — Pulling Publication Data via OpenAlex

A Python script (`faculty_openalex.py`) was developed to query the [OpenAlex API](https://openalex.org/) for each of the 34 faculty members and retrieve their **annual publication counts from 2014 to 2026**.

**How the script works:**
1. Reads faculty names and institutions from the Excel file
2. Searches OpenAlex by faculty name, disambiguating matches using institution affiliation scoring
3. Retrieves publication counts grouped by year using OpenAlex's `group_by=publication_year` endpoint
4. Flags each match with a confidence level — HIGH, MEDIUM, or LOW — so uncertain matches can be manually verified
5. Outputs results to `faculty_publications_openalex.xlsx`

**To run the script:**
```bash
pip3 install requests pandas openpyxl
python3 faculty_openalex.py
```

> Note: The script uses the OpenAlex polite pool (via a registered email) for faster and more stable API access.

---

### Step 4 — Preparing Data for Visualization

The publication counts (wide format, one column per year) and the rank data were merged and reshaped into **long format** — one row per faculty per year — suitable for Tableau. A `Promoted_This_Year` flag was also computed (value = 1 in any year where a faculty member's rank increased).

This Tableau-ready dataset is stored in `faculty_tableau_ready.csv` with the following columns:

| Column | Description |
|---|---|
| Faculty Name | Full name |
| Institution | University name |
| Abbreviation | Short institution code |
| Year | Calendar year (2014–2026) |
| Rank_Level | Numeric rank (1 = Assistant, 2 = Associate, 3 = Full) |
| Rank_Label | Text rank label |
| Publications | OpenAlex publication count for that year |
| OpenAlex_ID | Author identifier in OpenAlex |
| Match_Confidence | HIGH / MEDIUM / LOW |
| Promoted_This_Year | 1 if promoted this year, 0 otherwise |

---

## Tableau Visualization

The final interactive dashboard was built in **Tableau Public** using `faculty_tableau_ready.csv`.

**Visualization logic:**
- **X axis** — Year (2014–2026)
- **Y axis** — Publication count
- **Lines** — One line per faculty member, colored by faculty name, showing their publication trend over time
- **Dots** — Appear only in years when a promotion occurred; colored by the rank promoted *to* (orange = Associate Professor, red = Full Professor)
- **Filters** — Institution (multi-select) and Faculty Name (cascading — faculty list updates based on selected institutions)

**Research question being explored:** Does a faculty member's publication output increase in the years leading up to a promotion? The visualization allows direct comparison of publication trajectories across individuals and institutions.

→ **[View the live dashboard on Tableau Public](https://public.tableau.com/views/Book1_17761599715700/Dashboard1?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)**

---

## Repository Contents

| File | Description |
|---|---|
| `faculty_openalex.py` | Python script to fetch publication counts from OpenAlex |
| `Faculty Details Completed.xlsx` | Manually compiled faculty roster with year-by-year rank data |
| `faculty_tableau_ready.csv` | Long-format merged dataset ready for Tableau |

---

## Tools & APIs Used

- [OpenAlex](https://openalex.org/) — Open scholarly publication database
- [Carnegie Classifications](https://carnegieclassifications.acenet.edu/) — Institutional research classification
- [SIOP GTP Portal](https://portal.siop.org/graduate-training-program) — I/O Psychology doctoral program directory
- [Tableau Public](https://public.tableau.com/) — Data visualization
- Python (`requests`, `pandas`, `openpyxl`)
