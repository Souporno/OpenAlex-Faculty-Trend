"""
Faculty Cumulative Publications Script
Reads faculty from Faculty Details.xlsx and fetches publication counts
from OpenAlex API (grouped by year), then computes running cumulative totals
from each faculty member's career start through 2026.

IMPORTANT: Ze (Mia) Zhu's OpenAlex ID has been corrected to A5084303359
(previously incorrectly matched to "Lin Zhu", A5004614278).

Run this script on a machine with internet access to regenerate faculty_cumulative_pubs.xlsx.
"""

import requests
import time
import openpyxl
from openpyxl import Workbook

FACULTY_XLSX = "Faculty Details.xlsx"   # adjust path if needed
OUTPUT_XLSX  = "faculty_cumulative_pubs.xlsx"
TARGET_YEARS = list(range(2014, 2027))
EMAIL        = "souporno@uw.edu"         # polite API header

# Manually verified corrections
OPENALEX_OVERRIDES = {
    "Ze (Mia) Zhu": {
        "id": "A5084303359",
        "matched_name": "Ze Zhu",
        "confidence": "HIGH",
    }
}


def fetch_year_counts(author_id: str, email: str) -> dict:
    """Return {year: count} for ALL publication years for this author."""
    url = f"https://api.openalex.org/works"
    params = {
        "filter": f"author.id:{author_id}",
        "group_by": "publication_year",
        "per_page": 200,
        "mailto": email,
    }
    resp = requests.get(url, params=params, timeout=30)
    resp.raise_for_status()
    data = resp.json()
    year_counts = {}
    for item in data.get("group_by", []):
        try:
            yr = int(item["key"])
            cnt = int(item["count"])
            year_counts[yr] = cnt
        except (ValueError, KeyError):
            pass
    return year_counts


def compute_cumulative(year_counts: dict, target_years: list) -> dict:
    """Return {year: cumulative_total_as_of_year} for all target_years."""
    if not year_counts:
        return {yr: 0 for yr in target_years}
    earliest = min(year_counts.keys())
    max_target = max(target_years)
    running_total = 0
    cumulative = {}
    for y in range(earliest, max_target + 1):
        running_total += year_counts.get(y, 0)
        if y in target_years:
            cumulative[y] = running_total
    # Fill any target years before earliest
    for yr in target_years:
        if yr not in cumulative:
            cumulative[yr] = 0
    return cumulative


def main():
    # Load faculty
    wb_in = openpyxl.load_workbook(FACULTY_XLSX)
    ws_in = wb_in.active
    headers = [str(c.value) if c.value else '' for c in ws_in[1]]

    # Build output workbook
    wb_out = Workbook()
    ws_out = wb_out.active
    ws_out.title = "Cumulative Publications"
    out_headers = ["Faculty Name", "Institution", "OpenAlex ID", "Match Confidence",
                   "Total_All_Time"] + [f"Cumul_{yr}" for yr in TARGET_YEARS]
    ws_out.append(out_headers)

    faculty_rows = list(ws_in.iter_rows(min_row=2, values_only=True))
    print(f"Processing {len(faculty_rows)} faculty...\n")

    for row in faculty_rows:
        d = dict(zip(headers, row))
        name        = d.get('Faculty Name', '')
        institution = d.get('Institution', '')
        openalex_id = d.get('OpenAlex ID', '')
        confidence  = d.get('Match Confidence', '')
        matched_name= d.get('OpenAlex Matched Name', '')

        # Apply any manual overrides
        if name in OPENALEX_OVERRIDES:
            override = OPENALEX_OVERRIDES[name]
            openalex_id  = override["id"]
            matched_name = override["matched_name"]
            confidence   = override["confidence"]
            print(f"  [OVERRIDE] {name}: using {openalex_id} ({matched_name})")

        if not openalex_id:
            print(f"  [SKIP] {name}: no OpenAlex ID")
            ws_out.append([name, institution, '', confidence, 0] + [0]*len(TARGET_YEARS))
            continue

        try:
            year_counts = fetch_year_counts(openalex_id, EMAIL)
            total_all_time = sum(year_counts.values())
            cumul = compute_cumulative(year_counts, TARGET_YEARS)
            print(f"  {name} ({matched_name}): total={total_all_time}, "
                  f"cumul_2014={cumul.get(2014,0)}, cumul_2026={cumul.get(2026,0)}")
            row_out = [name, institution, openalex_id, confidence, total_all_time]
            row_out += [cumul.get(yr, 0) for yr in TARGET_YEARS]
            ws_out.append(row_out)
        except Exception as e:
            print(f"  [ERROR] {name}: {e}")
            ws_out.append([name, institution, openalex_id, confidence, 0] + [0]*len(TARGET_YEARS))

        time.sleep(0.1)  # polite rate limiting

    wb_out.save(OUTPUT_XLSX)
    print(f"\nSaved: {OUTPUT_XLSX}")


if __name__ == "__main__":
    main()
