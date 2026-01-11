# PLFS 2023-24 Unit Level Data Analysis (R-Focused)

**R-based analysis of India's Periodic Labour Force Survey (PLFS) July 2023 – June 2024 unit level data**  
Focus: Education indicators – Literacy rates, mean years of schooling, out-of-school children, reasons for non-attendance, and application of final survey weights.

[![Repo size](https://img.shields.io/github/repo-size/Maheshgorai/PLFS-2023-24?style=flat-square)](https://github.com/Maheshgorai/PLFS-2023-24)
[![Last commit](https://img.shields.io/github/last-commit/Maheshgorai/PLFS-2023-24?style=flat-square)](https://github.com/Maheshgorai/PLFS-2023-24/commits/main)

## Overview

This repository documents **my R-based exploration** of the **PLFS 2023-24 unit level microdata** from MoSPI (Ministry of Statistics and Programme Implementation, Government of India).

Key education insights derived using survey weights:
- **Literacy rate** (age 7+): ~80.9% nationally (male 87.2%, female 74.6%; urban 88.9%, rural 77.5%)
- **Mean years of schooling**
- **Out-of-school children** (never attended + dropped out, by age group/gender/region)
- **Reasons for not attending school** (economic factors, household work, marriage, distance, etc.)
- And more (gender disparities, state-wise variations, etc.)

Analysis applies **final survey weights/multipliers** (included in repo as "Final weight PLFS2023-24AR", etc.) to produce nationally representative estimates.

**National highlights (PLFS 2023-24 official)**:
- Literacy (7+): 80.9% (up from previous rounds)
- Gender gap: 12.6 pp
- Top states: Mizoram (~98.2%), Lakshadweep, Kerala
- Bottom: Bihar (~74.3%), Rajasthan, Madhya Pradesh

## Repository Contents

- **Final weight PLFS2023-24AR** & similar — Final survey weights/multipliers (essential for weighted estimates)
- (Planned) R scripts/notebooks (.R / .Rmd) for data processing, weighting, and indicator calculation
- Derived tables/charts (to be added)

**Note**: Raw unit-level .txt files (fixed-width format) are **not** included (access-restricted via MoSPI microdata portal). Repo contains weights, scripts, and results only.

## Data Source & Access

- **Official**: PLFS July 2023–June 2024 unit level data  
  → Register at [microdata.gov.in](https://microdata.gov.in) → Request approval → Download .zip with fixed-width .txt files (e.g., household/person level) + data layout/codebook + weights

- Format: Fixed-width text (.txt) — Use R's `read_fwf()` from `readr` or base `read.fwf()` with widths from the official data layout file.
