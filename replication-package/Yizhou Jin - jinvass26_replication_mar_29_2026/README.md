# Replication Package for "Buying Data from Consumers" (Jin and Vasserman)

Corresponding author: Yizhou Jin (yizhou.jin@utoronto.ca)

## Overview

The code in this replication package regenerates all 47 exhibits (tables and figures) from intermediate CSV and JSON data files using R, and compiles the paper PDF using LaTeX. A single master script (`Rscript code/run_all.R`) runs the entire pipeline in approximately 3 minutes.

The paper uses proprietary auto insurance panel data that cannot be redistributed. We provide two modes of replication:
- **Section I** uses precomputed intermediate results (summary statistics, model estimates, counterfactual outcomes) derived from the proprietary data.
- **Section II** generates synthetic data from calibrated distributional profiles (observable characteristics) and structural model estimates (claims, telematics scores and discounts, and choices), re-estimates the structural model, and runs counterfactual simulations, enabling end-to-end code verification without proprietary data.

## Data Availability Statement

The analysis data for this paper are derived from a proprietary auto insurance panel provided under a confidential data use agreement with a major U.S. insurer. The data cannot be redistributed.

To enable code verification, this replication package includes:
1. **Precomputed intermediate results** (CSV/JSON files) containing summary statistics, model fit moments, and counterfactual equilibrium outcomes derived from the proprietary data. These intermediates are sufficient to regenerate all exhibits.
2. **Calibrated distributional profiles** (JSON files) that capture aggregate moments of the proprietary data without revealing individual-level information. These profiles drive the simulated-data pipeline (Section II) for end-to-end code verification.

### Summary of Availability

| Data | Provided | Public | Preservation |
|------|----------|--------|--------------|
| Precomputed intermediates (CSV/JSON) | Yes | Yes | Indefinite (Dataverse) |
| Bootstrap estimates (CmdStan CSV) | Yes | Yes | Indefinite (Dataverse) |
| Stan model specifications | Yes | Yes | Indefinite (Dataverse) |
| Calibrated distributional profiles | Yes | Yes | Indefinite (Dataverse) |
| Regression output (JSON) | Yes | Yes | Indefinite (Dataverse) |
| Proprietary auto insurance panel | No | No | ≥ 5 years (data use agreement) |
| Proprietary estimation data (data_list) | No | No | Indefinite (data use agreement) |
| Competitor price quotes (Quadrant) | No | No | Indefinite (by authors) |

All non-proprietary data and code will be preserved indefinitely on the JPE Dataverse. The proprietary panel data will be preserved for at least five years after publication under the confidential data use agreement. The proprietary estimation data and competitor price quotes will be preserved indefinitely by the authors. The authors will provide reasonable assistance to replication requests.

### Statement about Rights

I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permissions are documented in the LICENSE.txt file.

## Instructions to Replicators

**Section I** — regenerate all exhibits from precomputed intermediates (~3 min):
1. Install R ≥ 4.3.0 and the required packages listed below.
2. Install LaTeX with `pdflatex` and `biber`.
3. From the repository root, run:
   ```bash
   Rscript code/run_all.R
   ```
4. Outputs are written to `output/exhibits/`. The compiled paper is at `paper/paper.pdf`.

**Section II** — end-to-end verification with synthetic data (~1–3 hrs TESTING mode; ~15–40 hrs full precision):
1. Complete all Section I prerequisites above.
2. Install [CmdStan](https://mc-stan.org/users/interfaces/cmdstan) ≥ 2.34 and the `cmdstanr` R package.
3. From the repository root, run:
   ```bash
   # Quick test run (default: TESTING = TRUE, reduced grids/iterations, ~2–4 hrs)
   Rscript code/run_simulated.R

   # Full-precision replication from scratch
   Rscript -e 'TESTING <- FALSE; FROM_SCRATCH <- TRUE; source("code/run_simulated.R")'
   ```
4. Outputs are written to `output/exhibits_simulated/`. The compiled paper is at `paper/paper_simulated.pdf`.

## Computational Requirements

### Software

| Component | Version | Notes |
|-----------|---------|-------|
| R | ≥ 4.3.0 (tested on 4.5.1) | |
| [CmdStan](https://mc-stan.org/users/interfaces/cmdstan) | ≥ 2.34 (tested on 2.34.1) | Section II only |
| LaTeX | `pdflatex` + `biber` | Paper compilation |
| OS | macOS 15+ or Linux | Tested on macOS Sequoia 15.4 (Darwin 25.3.0) |

**Section I R packages:** `tidyverse`, `ggplot2`, `dplyr`, `jsonlite`, `kableExtra`, `scales`, `RColorBrewer`, `reshape2`, `stargazer`, `stringr`, `magrittr`, `hash`, `foreign`, `lubridate`, `labeling`, `digest`, `grid`, `miscTools`, `lazyeval`

**Section II R packages:** everything above, plus `cmdstanr`, `doParallel`, `foreach`, `data.table`, `tictoc`, `MASS`, `fixest`

### Hardware

| Task | RAM | CPU |
|------|-----|-----|
| Section I (exhibits + PDF) | 8 GB | Any modern CPU |
| Section II — TESTING mode | 32 GB | 8+ cores recommended |
| Section II — full precision | 64 GB | 16+ cores recommended |

Tested on Apple M4 (10 cores, 32 GB RAM), Apple M3 Max (16 cores, 128 GB RAM), and Mac Studio (Apple M2 Ultra, 28 cores, 96 GB RAM).

### Runtime and Storage

| Pipeline | Runtime | Storage |
|----------|---------|---------|
| Section I (exhibits + PDF) | ~3 minutes | ~200 MB |
| Section II — TESTING mode | ~1–3 hours | ~500 MB additional |
| Section II — full precision | ~15–40 hours | ~1 GB additional |

**Section I breakdown:**

| Step | Script | Time |
|------|--------|------|
| Parameter tables | c2_get_param_tables.R | ~1 min |
| RF exhibits | c1_get_rf_exhibits.R | ~1 min |
| Fit + CTF exhibits | c3_get_fitctf_exhibits.R | ~1 min |
| Paper compilation | pdflatex + biber | <1 min |

**Section II breakdown (TESTING mode, default):**

| Step | Script | Time |
|------|--------|------|
| Data generation | sim_generate_rf_data.R, sim_generate_data_list.R | ~5 min |
| RF analysis | sim_c0–c5 | ~5 min |
| Structural estimation | sim_c6_estimate.R, sim_c7_model_fit.R | ~10–30 min |
| CTF equilibrium | sim_c8_ctf_run.R | ~1–3 hrs |
| Exhibits + PDF | c1/c2/c3 + pdflatex | ~3 min |

**Section II breakdown (full precision, `TESTING <- FALSE`):**

| Step | Script | Time |
|------|--------|------|
| Data generation | sim_generate_rf_data.R, sim_generate_data_list.R | ~5 min |
| RF analysis | sim_c0–c5 | ~5 min |
| Structural estimation | sim_c6_estimate.R, sim_c7_model_fit.R | ~4–8 hrs |
| CTF equilibrium | sim_c8_ctf_run.R | ~10–30 hrs |
| Exhibits + PDF | c1/c2/c3 + pdflatex | ~3 min |

### Controlled Randomness

The simulated-data pipeline (Section II) uses pseudorandom number generation for synthetic data and CmdStan optimization. Random seeds are set in the simulation scripts to ensure reproducibility.

### License

This replication package is released under the MIT License. See `LICENSE.txt`.

---

# Section I — Full Paper Replication

**Tagged release**: [`v1.0-replication`](https://github.com/yizhoujin/jinvass26_replication/releases/tag/v1.0-replication)

```bash
git clone --branch v1.0-replication https://github.com/yizhoujin/jinvass26_replication.git
cd jinvass26_replication
Rscript code/run_all.R
```

## Repository Structure

```
├── code/                          Scripts (run_all.R is the entry point)
├── data/
│   ├── precomputed/               Intermediate results from proprietary data
│   ├── estimates/                 Estimation output (replicable given model output)
│   └── simulated/                 Profiles for simulated-data mode (Section II)
├── paper/                         LaTeX source and static photographs
└── output/                        Generated exhibits — created by run_all.R
```

## Pipeline

`run_all.R` executes four steps:

```
┌─────────────────────────────────────────────────────────────────────┐
│  estimates/model_output/         ──→  c2_get_param_tables.R        │
│  (bootstrap CSVs, Stan files)         → 8 parameter tables         │
├─────────────────────────────────────────────────────────────────────┤
│  precomputed/rf/                 ──→  c1_get_rf_exhibits.R         │
│  estimates/regression_output/         → 22 reduced-form exhibits   │
├─────────────────────────────────────────────────────────────────────┤
│  precomputed/model_fit/          ──→  c3_get_fitctf_exhibits.R     │
│  precomputed/ctf/                     → 17 fit + CTF exhibits      │
├─────────────────────────────────────────────────────────────────────┤
│  output/exhibits/ + paper/text/  ──→  pdflatex + biber             │
│                                       → paper/paper.pdf            │
└─────────────────────────────────────────────────────────────────────┘
```

## Data and Exhibits

The paper's 47 exhibits fall into two categories based on how their input data is produced.

### Precomputed from proprietary data

These exhibits are derived from the insurer's proprietary panel data, which cannot be distributed. We provide the intermediate results — summary statistics, model fit moments, and counterfactual equilibrium outcomes — as CSV and JSON files.

A simulated-data pipeline (Section II) generates synthetic versions of these intermediates for code verification.

| Directory | Exhibits | Script | Description |
|---|---|---|---|
| `precomputed/rf/` | Tab 1, A.1–A.2; Fig 2a/b, 3, A.3, A.4, B.1a, B.2a/b (12 CSVs + 1 JSON) | c1 | Summary statistics and observable-characteristics summaries |
| `precomputed/model_fit/` | Tab 4–5, C.2–C.3; Fig 6a/b, B.3, B.4 (8 CSVs) | c3 | Model fit moments, choice shares, and figure plotting data |
| `precomputed/ctf/` | Tab 7, A.9–A.15, C.4–C.5 (10 CSVs) | c3 | Counterfactual equilibrium simulation results |

### Fully replicable from estimation output

These exhibits are generated entirely from estimation output — no proprietary data is needed beyond what is already contained in the estimation results. The simulated-data pipeline (Section II) re-estimates these from synthetic data, producing new output that feeds directly into the same exhibit-generation code.

| Directory | Exhibits | Script | Description |
|---|---|---|---|
| `estimates/model_output/` | Tab 6, A.3–A.8, C.3 (8) | c2 | Structural model bootstrap CSVs and Stan specifications. 7 models × ~100 bootstrap samples. Distributed as per-model `.tar.gz` archives (~60 MB total); `run_all.R` extracts them automatically on first run. |
| `estimates/regression_output/` | Tab 2–3, C.1; Fig 4–5, A.5–A.6, B.1b, C.1–C.4 (10 JSONs) | c1 | Reduced-form regression results: coefficients, standard errors, t-values, p-values, N, R² |

### Static photographs (not generated)

Fig 1a–d, A.1, A.2 are photographs included in `paper/exhibits_static/`.

## Description of Programs/Code

| File | Description |
|---|---|
| `code/run_all.R` | Master script: runs c1→c2→c3, extracts bootstrap archives, compiles paper PDF |
| `code/config.R` | Central configuration: all paths, constants, and mode-dependent settings |
| `code/c1_get_rf_exhibits.R` | Generates 22 reduced-form exhibits (tables, figures) from precomputed CSVs and regression JSONs |
| `code/c2_get_param_tables.R` | Generates 8 structural parameter tables from bootstrap CmdStan CSVs |
| `code/c3_get_fitctf_exhibits.R` | Generates 17 model fit and counterfactual exhibits from fit/CTF CSVs |
| `code/functions/helper.R` | Shared utilities: ggplot theme, winsorize, scale, format helpers |
| `code/functions/bootstrap_helpers.R` | Parses CmdStan bootstrap CSVs, generates LaTeX parameter tables |
| `code/functions/ctf_table_helpers.R` | Formats counterfactual results into LaTeX tables |
| `code/functions/getX.R` | Defines covariate variable names and transformations for X matrices |

## List of Tables and Programs

The provided code reproduces all tables and figures in the paper and appendices. Exhibits marked "Simulated" are also reproduced by the Section II pipeline from synthetic data.

| Exhibit | Output file | Script | Source data | Simulated |
|---------|-------------|--------|-------------|-----------|
| Table 1 | `tab_1.tex` | `c1_get_rf_exhibits.R` | `precomputed/rf/tab_1_*.csv` | Yes |
| Table 2 | `tab_2.tex` | `c1_get_rf_exhibits.R` | `regression_output/tab_2_*.json` | Yes |
| Table 3 | `tab_3.tex` | `c1_get_rf_exhibits.R` | `regression_output/tab_3_*.json` | Yes |
| Table 4 | `tab_4.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/model_fit/tab_4.csv` | Yes |
| Table 5 | `tab_5.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/model_fit/tab_5.csv` | Yes |
| Table 6 | `tab_6.tex` | `c2_get_param_tables.R` | `model_output/model_main/results/*.csv` | Yes |
| Table 7 | `tab_7.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/ctf/tab_7.csv` | Yes |
| Figure 2a | `fig_2a.png` | `c1_get_rf_exhibits.R` | `precomputed/rf/fig_2a.csv` | Yes |
| Figure 2b | `fig_2b.png` | `c1_get_rf_exhibits.R` | `precomputed/rf/fig_2b.csv` | Yes |
| Figure 3 | `fig_3.png` | `c1_get_rf_exhibits.R` | `precomputed/rf/fig_3.csv` | Yes |
| Figure 4 | `fig_4.png` | `c1_get_rf_exhibits.R` | `regression_output/fig_4_*.json` | Yes |
| Figure 5 | `fig_5.png` | `c1_get_rf_exhibits.R` | `regression_output/fig_5_*.json` | Yes |
| Figure 6a | `fig_6a.png` | `c3_get_fitctf_exhibits.R` | `precomputed/model_fit/fig_6a.csv` | Yes |
| Figure 6b | `fig_6b.png` | `c3_get_fitctf_exhibits.R` | `precomputed/model_fit/fig_6b.csv` | Yes |
| Fig 1a–d | `exhibits_static/` | (photographs) | — | No |
| Table A.1 | `appendix/tab_a1.tex` | `c1_get_rf_exhibits.R` | `precomputed/rf/appendix/*.csv` | Yes |
| Table A.3–A.6 | `appendix/tab_a3–a6.tex` | `c2_get_param_tables.R` | `model_output/*/results/*.csv` | Partial |
| Table A.7–A.8 | `appendix/tab_a7–a8.tex` | `c2_get_param_tables.R` | `model_output/model_main_*/results/*.csv` | No |
| Table A.9–A.15 | `appendix/tab_a9–a15.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/ctf/*.csv` | No |
| Fig A.1–A.2 | `exhibits_static/` | (photographs) | — | No |
| Fig A.3–A.6 | `appendix/fig_a3–a6.png` | `c1_get_rf_exhibits.R` | `precomputed/rf/appendix/*.csv` | Yes |
| Fig B.1–B.4 | `appendix/fig_b1–b4.png` | `c1/c3_get_*_exhibits.R` | `precomputed/rf,model_fit/*.csv` | Yes |
| Table C.1 | `appendix/tab_c1.tex` | `c1_get_rf_exhibits.R` | `regression_output/appendix/*.json` | Yes |
| Table C.2–C.3 | `appendix/tab_c2–c3.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/model_fit/*.csv` | Partial |
| Table C.4–C.5 | `appendix/tab_c4–c5.tex` | `c3_get_fitctf_exhibits.R` | `precomputed/ctf/*.csv` | No |
| Fig C.1–C.4 | `appendix/fig_c1–c4.png` | `c1_get_rf_exhibits.R` | `regression_output/appendix/*.json` | Yes |

---

# Section II — Simulated-Data Verification

The simulation pipeline generates synthetic data that replaces all proprietary inputs, enabling end-to-end code verification. It produces a `paper_simulated.pdf` with qualitatively similar (but numerically different) exhibits.

To keep compute requirements reasonable, the simulation pipeline estimates only the main structural model (no robustness specifications), produces only point estimates (no bootstrap for standard errors), and solves only the main counterfactual table (no appendix robustness CTF specs).

**Requirements**: Everything in Section I, plus the Section II R packages listed above.

## Pipeline

```bash
Rscript code/run_simulated.R
```

Two toggles control replication scope (set via `Rscript -e` before sourcing):
- **`TESTING`** (default `TRUE`) — when `TRUE`, uses reduced grids and iteration counts for a faster run (~1–3 hrs). Set `FALSE` for full-precision replication (~15–40 hrs).
- **`FROM_SCRATCH`** (default `FALSE`) — when `TRUE`, deletes all cached and generated output before running, forcing a clean end-to-end regeneration.

```bash
# Quick test run (defaults)
Rscript code/run_simulated.R

# Full-precision replication from scratch
Rscript -e 'TESTING <- FALSE; FROM_SCRATCH <- TRUE; source("code/run_simulated.R")'
```

Additional toggles for partial runs:
- `USE_CACHE <- FALSE` — skip restoring cached estimation/CTF outputs (default `TRUE`)
- `RUN_ESTIMATION <- FALSE` — skip structural estimation, Step 4 (default `TRUE`)
- `RUN_CTF <- FALSE` — skip counterfactual simulation, Step 5 (default `TRUE`)

```
┌─────────────────────────────────────────────────────────────────────────┐
│  Steps 1–2: Synthetic data generation                         ~2 min   │
│  data/simulated/data_profile_rf.json                                   │
│    → sim_generate_rf_data.R → synthetic panel data                     │
│  data/simulated/data_profile_data_list.json                            │
│    → sim_generate_data_list.R → synthetic data_list                    │
├─────────────────────────────────────────────────────────────────────────┤
│  Step 3: Reduced-form analysis                               ~5 min   │
│    → sim_c0–c5 RF scripts → simulated CSVs/JSONs                      │
│    (replaces precomputed/rf/ + estimates/regression_output/)           │
├─────────────────────────────────────────────────────────────────────────┤
│  Step 4: Structural estimation + model fit    ~30 min / ~4–8 hrs   │
│    → sim_c6_estimate.R → CmdStan optimization                         │
│    → sim_c7_model_fit.R → simulated fit CSVs                          │
│    (replaces estimates/model_output/ + precomputed/model_fit/)         │
├─────────────────────────────────────────────────────────────────────────┤
│  Step 5: Main counterfactual (Tab 7 only)   ~1–3 hrs / ~10–30 hrs │
│    → sim_c8_ctf_run.R → simulated CTF CSV                             │
│    (replaces precomputed/ctf/tab_7.csv)                                │
├─────────────────────────────────────────────────────────────────────────┤
│  Steps 6–7: Generate exhibits + compile paper                ~3 min   │
│    → c1/c2/c3 exhibit scripts (same code as Section I)                │
│    → output/exhibits_simulated/ → paper_simulated.pdf                  │
└─────────────────────────────────────────────────────────────────────────┘
```

**Total runtime**: ~1–3 hours (TESTING mode) or ~15–40 hours (full precision), dominated by Steps 4 and 5. With `USE_CACHE <- TRUE`, pre-computed estimation and CTF outputs are restored, reducing runtime to ~10 minutes.

## What the simulation covers

| Category | Simulated | Not simulated (uses real-data input) |
|---|---|---|
| Reduced-form exhibits | All 22 (Tab 1–3, A.1–A.2, C.1; all RF figures) | — |
| Parameter tables | Tab 6 (point estimates only, no SEs) | Tab A.3–A.8, C.3 (require robustness models) |
| Model fit | Tab 4–5, C.2–C.3; Fig 6a/b, B.3, B.4 | — |
| Counterfactual | Tab 7 (main specification) | Tab A.9–A.15, C.4–C.5 (robustness CTF specs) |

Exhibits not covered by the simulation use the real-data intermediates from Section I as fallback. The resulting `paper_simulated.pdf` will show simulated values for the core exhibits and real-data values for the robustness appendix.

## Simulation profiles

Two JSON files in `data/simulated/` define the synthetic data-generating process:

- **`data_profile_rf.json`**: Aggregate moments (means, variances, correlations) of the proprietary panel data, plus a `dgp_params` section containing calibrated DGP parameters (risk distributions, demand elasticities, score/discount mappings). Used to generate synthetic RF panels with matching distributional properties.
- **`data_profile_data_list.json`**: Moments of the structural estimation dataset (prices, coverages, choice patterns), plus a `sim_params` section containing estimation tuning constants (price correlation, OO firm adjustments, Stan configuration). Used to generate a synthetic `data_list` for CmdStan estimation.

These profiles are calibrated to match the real data's aggregate statistics without revealing individual-level information.

---

## Dataset List

| Data file | Source | Format | Provided | Description |
|-----------|--------|--------|----------|-------------|
| `data/precomputed/rf/*.csv` | Proprietary panel | CSV | Yes (intermediate) | Summary statistics, observable characteristics |
| `data/precomputed/rf/tab_1_meta.json` | Proprietary panel | JSON | Yes (intermediate) | Panel metadata for Table 1 |
| `data/precomputed/model_fit/*.csv` | Model estimation | CSV | Yes (intermediate) | Model fit moments, choice shares, figure data |
| `data/precomputed/ctf/*.csv` | CTF simulation | CSV | Yes (intermediate) | Counterfactual equilibrium outcomes |
| `data/estimates/model_output/*/results/*.csv` | CmdStan output | CSV | Yes | Bootstrap parameter estimates (~60 MB compressed) |
| `data/estimates/model_output/*/*.stan` | Stan source | Stan | Yes | Structural model specifications |
| `data/estimates/regression_output/*.json` | R regression | JSON | Yes | Reduced-form regression coefficients and statistics |
| `data/simulated/*.json` | Calibration | JSON | Yes | Distributional profiles for synthetic data generation |
| `paper/exhibits_static/*.png` | Photographs | PNG | Yes | Static photographs (Fig 1a–d, A.1, A.2) |

All data files are in non-proprietary formats (CSV, JSON, plain text). The original proprietary panel data is not included.

## Data Citations

**Primary data.** Proprietary administrative records from a large U.S. national auto insurer, 2012--2016. Policy-level panel covering 23 states with single-driver-single-vehicle policies sold via the direct channel. Includes policy characteristics, coverage choices, telematics/UBI enrollment and scores, claims history, renewal decisions, and traffic violations. Provided under a confidential data use agreement; not publicly available.

**Competitor price quotes.** Quadrant Information Services. Liability coverage quotes for the top five competitors in Illinois, matched to individual consumer characteristics and purchase dates.

## Software Citations

R Core Team. 2024. "R: A Language and Environment for Statistical Computing." R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/

Stan Development Team. 2024. "CmdStan: The Command-Line Interface to Stan, Version 2.34." https://mc-stan.org/

Carpenter, Bob, et al. 2017. "Stan: A Probabilistic Programming Language." *Journal of Statistical Software* 76(1): 1--32. DOI: 10.18637/jss.v076.i01

Berge, Laurent. 2018. "fixest: Fast Fixed-Effects Estimations." R package. https://CRAN.R-project.org/package=fixest

Wickham, Hadley, et al. 2019. "Welcome to the Tidyverse." *Journal of Open Source Software* 4(43): 1686. DOI: 10.21105/joss.01686

Gabry, Jonah and Rok Cesnovar. 2024. "cmdstanr: R Interface to CmdStan." R package. https://mc-stan.org/cmdstanr/

## References

Jin, Yizhou and Shoshana Vasserman. "Buying Data from Consumers: The Impact of Monitoring Programs in U.S. Auto Insurance." *Journal of Political Economy*, forthcoming.
