# Replicating Policy Preference & Salience Analysis

This repository contains the full Quarto project, data processing pipelines, and statistical code for the master's dissertation project.

## Important Note for Evaluators & Readers

Please refer to the following main files for the final, word-count-compliant dissertation:

* **`capstone.pdf`** (Compiled Dissertation)
* **`capstone.qmd`** (Main Quarto Source File)

> **Note:** Please **do not evaluate** the `manuscript.pdf` or `manuscript.qmd` files. Those represent a draft to be used for future work that exceeds the 10,000-word limit. The `capstone.*` files contain the finalized, fully compliant version of the research.

---

## 📁 Repository Structure

```text
.
├── capstone.qmd             # Final Quarto source file (< 10k words)
├── capstone.pdf             # Compiled final dissertation PDF
├── data_processed/          # Processed analysis-ready data
├── data_raw/                # Original data files
├── scripts/                 # R scripts and utility functions
├── outputs/                 # Output figures and tables
├── references.bib           # BibTeX references library
└── README.md                # Repository overview and instructions

```

---

## 🛠️ Replication & Execution

### Prerequisites

To compile the document and run the analyses, you will need:

* **R** (v4.2.0 or higher)
* **Quarto CLI** (v1.3 or higher)
* **LaTeX** (e.g., TinyTeX or TeX Live) for rendering PDF outputs

### Required R Packages

Ensure the following packages are installed before running scripts:

```r
install.packages(c(
  "quarto",
  "knitr",
  "kableExtra",
  "dplyr",
  "httr2",
  "tidyverse",
  "jsonlite",
  "readr",
  "readxl",
  "gtrendsR",
  "glue",
  "lme4",
  "lmerTest",
  "broom.mixed",
  "clubSandwich",
  "ggplot2",
  "ggeffects",
  "tidyr",
  "scales",
  "ggnewscale",
  "stringr"
))

```

### Steps to Replicate

1. **Clone the repository:**
```bash
git clone https://github.com/YOUR-USERNAME/YOUR-REPO-NAME.git
cd YOUR-REPO-NAME

```


2. **Run Data Pipelines (Optional):**
If rebuilding from raw data, execute the scripts in the `R/` directory sequentially:
```r
source("R/01_data_cleaning.R")
source("R/02_analysis.R")
source("R/03_robustness.R")

```


3. **Render the Final Capstone PDF:**
Run the following command in your terminal to compile `capstone.qmd`:
```bash
quarto render capstone.qmd --to pdf

```



---

## 📜 License & Citation

This repository is provided for academic evaluation and research replication purposes.

If you have any questions regarding the codebase or underlying methodology, please submit an issue or contact the author directly.
