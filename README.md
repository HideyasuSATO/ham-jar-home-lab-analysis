# JAR-deviation burden, home--laboratory differences, and purchase intent in cooked ham evaluations

This repository contains the public R code used to reproduce the data processing, statistical analyses, sensitivity checks, and manuscript-ready tables and figures for the study:

**JAR-deviation burden, home--laboratory differences, and purchase intent in consumer evaluations of commercial cooked hams**

The analysis reuses the public **HomeHam** cooked-ham consumer dataset. The code focuses on structured variables only: liking, Just-About-Right (JAR) scales, measured salt and fat contents, home-use and blind laboratory evaluations, price, usual purchase, and purchase intent. Free-Comment text variables are not analyzed.

## Repository contents

```text
ham_jar_home_lab_public_analysis.R   # One-file reproducible analysis script
README.md                            # This file
```

The raw dataset is **not redistributed** in this repository. Download it from the original data repository and place it in the project root as described below.

## Data requirement

Download the HomeHam dataset from the original public repository and place the Excel workbook in the project root directory as one of the following filenames:

```text
dataset.xlsx
```

The script expects an Excel workbook containing the following sheets:

```text
description
product packaging
product composition
consumer
consumer questionnaire (home)
consumer questionnaire (lab)
product sensory properties
product purchase informations
```

## Data citation

Please cite the original dataset and associated publications when using this code or derived results:

- HomeHam dataset, Mendeley Data, `10.17632/ptpb3zh6rr.1`
- Visalli et al. (2024), *Data in Brief*, `10.1016/j.dib.2024.110549`
- Mahieu, Visalli, and Schlich (2022), *Food Quality and Preference*, `10.1016/j.foodqual.2021.104389`

## Software requirements

The script was written for R and uses the native pipe operator (`|>`). Use R 4.1 or later. It was developed during analysis with R 4.3.

Required R packages:

```r
c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "janitor", "readr",
  "lubridate", "forcats", "tibble", "ggplot2", "lme4", "lmerTest",
  "emmeans", "ordinal", "ggrepel"
)
```

Install missing packages before running the script:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "janitor", "readr",
  "lubridate", "forcats", "tibble", "ggplot2", "lme4", "lmerTest",
  "emmeans", "ordinal", "ggrepel"
))
```

On Windows, if package installation fails because DLL files are locked, restart R/RStudio and remove any `00LOCK` folders in your R library before reinstalling packages.

## How to reproduce the analysis

1. Create a project directory.
2. Put `ham_jar_home_lab_public_analysis.R` in the project directory.
3. Download the public Excel dataset and save it in the same directory as `dataset.xlsx`.
4. Start R in the project directory and run:

```r
source("ham_jar_home_lab_public_analysis.R")
```

Alternatively:

```r
setwd("path/to/project")
source("ham_jar_home_lab_public_analysis.R")
```

The script uses the current working directory as the project root. No file paths need to be edited if the dataset and script are in the same directory.

## Main outputs

Running the script creates the following folders.

### `data_processed/`

Processed analysis datasets and QA tables, including:

```text
sensory_analysis_no_text.csv
model_liking.csv
model_liking_common_products.csv
model_purchase.csv
product_panel.csv
context_gain.csv
ham_processed_list.rds
```

### `analysis_outputs/`

Main model outputs, intermediate tables, figures, and model summaries:

```text
analysis_outputs/tables/
analysis_outputs/figures/
analysis_outputs/models/
```

### `analysis_outputs/sensitivity_checks/`

Sensitivity analyses, including J09 exclusion, leave-one-product-out correlations, direction-specific JAR penalty by location, bootstrap uncertainty summaries, and purchase-intent model checks:

```text
analysis_outputs/sensitivity_checks/tables/
analysis_outputs/sensitivity_checks/figures/
analysis_outputs/sensitivity_checks/models/
```

### `manuscript_outputs_revised/`

Manuscript-ready tables and figures:

```text
manuscript_outputs_revised/tables/
manuscript_outputs_revised/figures/
manuscript_outputs_revised/figures_tiff/
manuscript_outputs_revised/supplementary/tables/
manuscript_outputs_revised/supplementary/figures/
manuscript_outputs_revised/supplementary/figures_tiff/
```

The main figures are saved as PNG, PDF, and 600-dpi TIFF files. TIFF copies intended for journal submission are collected in:

```text
manuscript_outputs_revised/figures_tiff/
manuscript_outputs_revised/supplementary/figures_tiff/
```

A TIFF manifest is also written to:

```text
manuscript_outputs_revised/tiff_file_manifest.csv
```

## Analyses reproduced by the script

The script performs the following analysis workflow:

1. Load and clean the HomeHam workbook.
2. Construct product-level composition and packaging summaries.
3. Construct evaluation-level liking and JAR variables.
4. Define direction-specific JAR deviations and cumulative JAR-deviation burden.
5. Estimate home-use versus blind laboratory liking differences for overlapping products.
6. Model liking as a function of evaluation context and JAR-deviation burden.
7. Estimate direction-specific multivariate JAR penalties.
8. Relate measured salt and fat contents to JAR perception at product level.
9. Model purchase intent using liking, price, usual purchase, and JAR-deviation burden.
10. Run sensitivity checks, including J09 exclusion, leave-one-product-out correlations, bootstrap intervals, and alternative purchase-intent models.
11. Generate manuscript-ready main and supplementary tables and figures.

## Notes on interpretation

The home-use and blind laboratory evaluations are not a randomized within-consumer comparison. The home-use data include self-selected product purchase and non-blind evaluation, whereas the laboratory data provide blind evaluations of a subset of products. Product-level home--laboratory differences should therefore be interpreted as descriptive and model-based associations rather than causal effects of evaluation context.

The analysis intentionally avoids Free-Comment text variables to distinguish this reanalysis from the original Free-Comment and Ideal-Free-Comment study.

## Recommended `.gitignore`

The raw Excel file and generated outputs can be large and should normally not be committed to the repository. A simple `.gitignore` could include:

```text
# raw data
*.xlsx

# generated outputs
data_processed/
analysis_outputs/
manuscript_outputs_revised/

# R session files
.Rhistory
.RData
.Rproj.user/
```

## License

The code may be released under an open-source license such as MIT. The raw HomeHam dataset is not part of this repository and remains subject to the terms of the original data repository.

## Contact

Hideyasu Sato  
Department of Data Science for Food Systems, Faculty of Food and Nutritional Sciences, Toyo University  
Email: sato0035@toyo.jp
