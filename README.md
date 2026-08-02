# JAR-deviation burden, home–laboratory differences, and purchase intent in cooked ham evaluations

This repository contains the reproducible R code for the study:

**JAR-Deviation Burden, Home–Laboratory Differences, and Purchase Intent in Consumer Evaluations of Commercial Cooked Hams**

The analysis is a secondary analysis of the public **HomeHam** dataset. It uses structured variables only: overall liking, Just-About-Right (JAR) ratings, measured salt and fat contents, home-use and blind laboratory evaluations, price, usual purchase, and purchase intent. Free-Comment and Ideal-Free-Comment text variables are not analyzed.

## Repository contents

```text
ham_jar_home_lab_public_analysis.R   One-file reproducible analysis
README.md                            Data, software, and execution instructions
LICENSE                              MIT license for the code
.gitignore                           Excludes raw data and generated outputs
```

The raw HomeHam workbook is not redistributed in this repository.

## Data source

Download **HomeHam, Version 2** from Recherche Data Gouv:

- Dataset DOI: `10.57745/GA5R6S`
- Dataset record: `https://doi.org/10.57745/GA5R6S`

Save the downloaded Excel workbook in the repository root as:

```text
dataset.xlsx
```

The script expects these workbook sheets:

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

## Data and study citations

Please cite the public dataset and its associated data article when reusing the data or code:

- Visalli, M., Loiseau, A.-L., Cordelle, S., Mahieu, B., and Schlich, P. (2024). *A dataset of perception and preferences of French consumers for commercial cooked hams sampled according to their nutritional values and claims*. Recherche Data Gouv, Version 2. `https://doi.org/10.57745/GA5R6S`
- Visalli, M., Loiseau, A.-L., Cordelle, S., Mahieu, B., and Schlich, P. (2024). “A dataset of perception and preferences of French consumers for commercial cooked hams sampled according to their nutritional values and claims.” *Data in Brief*, 54, 110549. `https://doi.org/10.1016/j.dib.2024.110549`
- Mahieu, B., Visalli, M., and Schlich, P. (2022). “Identifying drivers of liking and characterizing the ideal product thanks to Free-Comment.” *Food Quality and Preference*, 96, 104389. `https://doi.org/10.1016/j.foodqual.2021.104389`

## Software requirements

Use R 4.1 or later. The analysis was developed with R 4.3.

Required packages:

```r
c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "janitor", "readr",
  "lubridate", "forcats", "tibble", "ggplot2", "lme4", "lmerTest",
  "emmeans", "ordinal", "ggrepel"
)
```

Install them once before running the analysis:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "janitor", "readr",
  "lubridate", "forcats", "tibble", "ggplot2", "lme4", "lmerTest",
  "emmeans", "ordinal", "ggrepel"
))
```

The public script does not install packages automatically. On Windows, restart R or RStudio and remove any stale `00LOCK` directory before reinstalling a package whose DLL is locked.

## Reproducing the analysis

1. Clone or download this repository.
2. Download the public HomeHam workbook and save it as `dataset.xlsx` in the repository root.
3. Start R in the repository root.
4. Run:

```r
source("ham_jar_home_lab_public_analysis.R")
```

The script uses the current working directory as the project root; no machine-specific path needs to be edited.

Console progress messages are suppressed by default. To display them:

```r
options(ham.analysis.verbose = TRUE)
source("ham_jar_home_lab_public_analysis.R")
```

## Analysis workflow

The script performs the following steps in sequence:

1. Reads and validates the HomeHam workbook.
2. Constructs evaluation-level liking and JAR variables without using free-text responses.
3. Creates cumulative JAR-deviation count and severity measures and eight direction-specific deviation indicators.
4. Summarizes consumer metadata by evaluation context, while retaining a transparent check for the duplicated laboratory metadata identifier.
5. Estimates product-level home-use–laboratory liking and JAR-deviation gaps for the 16 overlapping products.
6. Fits liking models with product fixed effects and a consumer random intercept; a random-product specification is reported as a sensitivity analysis.
7. Fits the direction-specific multivariate JAR model with evaluation location and product as fixed effects and a consumer random intercept.
8. Relates measured salt and fat contents to product-level JAR perception.
9. Fits the primary ordinal cumulative-link mixed model for purchase intent with consumer and product random intercepts. Linear and binary mixed models are sensitivity analyses; in the binary model, “yes” is coded 1 and “no” or “uncertain” is coded 0.
10. Runs J09-exclusion, leave-one-product-out, location-interaction, bootstrap, and estimated-marginal-mean sensitivity checks.
11. Produces manuscript-ready main and supplementary tables and figures, including 600-dpi TIFF files.

## Main output directories

```text
data_processed/
analysis_outputs/
analysis_outputs/sensitivity_checks/
manuscript_outputs_revised/
```

Important outputs include:

```text
data_processed/consumer_demographics_by_location.csv
analysis_outputs/tables/16_direction_specific_model_specification.csv
analysis_outputs/sensitivity_checks/tables/07a_direction_specific_model_specification.csv
analysis_outputs/sensitivity_checks/tables/10_purchase_intent_ordinal_clmm_coefficients.csv
manuscript_outputs_revised/tables/
manuscript_outputs_revised/figures/
manuscript_outputs_revised/figures_tiff/
manuscript_outputs_revised/supplementary/tables/
manuscript_outputs_revised/supplementary/figures/
manuscript_outputs_revised/supplementary/figures_tiff/
manuscript_outputs_revised/tiff_file_manifest.csv
```

The manuscript-output stage stops rather than silently substituting another model when the full ordinal purchase-intent model with consumer and product random intercepts is unavailable.

## Interpretation

The home-use and blind laboratory evaluations are not a randomized within-consumer comparison. Home-use products were self-selected purchases evaluated with marketplace information available, whereas the laboratory data are blind evaluations of a common subset of products by a different panel. Home–laboratory gaps therefore describe differences between evaluation contexts and should not be interpreted as causal effects of location, blinding, packaging, or any single extrinsic cue.

The laboratory consumer metadata contain one duplicated identifier. Demographic summaries use the 86 metadata records, while evaluation-level model construction uses identifiers from the sensory table and a deduplicated metadata join to prevent many-to-many matches.

The primary product-level correlations are calculated from the original product means. The bootstrap correlation table reports the mean and 2.5th and 97.5th percentiles of correlations recalculated after within-product-and-context resampling; it does not replace the primary correlations.

## License

The code is released under the MIT License. The HomeHam dataset is not included and remains subject to the terms stated by Recherche Data Gouv and the data creators.

## Questions and reproducibility reports

Please use the repository’s GitHub Issues page for questions about running the code or reporting reproducibility problems. Do not upload the raw consumer dataset to an issue or pull request.
