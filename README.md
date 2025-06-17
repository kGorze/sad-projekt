# DCCI Medical Data, version 0.0.60

### Program structure and functionality

The program follows a **modular architecture with four main analytical modules**, each responsible for a different aspect of statistical analysis. All modules work together to form a comprehensive data-analysis system.

* **Data-Preparation Module**

```bash
# Automatic data preparation with default settings and various input files
Rscript main.R --input data.csv --export
```

The program automatically loads the data, detects variable types and the grouping column, classifies missingness (MCAR, MAR, MNAR) and chooses the optimal imputation method: regression for MAR, MICE for many missing values, or simple techniques for MCAR, with a sensitivity analysis. Outliers are detected with IQR, Z-score, and modified Z-score, then winsorised, removed, or log-transformed; if > 15 % of observations are outliers, the system automatically recommends non-parametric tests. Every step is fully documented in the final report.

* **Descriptive-Statistics Module**

```bash
# Generate descriptive statistics with an HTML report
Rscript main.R --descriptive_stats --input data.csv --report --export
```

This module produces a comprehensive profile for each study group. It automatically computes measures of central tendency (mean, median), dispersion (standard deviation, interquartile range) and distribution shape (skewness, kurtosis). Results are shown in the console and in clear HTML tables with colour-coding to aid interpretation. The program adapts the number of decimal places to each variable, and adds extra metrics such as the coefficient of variation for deeper insight.

* **Comparative-Analysis Module**

```bash
# Comparative analysis with an HTML report
Rscript main.R --comparative_analysis --input data.csv --report --export
```

First, the module checks normality (Shapiro–Wilk < 50 cases, Anderson–Darling ≥ 50) and homogeneity of variance (Levene, Bartlett, Fligner). It then automatically selects ANOVA, Welch’s ANOVA, Kruskal–Wallis, or χ², calculates effect sizes, and performs post-hoc tests with multiple-comparison corrections. Missing values are imputed on the fly: mean/median for sparse MCAR, regression for patterned missingness, MICE for complex cases. The report shows how these choices influence the conclusions.

* **Correlation-Analysis Module**

```bash
# Correlation analysis with an HTML report
Rscript main.R --correlation_analysis --input data.csv --report --export
```

This module explores relationships among variables, choosing Pearson’s correlation for normal data or Spearman’s for non-normal data. It applies FDR (False Discovery Rate) correction to control Type I error under multiple testing. Results appear as a correlation matrix with colour-coded strengths and detailed tables listing significant correlations with an interpretation of effect size.

* **Advanced Inferential-Analysis Module**

```bash
# Enhanced inferential analysis with a report
Rscript main.R --enhanced_inferential --input data.csv --report --export
```

Module 5 searches for covariates moderately correlated with the group (≈ 5–80 %) and with a coefficient of variation > 5 %. These variables are centred on their own means, and models containing only the group factor are compared with models that add the covariates, using multiple regression and ANCOVA. Marginal means and pairwise contrasts are computed. If the sample has at least 75 observations (and ≥ 20 cases per covariate), the module also tests the group × covariate interaction. The optimal model is chosen with Akaike and Bayesian information criteria plus adjusted R². Effect sizes are reported as partial η² for ANCOVA and f² for regression.

## Output reports

A full data analysis can be produced via a unified report:

```bash
Rscript main.R --unified_dashboard
```

The program then generates four detailed HTML reports:

* **Descriptive statistics** with group profiles and assumption tests
* **Comparative analysis** with between-group tests
* **Correlation analysis** showing all significant associations with FDR control
* **Advanced inferential analysis** with ANCOVA, multiple regression, and interaction tests

and links them through an index page. **All plots are also saved separately as PNG files.**

## Sample laboratory data – reports in `output/reports`

1. The statistical report automatically diagnoses biomedical data from three groups (DISEASE1, DISEASE2, CONTROL; 25 cases each). It detects variable types, missing values, and distributions – highlighting pronounced non-normality for hsCRP, PLT, and MON, with borderline deviations for age, HGB, HCT, and LEU. Variances are homogeneous, so the algorithm proposes Welch’s ANOVA when variances differ, or Kruskal–Wallis when the distribution is clearly skewed. Density plots and sex bar charts aid visual assessment. Outlier analysis flags MON and hsCRP as potential outliers.

2. The correlation analysis maps relationships among nine blood variables in 75 observations across three groups. Positive erythrocyte-related links dominate globally: HGB–HCT (r = 0.85), ERY–HGB (0.70), ERY–HCT (0.67). Moderate associations include HGB–MCHC (+0.49) and negative PLT–MCHC (-0.32) and age–MCHC/LEU (-0.31). In DISEASE1, hsCRP–MON (+0.58) and a strong HGB–HCT–MCHC cluster stand out. DISEASE2 confirms robust erythrocyte triplets but lacks significant inflammatory links. In controls, age correlates positively with ERY and negatively with LEU, while ERY aligns positively with PLT and inversely with MCHC. Matrices and histograms show most coefficients hover near zero, underlining the selectivity of the detected relationships.

3. The comparative analysis of the three equal groups tests nine continuous and one categorical variable, choosing the test each time based on normality and variance homogeneity. Most parameters do not differ significantly between groups (age, hsCRP, ERY, PLT, MON, LEU, sex). Significant differences emerge for haemoglobin, haematocrit, and MCHC (Welch’s ANOVA, p ≤ 0.013); effect sizes are large (d 0.87–1.07) with high power (> 85 %). Log transformations or non-parametric methods safeguard models with non-normal residuals.

4. Influence analysis fits two simple models for each of the nine variables: intercept-only vs. group-only. AIC/BIC and adjusted R² indicate that for most parameters (age, hsCRP, ERY, PLT, MON, LEU) the intercept model is best—group membership does not improve fit. Only HGB (Adj R² ≈ 0.17), HCT (0.07), and MCHC (0.13) benefit from the group-only model, confirming earlier ANOVA results. No group × covariate interactions are found, implying a uniform group effect across the sample. Overall, this points to a selective yet stable impact of disease on erythrocyte parameters.

## Project architecture

### Main script `main.r`

The file `main.r` loads all listed modules and defines a command-line interface for running individual analyses:

```R
source("modules/utils/config.R")
source("modules/utils/logging.R")
source("modules/utils/statistical_helpers.R")
source("modules/data/fetch_dataset.R")
source("modules/data/inspect_data.R")
source("modules/data/validate_data.R")
source("modules/data/repair_dataset.R")
source("modules/analysis/assumptions_dashboard.R")
source("modules/analysis/master_descriptive_summary.R")
source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")
source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")
```

### Module structure

```
modules/
- analysis/    core statistical-analysis implementations
- data/        data loading, inspection, and repair
- reporting/   report generation and result export
- utils/       configuration, logging, and helper functions
```

### Files in `modules/analysis`

* `descriptive_stats.R`, `comparative_analysis.R`, `correlation_analysis.R` – main analytical modules for descriptive statistics, group comparisons, and correlation analysis.
* `enhanced_inferential_framework.R` – advanced inferential analysis (ANCOVA, regression, interactions).
* `assumptions_dashboard.R`, `master_descriptive_summary.R`, `enhanced_posthoc.R`, `residual_transformation.R` – support functions for assumption testing, descriptive summaries, post-hoc analysis, and residual diagnostics.

### Files in `modules/data`

* `fetch_dataset.R` – loads a CSV file, detects encoding, and converts data types.
* `inspect_data.R`, `validate_data.R` – check dataset structure and validity.
* `repair_dataset.R` – performs missing-value analysis, imputation, outlier detection, and produces a data-cleaning report.

### Files in `modules/reporting`

* `generate_report.R` – creates HTML reports for any analysis module, combining results and plots.
* `export_results.R` – saves tables and outputs to `output/tables`.

### Files in `modules/utils`

* `config.R` – global analysis settings (significance level, plot parameters, etc.).
* `logging.R` – logging of analysis steps and errors.
* `plotting_utils.R` – tools for managing graphic devices and safely saving plots.
* `statistical_helpers.R` – statistical functions used in analyses (e.g., skewness, kurtosis, z-score).
