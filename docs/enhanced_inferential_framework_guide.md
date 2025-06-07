# Enhanced Inferential Framework Guide

## Overview

The Enhanced Inferential Framework extends the basic statistical analysis capabilities by implementing advanced statistical modeling techniques including:

- **Multiple Linear Regression** with covariate adjustments
- **ANCOVA (Analysis of Covariance)** models
- **Interaction terms analysis** (group × covariate interactions)
- **Automated model selection** and comparison
- **Enhanced effect size calculations** with confidence intervals

## Features

### 1. Automatic Covariate Detection

The framework automatically identifies potential covariates in your dataset:

- **Age variables**: Detects variables named `wiek`, `age`, `Age`, `AGE`, etc.
- **Biomarker variables**: Identifies `hsCRP`, `BMI`, `CRP`, and similar patterns
- **Other continuous variables**: Selects variables with moderate correlation to group membership (0.1 < |r| < 0.7)

### 2. Multiple Linear Regression

For each dependent variable, the framework fits:

- **Baseline model**: `outcome ~ group`
- **Full model**: `outcome ~ group + covariates`
- **Model comparison**: Statistical comparison between models

### 3. ANCOVA Analysis

- Fits ANCOVA models adjusting for covariates
- Calculates **Type III ANOVA** for unbalanced designs
- Computes **estimated marginal means** with contrasts
- Provides **effect sizes** (η² and partial η²)

### 4. Interaction Analysis

Tests for significant interactions between:
- Group membership and each covariate
- Automatically includes significant interactions in final models
- Reports model improvement metrics

### 5. Model Selection

Compares multiple models using:
- **AIC** (Akaike Information Criterion)
- **BIC** (Bayesian Information Criterion)
- **Adjusted R²**
- **RMSE** (Root Mean Square Error)

## Usage

### Command Line Interface

```bash
# Run enhanced inferential analysis with report generation
Rscript main.R --enhanced_inferential --report

# Run with custom input file and export results
Rscript main.R --enhanced_inferential --input my_data.csv --report --export

# Run without plots (faster)
Rscript main.R --enhanced_inferential --report
```

### R Console/Script Usage

```r
# Load the framework
source("modules/analysis/enhanced_inferential_framework.R")

# Run full enhanced inferential analysis
results <- perform_enhanced_inferential_analysis(
  data = my_data, 
  group_column = "grupa", 
  include_plots = TRUE
)

# Quick analysis with automatic report
results <- quick_enhanced_inferential(
  data = my_data, 
  group_column = "grupa", 
  generate_report = TRUE
)
```

## Data Requirements

### Input Data Structure

Your dataset should contain:

1. **Group variable** (default: "grupa")
   - Categorical variable defining groups (e.g., "CHOR1", "CHOR2", "KONTROLA")

2. **Dependent variables**
   - Continuous outcomes you want to analyze
   - Will be automatically detected as numeric variables

3. **Covariates** (optional but recommended)
   - Age variables (e.g., "wiek", "age")
   - Biomarkers (e.g., "hsCRP", "BMI")
   - Other continuous variables

### Example Data Structure

```
grupa     wiek    hsCRP    biomarker1    biomarker2    outcome1    outcome2
CHOR1     65      2.3      145.2         67.8          12.4        8.9
CHOR2     72      1.8      138.7         72.1          11.8        9.2
KONTROLA  58      1.2      132.4         65.3          10.9        8.1
...
```

## Output Structure

The analysis returns a comprehensive result object containing:

### `multiple_regression`
- Baseline and full models for each dependent variable
- Model comparisons and diagnostics
- Coefficient estimates with confidence intervals

### `ancova_analysis`
- ANCOVA models with Type III ANOVA tables
- Estimated marginal means and contrasts
- Effect sizes (η² and partial η²)

### `interaction_analysis`
- Tests for group × covariate interactions
- Significance tests and model improvements
- R² change statistics

### `model_comparison`
- Comparison tables with AIC, BIC, R²
- Best model selection with rationale
- Model ranking across criteria

### `effect_sizes`
- Cohen's f² for regression models
- Partial η² for ANCOVA models
- Effect size interpretations

## Model Assumptions

The framework automatically checks:

1. **Normality of residuals** (Shapiro-Wilk test)
2. **Homoscedasticity** (Breusch-Pagan test)
3. **Independence** (Durbin-Watson test)
4. **Influential observations** (Cook's distance)

## Interpretation Guidelines

### Effect Sizes

**Cohen's f² (Regression models):**
- < 0.02: Negligible effect
- 0.02-0.15: Small effect
- 0.15-0.35: Medium effect
- > 0.35: Large effect

**Partial η² (ANCOVA models):**
- < 0.01: Negligible effect
- 0.01-0.06: Small effect
- 0.06-0.14: Medium effect
- > 0.14: Large effect

### Model Selection

The framework uses a composite scoring system:
- Lower AIC/BIC values indicate better model fit
- Higher adjusted R² indicates more explained variance
- Best model minimizes composite score across criteria

### Statistical Significance

- **p < 0.05**: Statistically significant
- **p < 0.01**: Highly significant
- **p < 0.001**: Very highly significant

## Example Workflow

1. **Data Preparation**
   ```r
   # Load your data
   my_data <- read.csv("medical_data.csv")
   
   # Check data structure
   str(my_data)
   ```

2. **Run Analysis**
   ```r
   # Perform enhanced inferential analysis
   results <- perform_enhanced_inferential_analysis(my_data, "grupa")
   ```

3. **Examine Results**
   ```r
   # Check covariates identified
   results$metadata$covariates
   
   # View model comparison for specific variable
   results$model_comparison$outcome1$comparison_table
   
   # Check for significant interactions
   results$interaction_analysis$outcome1
   ```

4. **Interpret Findings**
   ```r
   # Best models
   lapply(results$model_comparison, function(x) x$best_model)
   
   # Effect sizes
   results$effect_sizes
   ```

## Integration with Existing Framework

The enhanced inferential framework integrates seamlessly with the existing analysis pipeline:

- **Comparative Analysis**: Now includes enhanced inferential results
- **Reporting**: Compatible with HTML report generation
- **Export**: Results can be exported to CSV format
- **Plotting**: Generates model comparison visualizations

## Troubleshooting

### Common Issues

1. **Insufficient Data**: Ensure adequate sample size (minimum 15-20 observations per analysis)

2. **Missing Covariates**: If no covariates are detected, check variable naming conventions

3. **Model Convergence**: For complex interactions, simplify the model or check for multicollinearity

4. **Assumption Violations**: Review model diagnostics and consider data transformations

### Error Messages

- **"Insufficient data for multiple regression"**: Increase sample size or remove missing values
- **"No covariates available for ANCOVA"**: Check that potential covariates are numeric and properly named
- **"Error in model comparison"**: Verify that all models can be fitted with available data

## Advanced Usage

### Custom Covariate Selection

```r
# Manually specify covariates
custom_covariates <- c("age", "baseline_measure", "severity_score")
results <- perform_multiple_linear_regression(data, dependent_vars, "group", custom_covariates)
```

### Model Diagnostics

```r
# Check assumptions for specific variable
results$multiple_regression$outcome1$diagnostics

# Examine residual plots
plot(results$ancova_analysis$outcome1$model)
```

### Effect Size Analysis

```r
# Calculate additional effect sizes
library(effectsize)
model <- results$ancova_analysis$outcome1$model
eta_squared(model, partial = TRUE, ci = 0.95)
```

## References

- Cohen, J. (1988). Statistical power analysis for the behavioral sciences.
- Field, A. (2013). Discovering statistics using IBM SPSS statistics.
- Maxwell, S. E., & Delaney, H. D. (2004). Designing experiments and analyzing data. 