# Sales Prediction — OLS Regression in R

Iterative OLS regression study in R across 11 progressively complex model specifications — from univariate to polynomial and interaction-term models. Includes VIF multicollinearity diagnostics, ANOVA-based model comparison, and residual analysis.

**Stack:** R · base stats · ggplot2 · car (VIF) · RMarkdown

---

## Overview

This project builds a systematic regression analysis pipeline in R, starting from a simple univariate baseline and iteratively adding feature complexity. The goal is to identify the model specification that maximizes predictive accuracy while maintaining statistical validity — avoiding overfitting and multicollinearity.

---

## Methodology

### Model Progression (11 Specifications)

| Stage | Model Type | Description |
|-------|-----------|-------------|
| 1 | Univariate OLS | Single predictor baseline |
| 2–5 | Multivariate OLS | Additive feature expansion |
| 6–8 | Polynomial OLS | Non-linear term addition (x², x³) |
| 9–11 | Interaction Terms | Cross-feature interaction effects |

### Diagnostics Applied

- **VIF (Variance Inflation Factor)** — Detects multicollinearity; features with VIF > 5 investigated for removal
- **ANOVA F-test** — Compares nested models to determine if added complexity is statistically justified
- **Residual Analysis** — Checks linearity, homoscedasticity, normality (Q-Q plots, Breusch-Pagan test)
- **Adjusted R²** — Penalizes unnecessary parameters; primary selection criterion across model iterations

---

## Key Findings

- Polynomial terms improved fit significantly up to degree 2; cubic terms showed signs of overfitting on the holdout set.
- Interaction terms between [key predictors] captured non-additive effects, yielding the best adjusted R² among all 11 specifications.
- VIF screening identified collinear pairs early, preventing inflated coefficient estimates in the multivariate stages.
- ANOVA comparisons confirmed that each step-wise addition was statistically significant (p < 0.05) before inclusion.

---

## Repository Structure

```
SalesPredictionProject_R/
├── data/                   # Source dataset
├── scripts/                # R scripts for each model specification
├── reports/                # Generated model comparison report
└── README.md
```

---

## Technologies

| Tool | Purpose |
|------|---------|
| R (base stats) | OLS regression, ANOVA, residual diagnostics |
| car package | VIF multicollinearity diagnostics |
| ggplot2 | Residual plots, fitted vs actual visualization |
| RMarkdown | Reproducible analysis report |

---

## Setup

```r
# Clone repository and open in RStudio
# Install dependencies:
install.packages(c("car", "ggplot2", "lmtest"))

# Run analysis:
source("scripts/regression_analysis.R")
```
