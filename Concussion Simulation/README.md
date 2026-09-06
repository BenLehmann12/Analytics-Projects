# Sports-Related Concussion Recovery: A Biostatistical Analysis

A full end-to-end statistical and machine learning analysis of 30,000 athlete concussion records in **R** — covering hypothesis testing, A/B testing, survival analysis, predictive modeling, and unsupervised learning, delivered as a reproducible R Markdown report.

> **Note:** This project uses a simulated/synthetic dataset for methodological demonstration. Findings should not be used for real clinical decision-making without validation on real-world data.

---

## Table of Contents

- [Overview](#overview)
- [Key Findings](#key-findings)
- [Methodology](#methodology)
- [Tech Stack](#tech-stack)
- [Repository Structure](#repository-structure)
- [How to Reproduce](#how-to-reproduce)
- [What This Project Demonstrates](#what-this-project-demonstrates)
- [Limitations](#limitations)
- [License](#license)

---

## Overview

This project analyzes 30,000 sport-related concussion events to answer four questions:

1. Can acute SCAT5 scores be turned into a clinically meaningful **severity scale**?
2. What **factors are statistically associated** with prolonged recovery, and do those associations survive correction for multiple comparisons?
3. Can we **predict** which athletes will have a prolonged return-to-play (RTP), and how good can that prediction realistically get?
4. Do athletes cluster into **natural recovery profiles** without using any outcome label?

The full analysis lives in a single, self-contained R Markdown document that renders to a polished HTML/PDF report with 40+ statistical visualizations.

---

## Key Findings

- Built a clinically-anchored **Low / Medium / Severe severity scale** by indexing each athlete's acute SCAT5 score to their own healthy baseline, rather than using an arbitrary population cutoff.
- Ran **15+ hypothesis tests** (t-tests, one- and two-way ANOVA with Tukey HSD, chi-square, Kruskal-Wallis) across **70+ pairwise comparisons**, applying **Bonferroni** and **Benjamini-Hochberg** correction to control false discovery — showing how many "significant" findings disappear once corrected for multiple testing at n = 30,000.
- Designed and executed a formal **A/B test** (two-proportion z-test, Cohen's h, power analysis) comparing high-contact vs. lower-contact sports on prolonged-RTP rate.
- Ran a **Kaplan-Meier / Cox proportional-hazards survival analysis** treating recovery time as a proper time-to-event outcome, producing hazard ratios and log-rank tests.
- Built, cross-validated, and benchmarked **8 machine learning algorithms** (logistic regression, decision tree, random forest, gradient boosting, elastic-net logistic regression, SVM, neural network, and a stacked ensemble), with class-imbalance correction and ROC-based threshold optimization.
- Went beyond "chase the metric": used a **variance-decomposition test** (regressing the underlying continuous outcome directly) to rigorously prove a ~0.65-0.69 AUC ceiling exists in the available features — and specified exactly which new clinical variables would be needed to break past it.
- Applied **PCA and K-means clustering** to identify data-driven recovery profiles independent of any outcome label.

---

## Methodology

| Stage | Techniques Used |
|---|---|
| **Data Prep & Scaling** | Custom severity scale construction, feature engineering, class balance analysis |
| **Exploratory Data Analysis** | Distribution analysis, correlation matrices, stratified visualizations |
| **Hypothesis Testing** | Welch's t-test, Wilcoxon rank-sum, one-way & two-way ANOVA, Tukey HSD, chi-square, Kruskal-Wallis, pairwise Wilcoxon post-hoc |
| **Multiple Testing Correction** | Bonferroni, Benjamini-Hochberg (FDR) across 70+ tests |
| **A/B Testing** | Two-proportion z-test, Cohen's h effect size, confidence intervals, statistical power analysis |
| **Survival Analysis** | Kaplan-Meier estimation, log-rank test, Cox proportional-hazards regression, hazard-ratio forest plots |
| **Predictive Modeling** | Logistic regression, decision tree, random forest, gradient boosting (GBM), elastic-net (glmnet), SVM, neural network, multinomial logistic regression, stacked ensemble |
| **Model Optimization** | 5-fold cross-validated hyperparameter tuning, down-sampling for class imbalance, Youden's J threshold optimization |
| **Model Diagnostics** | ROC/AUC comparison, confusion matrices, variable importance, variance-decomposition ceiling analysis |
| **Unsupervised Learning** | Principal Component Analysis, K-means clustering, silhouette validation |

---

## Tech Stack

- **Language:** R (4.3+)
- **Core packages:** `tidyverse`, `caret`, `glmnet`, `randomForest`, `ranger`, `gbm`, `kernlab`, `nnet`, `survival`, `ggfortify`, `cluster`, `factoextra`, `pROC`, `broom`, `car`, `pwr`
- **Reporting:** R Markdown → HTML / PDF (via `knitr`, `pandoc`, `xelatex`)

---

## Repository Structure

```
.
├── concussion_analysis.Rmd              # Full analysis source (single reproducible document)
├── Concussion_Biostatistics_Report.html # Rendered report (open directly in a browser)
├── Concussion_Biostatistics_Report.pdf  # Rendered report (PDF version)
├── data/
│   └── athlete_concussion_enriched_30k.csv
└── README.md
```

---

## How to Reproduce

1. Clone the repository and open it in RStudio (or run from the command line).
2. Install the required packages:

   ```r
   install.packages(c(
     "tidyverse", "caret", "glmnet", "randomForest", "ranger", "gbm",
     "kernlab", "nnet", "survival", "ggfortify", "cluster", "factoextra",
     "pROC", "broom", "car", "pwr", "corrplot", "gridExtra", "patchwork",
     "reshape2", "scales", "viridis", "rpart", "knitr", "rmarkdown"
   ))
   ```

3. Render the report:

   ```r
   rmarkdown::render("concussion_analysis.Rmd")
   ```

   This produces `concussion_analysis.html` in the project directory. A PDF can be generated by rendering with `output_format = "pdf_document"` (requires a LaTeX distribution such as TinyTeX or TeX Live).

> **Note on runtime:** the full document includes 5-fold cross-validated hyperparameter tuning across 8 models, so a full render from scratch can take several minutes. `knitr` caching (`cache=TRUE`) is enabled throughout, so subsequent renders after small edits are much faster.

---

## What This Project Demonstrates

- **Statistical rigor:** proper hypothesis testing, effect sizes (Cohen's d/h, Cramer's V, eta-squared), and multiple-testing correction rather than reporting raw p-values at scale.
- **Experimental design:** a fully specified A/B test with power analysis, not just a post-hoc comparison.
- **Applied biostatistics:** survival analysis (Kaplan-Meier, Cox regression) applied to a genuine time-to-event outcome.
- **ML engineering:** cross-validated tuning, imbalance handling, and threshold optimization across 8 distinct algorithms.
- **Scientific honesty:** rather than chasing marginal accuracy gains, the project explicitly quantifies the data's information ceiling and states what additional data would be needed to do better — a distinction between a good analyst and one who just runs models.
- **Communication:** a single, reproducible, decision-ready report that a non-technical stakeholder could still follow.

---

## Limitations

- The dataset is **simulated** (flagged via an `is_simulated` column) and is intended for methodological demonstration, not clinical use.
- No censoring is present in the survival analysis (all athletes eventually return to play), so the survival framework is used primarily for its comparative/visual tools (KM curves, log-rank tests) rather than to handle incomplete follow-up.
- Predictive performance (AUC ≈ 0.65-0.69) reflects a genuine information ceiling in the available features, documented explicitly in the report rather than masked by additional tuning.

---

## License

This project is available under the [MIT License](LICENSE).
