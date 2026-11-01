# Box-Office Intelligence
### A Statistical and Machine-Learning Analysis of ~10,000 Movies (TMDB)

An end-to-end analytics project on the film industry: data audit and cleaning, exploratory analysis, descriptive and inferential statistics, multiple-hypothesis testing, quasi-experimental A/B testing, supervised machine learning, and unsupervised segmentation. All code is Python, in one executable notebook.

---

## 1. Project statement

I analyse a TMDB extract of ~10,000 films to answer decision-relevant questions a studio, financier, or acquisition team would ask: what drives revenue and ROI, which industry "rules of thumb" survive statistical scrutiny, and how well outcomes can be predicted *before* release.

Principles followed throughout:
- Every claim is reported with an **effect size and confidence interval**, not just a p-value.
- Models are judged on a **temporal hold-out** (train on the past, test on the future).
- **Leakage, selection bias, and confounding** are tested or disclosed explicitly.

### Business questions

| # | Question | Main methods |
|---|----------|--------------|
| Q1 | What does the landscape look like, and how has it changed? | EDA, trend tests |
| Q2 | How strongly does budget drive revenue? | Correlation, log-log regression |
| Q3 | Do big-budget films carry more or less relative risk? | Variance tests, ROI distributions |
| Q4 | Do release timing, language, genre, and studio backing matter? | t / Mann-Whitney, ANOVA / Kruskal, chi-square, OLS, logistic |
| Q5 | Which genres genuinely stand out after correcting for many tests? | Bonferroni, Holm, Benjamini-Hochberg, permutation max-T |
| Q6 | Is "summer release" or "studio backing" a real effect or a proxy for bigger budgets? | A/B framework, power, Bayesian A/B, propensity matching |
| Q7 | Can we predict revenue and hit/flop before release, with honest uncertainty? | Ridge, Random Forest, XGBoost, quantile GBM, SHAP |
| Q8 | Are there natural film "types", and which films beat expectations? | K-Means, GMM, PCA, residual analysis |

### Hypotheses (declared before testing)
H1 budget is the strongest revenue predictor (sub-linear) - H2 ROI variability falls with budget - H3 summer/holiday windows outperform - H4 English-language films earn more - H5 genre matters, but fewer genres survive multiple-testing correction - H6 rating correlates with revenue, weakly relative to budget - H7 major-studio backing raises hit probability after matching - H8 co-productions are rated higher.

### Key definitions
- **ROI** = revenue / budget. **Hit** = revenue >= 2x budget (the industry "2x rule").
- **Pre-release features** (budget, runtime, genres, month, countries, studio flags, prior track record of director / lead actor) are the only inputs to predictive models. Post-release signals (`vote_count`, `popularity`, `vote_average`) are used for description and clustering only.

---

## 2. Repository contents

```
entertainment_analytics/
|-- entertainment_analytics_project_executed.ipynb   # full notebook with all outputs
|-- entertainment_analytics_report.html              # same notebook as a shareable report
|-- README.md
|-- requirements.txt
|-- figures/                  # ~50 charts exported as PNG
|-- models/                   # fitted pipelines (joblib): revenue regressor, hit classifier
|-- notebook_source_parts/    # plain-text source the notebook was assembled from
\-- build_nb.py               # assembles notebook_source_parts/*.txt into an .ipynb
```

The dataset (`tmbddataset.csv`) is **not** included; supply your own copy.

---

## 3. How to run

```bash
pip install -r requirements.txt
# put tmbddataset.csv next to the notebook (or in a ./data/ folder)
jupyter lab entertainment_analytics_project_executed.ipynb   # then Run All
```

- Full run takes roughly **20 minutes** (bootstrap, permutation, and hyper-parameter-search cells dominate).
- A fixed random seed (42) is used throughout.
- The notebook creates `figures/` and `models/` in the working directory.
- Tested with Python 3.12, pandas 3.0, scikit-learn 1.8, statsmodels 0.15, XGBoost 3.4. Older versions may need small adjustments (for example, `DataFrame.map` replaced `applymap` in pandas 3).

### Expected input columns
`id, title, budget, revenue, genres, runtime, release_date, production_companies, production_countries, cast`, plus `director, vote_average, vote_count, popularity, original_language`. List-like columns are stored as text and parsed in the notebook.

### Troubleshooting
If a `statsmodels` logit/OLS cell raises `LinAlgError: Singular matrix`, a covariate is constant or perfectly collinear in your data (or the columns are badly scaled). Drop constant/collinear columns and standardise continuous covariates before calling `smf.logit`; fall back to `.fit(method='bfgs')` if Newton's method still fails.

---

## 4. Notebook outline

| Section | Content |
|---|---|
| 1 | Project statement, questions, hypotheses |
| 2 | Setup |
| 3 | Data audit and cleaning (logged), selection-bias check |
| 4 | Feature engineering, including talent track-record features built without look-ahead |
| 5 | EDA: distributions, time trends, genre economics, co-occurrence lift, seasonality, language/country, studios, directors, reception, correlations |
| 6 | Descriptive statistics: skew/kurtosis, normality tests and Q-Q plots, outliers, correlations with CIs, partial correlation, mutual information, trend tests |
| 7 | Inferential statistics: bootstrap/Wilson CIs, Welch t and Mann-Whitney, ANOVA/Kruskal/Tukey, Brown-Forsythe, chi-square, OLS with diagnostics, logistic regression, power analysis |
| 8 | Multiple testing: simulation, placebo test, Bonferroni/Holm/BH/BY, permutation max-T |
| 9 | A/B testing: A/A and peeking demos, frequentist + Bayesian tests, propensity matching, three experiments |
| 10 | Machine learning: regression and classification, prediction intervals, permutation importance and SHAP, leakage demonstration, learning curve, decile lift |
| 11 | Clustering: K-Means, GMM, Ward, PCA, stability and profiling |
| 12 | Over- and under-performers versus out-of-fold expectation |
| 13 | Conclusions, hypothesis scorecard, recommendations, limitations, next steps |

---

## 5. Headline results

- **Data:** 9,999 rows -> 9,733 usable; **4,582** films have usable budget and revenue (about 48% of budgets and 44% of revenues were placeholder zeros).
- **Budget** dominates revenue: elasticity ~0.73 (95% CI 0.68-0.78). ROI dispersion shrinks as budgets grow.
- **English vs non-English:** large revenue gap, but **no ROI difference** - it is a budget effect.
- **Studio backing:** about **+16 percentage points** in hit probability after matching; the strongest tactic tested, and it survives correction across experiments.
- **Summer release:** the naive +6.4 pp hit-rate lift roughly **halves** after matching (PSM +3.0 pp, CI -0.5 to +6.5); revenue lift shrinks from about +40% to about +14%.
- **Co-productions** are rated slightly *lower* than single-country films (negligible), opposite to H8.
- **Genre:** 27 of 34 tests significant uncorrected, 22 survive Bonferroni; most effects are small.
- **Revenue model (future films):** R^2 ~0.37, within a factor of 2 of actual revenue for ~44% of films (naive baseline: ~29%). Ridge, random forest, and XGBoost perform almost identically.
- **Hit/flop classifier:** test ROC-AUC ~0.67; more useful for ranking (top-scored decile hit 67% of the time versus 15% for the bottom decile).
- **Leakage:** adding vote count / popularity lifts R^2 from 0.37 to 0.54 - unusable, because those signals do not exist before release.
- **Segments:** four loosely separated types (small and niche, acclaimed mid-budget, mainstream commercial, blockbusters) with hit rates of 38% / 60% / 56% / 83%.

---

## 6. Limitations

- **Selection bias:** only films with disclosed financials are analysed; they skew English, US, and commercial.
- **Nominal dollars:** no inflation adjustment; `year` is controlled for. Revenue may be incomplete for recent and streaming-original films (flagged, not removed).
- **Observational data:** propensity matching adjusts only for measured covariates. Marketing spend, distribution breadth, and star power beyond prior-film averages are unobserved, so A/B-style effects are associations, not proven causes.
- **Temporal drift:** hit rate falls from ~64% (train) to ~45% (test). The nominal 80% prediction interval covered only ~65% of test films and needs recalibration before real use.
- **Data quality:** values under $10,000 were treated as missing; remaining revenue figures cannot be independently verified. Cast is limited to five names.
- **Cluster quality:** silhouette ~0.20, so segments are descriptive buckets on a continuum rather than natural categories.

## 7. Possible next steps
Inflation (CPI) adjustment and marketing spend; time-aware conformal prediction intervals; text features from overviews and keywords; richer talent and franchise features; an interactive scenario dashboard; doubly-robust causal estimators with sensitivity analysis for hidden confounding.

## 8. Data source
Movie data originates from TMDB (The Movie Database). Check TMDB's terms of use before redistributing the data or derived outputs.
