# Netflix vs. Disney: Content Analytics & Intelligence Project

A statistics, machine learning, and time-series study of streaming catalog data. The project compares the Netflix and Disney catalogs from several angles: how each catalog grew over time, how the two platforms differ statistically, whether the differences are predictable, what the descriptions say about positioning, and whether each platform's acquisitions are getting fresher or older.

---

## Table of Contents

1. [Project Overview](#1-project-overview)
2. [Dataset](#2-dataset)
3. [Tech Stack](#3-tech-stack)
4. [Getting Started](#4-getting-started)
5. [Methodology: What Was Done](#5-methodology-what-was-done)
6. [Key Results](#6-key-results)
7. [Limitations & Honest Caveats](#7-limitations--honest-caveats)
8. [Recommendations & Next Steps](#8-recommendations--next-steps)
9. [Repository Structure](#9-repository-structure)

---

## 1. Project Overview

The project is built around one comparison, **Netflix vs. Disney**, and asks a set of questions that build on each other:

| Question | Technique |
|---|---|
| What does each catalog look like? | Cleaning, EDA, interactive Plotly charts |
| How has each catalog grown, and can it be forecast? | Time-series analysis: Mann-Kendall, ADF, decomposition, Holt-Winters, SARIMA |
| Which variables are normally distributed? | Shapiro-Wilk, D'Agostino-Pearson, Q-Q plots, Yeo-Johnson transform |
| Which platform differences are statistically real? | 7 hypothesis tests with Benjamini-Hochberg FDR correction |
| Are the differences practically meaningful? | A/B testing with z-tests, Welch's t-test, effect sizes, power analysis |
| Can metadata predict the platform, or how old a title is when added? | Logistic/Linear Regression, Random Forest |
| What do the descriptions reveal about positioning? | TF-IDF, NMF topic modeling, keyword differentiation, sentiment analysis |
| Do content "types" cut across platform lines? | K-Means, PCA, hierarchical clustering |
| Is each platform's acquisition strategy getting fresher or staler? | Cohort analysis and trend testing |

---

## 2. Dataset

**File:** `tv_movie_shows.csv`

- **9,338 titles**: 7,926 Netflix (84.9%) and 1,412 Disney (15.1%)
- **51 columns**: cast, director, country, genre (`listed_in`), rating, duration, description, release year, and catalog-addition date, plus many engineered features
- **Mix:** Netflix is 69.5% movies, Disney 72.4% movies

**Key columns used**

| Column | Meaning |
|---|---|
| `platform` | Netflix or Disney (the main comparison axis) |
| `type`, `is_movie` | Movie or TV Show |
| `date_added`, `year_added`, `month_added` | When the title joined the catalog |
| `release_year`, `decade_released` | Original release |
| `content_age_years` | `year_added - release_year` (age when added) |
| `duration_minutes` / `num_seasons` | Movies / TV shows |
| `rating`, `maturity_level` | Content rating and a numeric maturity encoding |
| `num_cast_members`, `num_directors`, `num_countries`, `num_genres` | Count features |
| `description_word_count`, `title_word_count` | Text-length features |
| `production_scale_index`, `international_scope_score`, `metadata_completeness_score`, `freshness_index` | Pre-engineered composite scores |

---

## 3. Tech Stack

- **Data:** `pandas`, `numpy`
- **Visualization:** `matplotlib`, `seaborn`, `plotly`, `wordcloud`
- **Statistics:** `scipy`, `statsmodels`, `pymannkendall`
- **Machine learning:** `scikit-learn`
- **NLP:** `scikit-learn` (TF-IDF, NMF), `textblob`

---

## 4. Getting Started

```bash
# 1. Install dependencies
pip install pandas numpy matplotlib seaborn plotly scipy statsmodels \
            pymannkendall scikit-learn wordcloud textblob jupyter

# 2. Place tv_movie_shows.csv in the same folder as the notebook

# 3. Launch and run all cells
jupyter notebook NetflixDisneyAnalyticsProject.ipynb
```

All random seeds are fixed (`random_state=42`), so results are reproducible.

---

## 5. Methodology: What Was Done

### 5.1 Data Loading & Cleaning
- Loaded the CSV (9,338 rows × 51 columns) and visualized missing values per column.
- Treated most missingness as **structural** rather than erroneous: `duration_minutes` exists only for movies, `num_seasons` only for TV shows, and some `director`/`cast`/`country` values are unlisted on the platforms themselves.
- Parsed `date_added` to datetime and confirmed there were **0 duplicates** on (title, release_year, platform).
- Filled missing `director`, `cast`, `country`, and `rating` with `"Unknown"`.
- Built `df_ts`, the subset with a valid add-date (**9,245 of 9,338 rows**), for time-series work.
- Created exploded (one-row-per-value) views of genres (21,212 rows) and countries (10,497 rows) for multi-value analysis.

### 5.2 Exploratory Data Analysis
Static charts, always split by platform:
- Titles by platform and Movie/TV split
- Top 15 genres overall, and genre share within each platform
- Top 12 producing countries
- Content rating distribution by platform
- Movie duration distributions (KDE)
- Release-year histogram and content age at time of adding
- Correlation heatmap of 13 numeric features

### 5.3 Interactive Visualizations (Plotly)
- Titles added per year (line chart)
- Cumulative catalog growth (area chart)
- Platform → Type → Rating **sunburst**
- Genre **treemap** by platform (genres with 15+ titles)
- Movie duration box plot
- Content age vs. cast size scatter (bubble size = number of genres)

### 5.4 Time-Series Analysis of Content Growth
- Built monthly "titles added" series per platform (167 months, Jan 2008 to Nov 2021).
- **Augmented Dickey-Fuller** stationarity test.
- **Mann-Kendall** trend test on yearly totals, with Sen's slope.
- **Seasonal decomposition** (additive, period = 12) of the Netflix series.
- **Year-over-year growth rates** for both platforms.
- **Forecasting:** Holt-Winters Exponential Smoothing vs. SARIMA(1,1,1)(1,1,1,12), with a 12-month hold-out, RMSE comparison, and 95% confidence band.

### 5.5 Normality Testing & Transformation
- Ran **Shapiro-Wilk** (sampled to 4,999 rows, since it is unreliable on large samples) and **D'Agostino-Pearson** (full data) on `duration_minutes`, `content_age_years`, and `num_cast_members`.
- Added **Q-Q plots** for visual confirmation.
- Applied a **Yeo-Johnson power transform** (chosen over Box-Cox because it handles zeros) and re-tested.
- This step justified using **non-parametric tests** in the next section.

### 5.6 Multiple Hypothesis Testing (with FDR Correction)
Seven simultaneous tests, corrected with **Benjamini-Hochberg FDR**:

| # | Test | Question |
|---|---|---|
| 1 | Mann-Whitney U | Do movie durations differ by platform? |
| 2 | Chi-square | Is content type associated with platform? |
| 3 | Chi-square | Is content rating associated with platform? |
| 4 | Kruskal-Wallis | Does duration differ across release decades? |
| 5 | Spearman | `release_year` vs. `num_cast_members` |
| 6 | Spearman | `content_age_years` vs. `description_word_count` |
| 7 | Mann-Whitney U | Does metadata completeness differ by platform? |

### 5.7 A/B Testing Framework
Netflix = Group A, Disney = Group B.
- **Two-proportion z-tests** with Wilson confidence intervals and **Cohen's h** for: multi-genre rate, US-production rate, multi-country co-production rate, and director-listed rate.
- **Welch's t-test** with **Cohen's d** for the continuous metadata completeness score.
- **Power analysis** (`TTestIndPower`): achieved power for the observed effect, and the sample size needed to detect a small effect (d = 0.2) at 80% power.

### 5.8 Machine Learning
**Classification: Netflix or Disney?**
- Models: Logistic Regression (scaled, balanced class weights) and Random Forest (300 trees, depth 10, balanced).
- 18 features, stratified 75/25 split, 9,254 modeling rows.
- Platform-encoding columns (`is_netflix`, `netflix_movie_flag`, `disney_tv_flag`) were **excluded to prevent target leakage**.
- Evaluated with accuracy, ROC-AUC, classification report, confusion matrix, ROC curves, and feature importance.
- **Honesty check:** `description_word_count` dominated feature importance (about 51%), making the task nearly trivial. The model was **refit without `description_word_count` and `title_word_count`** to measure the remaining signal.

**Regression: predicting `content_age_years`**
- Linear Regression baseline vs. Random Forest Regressor.
- `release_year` was excluded because it would leak the target directly.
- Evaluated with R², RMSE, MAE, an actual-vs-predicted plot, and feature importance.

### 5.9 NLP & Text Analysis
- **TF-IDF** vectorization (unigrams + bigrams, 3,000 features, `min_df=5`).
- **NMF topic modeling** with 8 topics, plus the platform composition of each topic.
- **Keyword differentiation:** the words with the largest mean TF-IDF gap between platforms.
- **Word clouds** per platform.
- **Sentiment analysis** (TextBlob polarity and subjectivity) with a Mann-Whitney U test on polarity.

### 5.10 Clustering & Segmentation
- Standardized 10 numeric features (9,254 titles).
- Chose *k* using the **elbow method** and **silhouette score** (suggested k = 4).
- Fit **K-Means (k = 4)**, visualized in **PCA** space (the first two components explain 31.1% and 18.4% of variance), and compared cluster membership against platform.
- Profiled each cluster numerically and by dominant genres.
- Added a **Ward-linkage dendrogram** on a 300-title subsample as a hierarchical cross-check.

### 5.11 Vintage & Cohort Analysis
- **Cohort heatmap:** release decade × year added.
- Median **content age at add-time** by add-year cohort, per platform.
- **Mann-Kendall** trend test on that freshness series.
- Box plots of age distribution for the last 10 cohorts.
- Trend of the engineered `freshness_index`.
- Release-decade mix within each platform's catalog.

---

## 6. Key Results

### Catalog composition
- Netflix is about 5.6× larger than Disney in this dataset and skews international and multi-genre. Disney is concentrated in family genres and US productions.

### Growth over time
| Finding | Result |
|---|---|
| Netflix yearly additions trend | **Increasing**, p = 0.0001, Sen's slope ≈ **134 titles/year** |
| Disney yearly additions trend | Increasing, p = 0.0256 (Sen's slope ≈ 0, see caveats) |
| ADF test, Netflix | Non-stationary (p = 0.898) |
| ADF test, Disney | "Stationary" (p < 0.001), but see caveats |
| Hold-out RMSE (12 months) | **Holt-Winters 69.9** vs. SARIMA 72.7. Holt-Winters is marginally better. |

### Statistical tests
All 7 tests remain significant after Benjamini-Hochberg correction. The strongest:

| Test | Result |
|---|---|
| Rating vs. platform (χ²) | χ² = 3,451.8, p ≈ 0 (strongest difference) |
| Movie duration, Netflix vs. Disney | Netflix mean **99.7 min** vs. Disney **71.3 min**, p ≈ 6e-96 |
| Duration across release decades | H = 406.1, p ≈ 7e-82 |
| Content age vs. description length | ρ = -0.122 |
| Release year vs. cast size | ρ = -0.084 |
| Metadata completeness | Netflix 2.51 vs. Disney 2.38, p ≈ 1e-5 |
| Content type vs. platform | χ² = 4.69, p = 0.030 (weakest, and practically small) |

### A/B tests

| Metric | Netflix | Disney | Cohen's h / d | Effect |
|---|---|---|---|---|
| Multi-genre listing rate | 77.1% | 95.5% | -0.572 | Large |
| US-production rate | 42.0% | 81.4% | -0.840 | Large |
| Multi-country co-production | 15.0% | 12.8% | 0.063 | Small |
| Director-listed rate | 69.8% | 67.1% | 0.060 | Small |
| Metadata completeness (mean) | 2.51 | 2.38 | d = 0.180 | Small |

The study is heavily over-powered for the metadata test (power = 1.00). Only about **393 titles per group** would be needed to detect a small effect at 80% power. The practical takeaway is that **US-production share and multi-genre labeling are the real strategic differences**. The others are statistically detectable but tiny.

### Machine learning
| Model | Accuracy | ROC-AUC |
|---|---|---|
| Logistic Regression (all features) | 0.990 | 0.999 |
| Random Forest (all features) | 0.995 | 1.000 |
| Random Forest (without text-length features) | **0.964** | **0.993** |

- Even without description length, platform remains highly predictable, so the two catalogs have distinct structural profiles.
- **Regression (content age):** Linear R² = 0.191, Random Forest R² = 0.346 (RMSE 10.37 years, MAE 5.97 years). This is modest, as expected, because licensing decisions are not fully captured by metadata.

### NLP
- Topics include coming-of-age/young-adult stories, Disney classic characters, documentaries and WWII, school and teen content, family dramas, and children's content.
- **Sentiment:** Disney descriptions are more positive (mean polarity **0.103** vs. Netflix **0.062**, p = 0.0004), consistent with Disney's family-friendly positioning.

### Clustering (K = 4)
| Cluster | Size | Profile | Top genres |
|---|---|---|---|
| 0 | 1,114 | Small cast, short descriptions, low maturity | Family, Comedy, Animation |
| 1 | 735 | Large cast, ~3 countries, high maturity | Dramas, International Movies, Independent Movies |
| 2 | 4,614 | Large cast, single country, high maturity | International Movies, Dramas, Comedies |
| 3 | 2,791 | Small cast, few directors, fewer genres | Documentaries, International TV Shows, Docuseries |

### Vintage & cohort
- **Netflix's acquisitions are getting significantly fresher** over time: Mann-Kendall trend decreasing, p < 0.0001, Sen's slope ≈ -1 year per cohort-year.
- The same trend test was not run for Disney because its add-year history is too short (see caveats).

---

## 7. Limitations & Honest Caveats

These are worth knowing before drawing strong conclusions.

1. **Truncated / uneven time windows.** The monthly series runs Jan 2008 to Nov 2021, but Netflix shows **0 additions for Oct and Nov 2021**. This looks like a data cutoff rather than a real pause, and it affects the stationarity test and the forecast hold-out window.
2. **Disney's time series is short.** Disney additions begin around 2019 (the 2019 YoY growth is `inf`), and the earlier months are zero-filled. This is why Disney's ADF test says "stationary" (a long run of zeros followed by activity is a poor stationarity test) and why Sen's slope is 0.00 despite a significant trend p-value. Treat Disney time-series results as indicative only.
3. **Disney YoY growth** is undefined before 2019, and 2020 and 2021 show declines, partly because 2021 is a partial year.
4. **Classifier leakage-adjacent signal.** Description length alone nearly identifies the platform. The reduced-feature model is the more meaningful result.
5. **Class imbalance** (85/15) is handled with `class_weight="balanced"` and stratified splits, but accuracy alone can mislead. ROC-AUC and per-class precision/recall are reported alongside it.
6. **Normality is rarely achieved**, even after transformation, which is why non-parametric tests are used throughout.
7. **Large samples make tiny effects significant.** This is why effect sizes (Cohen's h/d) are reported next to every p-value.
8. **Forecast uncertainty.** Catalog additions reflect licensing and business decisions, so forecasts are directional rather than precise.
9. **Clusters are descriptive**, not ground truth. The silhouette-chosen k = 4 is a reasonable summary, not a definitive taxonomy.

---

## 8. Recommendations & Next Steps

- **Time series:** use weekly or daily data if available to sharpen the seasonality estimate, and refresh the dataset to remove the 2021 truncation.
- **Content quality:** bring in external data (IMDb ratings, awards, subscriber counts, viewership) to test hypotheses about quality and performance, not just catalog composition.
- **Monitoring:** re-run the A/B comparisons on a rolling basis to see whether the platform differences are narrowing or widening as catalogs grow.
- **Disney analysis:** repeat the cohort and trend work once Disney has a longer add-date history.
- **Modeling:** try gradient boosting and add TF-IDF/text-embedding features to the classifier. Add cross-validation and hyperparameter tuning.

---

## 9. Repository Structure

```
.
├── NetflixDisneyAnalyticsProject.ipynb   # Full analysis notebook
├── tv_movie_shows.csv                    # Dataset (not included here; add before running)
└── README.md                             # This file
```
