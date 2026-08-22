📊 Project Overview
Using Iowa Department of Revenue quarterly retail sales tax filings (48,074 city-quarter records), this project answers a series of financial and statistical questions:
- Do urban counties (Polk, Linn, Scott, Black Hawk, Johnson, Pottawattamie, Dubuque, Woodbury) behave differently from rural counties, statistically and economically?
- How concentrated is Iowa's retail tax base across its 99 counties?
- Can we forecast future quarterly statewide sales?
- Can machine learning predict taxable sales, and can it distinguish urban vs. rural counties from financial behavior alone?
- What do these patterns look like geographically, county by county, year by year?
Findings
**Statistical Testing**
- Taxable sales and average-sale-per-return are strongly right-skewed and fail normality tests (Shapiro-Wilk, p < 0.001), motivating non-parametric methods.
- Urban counties show significantly higher average sale-per-return than Rural counties (Mann-Whitney U, p < 0.001; Cohen's d in the "large effect" range).
- Kruskal-Wallis confirms significant variation in sale-per-return across all 99 counties (not just the binary Urban/Rural split).
- Number of returns filed and taxable sales are strongly positively correlated (Spearman r ≈ 0.88).
 
**A/B Test (Urban vs. Rural)**
- At the county-quarter level, the mean difference in avg. sale-per-return (Urban − Rural) is large and tightly estimated (95% CI does not include zero).
- The test is heavily overpowered at observed sample sizes (power ≈ 1.0); the effect would be detectable with a much smaller sample.
 
**Financial Analysis**
- Effective tax rates cluster around Iowa's 6% base state sales tax rate, with local-option taxes creating some dispersion above it.
- Revenue is concentrated: the Gini coefficient across counties indicates substantial inequality, and the 8 Urban counties account for a large majority of statewide taxable sales.
- YoY growth shows a clear 2020 COVID-19 dip followed by a 2021 rebound across both Urban and Rural counties.
 
**Time Series**
- The statewide quarterly series is non-stationary in levels (ADF fails to reject, KPSS rejects stationarity) but becomes stationary after seasonal (lag-4) differencing.
- A SARIMA(1,1,1)x(1,1,1,4) model forecasts the final 8 held-out quarters with single-digit MAPE.
 
**Machine Learning**
- Regression models (Linear, Ridge, Random Forest, Gradient Boosting, XGBoost) predicting quarterly taxable sales from lag/rolling features achieve R² > 0.98 on a time-respecting holdout, with lagged sales dominating feature importance.
- Classifiers distinguishing Urban vs. Rural counties from financial signatures alone (no county identity used) achieve ROC-AUC > 0.99, reinforcing the A/B test conclusion that Urban and Rural retail economies

🧠 Methodology
1. Data Cleaning & Feature Engineering
- Parsed quarterly dates; derived calendar_year, calendar_quarter
- Classified each of Iowa's 99 counties as Urban (8 counties with major metro areas) or Rural
- Derived avg_sale_per_return and effective_tax_rate
2. Exploratory Data Analysis & Statistical Visualization
- Distribution plots (raw vs. log-transformed), boxplots by county type, statewide time trend, Spearman correlation heatmap
3. Statistical Hypothesis Testing
- Shapiro-Wilk normality tests + Q-Q plots
- Levene's test for equal variances
- Mann-Whitney U (primary) and Welch's t-test (Urban vs. Rural)
- Kruskal-Wallis across all 99 counties
- Pearson and Spearman correlation (returns filed vs. taxable sales)
4. A/B Testing
- County-quarter level comparison (Urban = treatment, Rural = control) with mean difference, 95% confidence interval, Cohen's d, and post-hoc statistical power via statsmodels
5. Financial Analysis
- Effective tax rate distribution vs. Iowa's 6% base rate
- Year-over-year growth trends (Urban vs. Rural), including the 2020 COVID-19 dip
- Revenue concentration via Lorenz curve and Gini coefficient; top 10 counties by total taxable sales
6. Time Series Modeling & Testing
- Augmented Dickey-Fuller (ADF) and KPSS stationarity tests
- Seasonal decomposition (additive, period = 4)
- ACF/PACF analysis
- SARIMA(1,1,1)x(1,1,1,4) fit on a time-respecting train/test split, evaluated with MAE, RMSE, and MAPE
7. Machine Learning — Regression
- Predicting quarterly taxable sales from lagged sales, rolling averages, returns filed, and county type
- Models compared: Linear Regression, Ridge Regression, Random Forest, Gradient Boosting, XGBoost
- Time-respecting (not random) train/test split to avoid lookahead leakage
- Evaluated with MAE, RMSE, R², MAPE; feature importance and residual diagnostics
8. Machine Learning — Classification
- Predicting Urban vs. Rural county status purely from financial signature (log sales, log returns, avg. sale-per-return, effective tax rate)
- Models compared: Logistic Regression, Random Forest, XGBoost
- Evaluated with accuracy, precision, recall, F1, ROC-AUC, confusion matrices, ROC curves
9. Interactive Visualizations (Plotly)
- Statewide quarterly sales with a draggable range slider
- Top 15 counties by taxable sales, animated year-by-year
- Urban vs. Rural distribution, animated by year
- YoY growth scatter by county, hoverable
- Effective tax rate distribution by year
10. Iowa County Map
- Choropleth of all 99 counties by taxable sales, animated by year, built from county FIPS codes matched to US Census county boundaries
Log-scaled snapshot of the latest year to surface variation among rural counties otherwise dwarfed by urban totalsbehave distinctly.
