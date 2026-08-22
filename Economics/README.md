
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
- Classifiers distinguishing Urban vs. Rural counties from financial signatures alone (no county identity used) achieve ROC-AUC > 0.99, reinforcing the A/B test conclusion that Urban and Rural retail economies behave distinctly.
