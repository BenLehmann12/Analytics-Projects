# Boston Celtics Analytics: Five Seasons, 2021-22 to 2025-26

An end-to-end analytics project on the Boston Celtics' last five regular seasons (410 games, 104 player-seasons). It covers data auditing, interactive visualization, hypothesis testing, statistical modelling and machine learning, with an emphasis on **honest inference**: effect sizes with confidence intervals, multiple-comparison corrections, power analysis, and clear reporting of null results.

Everything lives in one notebook, [`celtics_analytics.ipynb`](celtics_analytics.ipynb), backed by a helper module, `celtics_utils.py`.

---

## Headline findings

| Area | Finding |
|---|---|
| **Team trajectory** | Wins went 51 → 57 → **64** → 61 → 56. The proxy net rating peaked at 11.4 in 2023-24. |
| **Luck vs. skill** | Season mean margins are statistically indistinguishable across the five years (ANOVA p = .27). Binomial luck alone is worth about ±4 wins. 2021-22 won ~8 fewer games than its point differential implied. |
| **Home court** | Home win% 75.6% vs. road 65.4% (+10.2 pp, 95% CI +1.5 to +19.0). Suggestive, but it does **not** survive Holm correction across the test family. |
| **Rest / back-to-backs** | No detectable effect, but the minimum detectable effect is ±5.6 pts, so moderate effects can't be ruled out. |
| **Clutch / momentum** | Overtime and close-game records are coin-flips, with no streak dependence. |
| **Style shift** | 3PA/FGA rose from .425 to .536 and the shot mix moved away from the rim. 3P% itself did not change significantly. |
| **Opponent identity** | Opponent-specific effects are indistinguishable from noise (ICC ≈ 0.005). |
| **3P% "improvements"** | 0 of 26 year-over-year changes survive FDR correction. Empirical-Bayes shrinkage cut next-season 3P% prediction error by ~26%. |
| **Predicting wins (ML)** | Best out-of-sample AUC ≈ 0.57, no better than a home-only baseline. This is a legitimate negative result: pre-game schedule data carries very little signal. |
| **Player archetypes** | Three stable groups: perimeter shooters/connectors, high-usage scorers, rim-running bigs. |

See Section 7 of the notebook for the full table, strength-of-evidence ratings and limitations.

---

## What's in the notebook

| Section | Contents |
|---|---|
| **0 · Setup** | Imports, theme, paths, helper for saving interactive figures as standalone HTML |
| **1 · Data audit & feature engineering** | Cross-file reconciliation (schedule ↔ team totals ↔ player rows), data-quality quirks, and leakage-free pre-game features (rest, back-to-backs, form, run length, tip-off time, etc.) |
| **2 · Interactive EDA** | Season races, rolling form, margin distributions, season "barcode", opponent heatmap, team-identity trends, animated player explorer, usage vs. efficiency, points sunburst, minutes distribution, radar profiles, correlations |
| **3 · Basketball visuals** | Interactive half-court shot-zone map (any player-season or whole team), shot-profile evolution, shot distance vs. efficiency |
| **4 · Hypothesis & A/B testing** | 15 tests (home court, back-to-backs, rest dose-response, season differences, clutch, momentum, matinees, attendance, 3-point revolution, shot-zone shift, starters vs. bench, player 3P% changes), plus power analysis, a Bayesian Beta-Binomial view, and Holm / Benjamini-Hochberg corrections |
| **5 · Statistical modelling** | Logistic regression, OLS with robust SEs and diagnostics, mixed-effects opponent model, Monte-Carlo season simulation, empirical-Bayes 3P% shrinkage |
| **6 · Machine learning** | Win prediction with leave-one-season-out CV and forward-chaining checks, margin regression, K-Means/PCA player archetypes, next-season player forecasting |
| **7 · Findings & caveats** | Summary table, why the ML "failed", and limitations |
| **8 · Next steps** | Opponent context, player game logs, play-by-play, hierarchical Bayesian models |

---

## Methods at a glance

- **Unit of analysis:** the game (n = 410) for schedule questions; team-season or player-season for roster questions.
- **Testing framework:** α = 0.05 two-sided. Where feasible, each test is run parametrically *and* non-parametrically (Welch t, Mann-Whitney, permutation, bootstrap CIs). Effect sizes (Cohen's d / h) are always reported.
- **Multiple testing:** Holm (family-wise error) and Benjamini-Hochberg (FDR) applied across the logged test family.
- **Validation:** leave-one-season-out cross-validation, with a forward-chaining robustness check and paired-bootstrap comparison against baselines.
- **No leakage:** all game features are known before tip-off.
- **Causal language is avoided.** Schedule factors aren't randomized, so results are associations.

---

## Getting started

### Requirements

Python 3.10+ and:

```
numpy  pandas  scipy  statsmodels  scikit-learn
plotly  jupyter
xgboost   # optional
```

```bash
pip install numpy pandas scipy statsmodels scikit-learn plotly jupyter xgboost
```

### Data

The notebook expects four CSV files in a single folder: three player-level files (including a shooting file whose name contains `shooting`) and one game schedule file. Set the folder path in the setup cell:

```python
DATA_DIR = Path("path/to/your/data")   # <- folder holding the 4 CSVs
```

`celtics_utils.load_all(DATA_DIR)` merges the player files on *(season, player)*, separates the `Team Totals` rows into a team table, and builds the game table.

### Run

```bash
jupyter notebook celtics_analytics.ipynb
```

Run the cells top to bottom. Figures are displayed interactively and the key ones are also saved as standalone HTML files to `outputs/figures/`.

> **Tip:** the saved notebook includes embedded interactive Plotly output and is large. GitHub may not render it fully. For viewing, open the HTML files in `outputs/figures/` or export the notebook with `jupyter nbconvert --to html celtics_analytics.ipynb`.

---

## Repository structure

```
.
├── celtics_analytics.ipynb    # main analysis notebook
├── celtics_utils.py           # data loading, stats helpers, plotting theme, shot-zone court
├── outputs/
│   └── figures/               # standalone interactive HTML figures (generated)
└── README.md
```

---

## Data caveats

- Observational data: comparisons are associations, and opponent strength is an unmeasured confounder.
- Small samples (410 games, 104 player-seasons). Effects under roughly 4-6 points of margin are undetectable.
- The shooting file's team-total *Avg Dist* disagrees with the FGA-weighted player average in 4 of 5 seasons, so team shot profiles are recomputed from player rows.
- Shot-zone attempts are reconstructed from rounded percentages (accurate to about ±1 shot).
- No opponent box scores, so defensive rating is a proxy.
- Home attendance is a constant 19,156 in the data.
- Regular season only; no playoff data.

---

## Possible extensions

- Add opponent strength (net rating, pace, injuries, rest) and rebuild the win model with an Elo or Bradley-Terry rating.
- Use player game logs for on/off splits and availability effects (e.g., with vs. without Tatum).
- Use play-by-play or tracking data for true shot-location heat maps and lineup analysis.
- Fit Bayesian hierarchical models of player skill with aging curves.

---

## Data sources

Add your data source here (e.g., Basketball-Reference) and respect its terms of use when sharing the raw CSVs.

## License

Add a license of your choice (e.g., MIT).

## Example
https://github.com/user-attachments/assets/66acdd47-cfbc-4b29-87a3-9b6947c73b52


