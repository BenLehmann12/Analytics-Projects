# ⚾ Cubs Pitching Analytics

**Bayesian hierarchical modeling, machine learning, Monte Carlo simulation and interactive visualization on 25,000 Statcast fastballs.**

A three-notebook Python project that asks a practical analyst question: *how much of what we see in a pitcher's (or batter's) numbers is real skill, and how much is luck?* It then uses the answer to build predictive models, a pitch-by-pitch simulator, and a set of interactive baseball visualizations.

`Python 3.12` · `PyMC` · `LightGBM` · `SHAP` · `statsmodels` · `Plotly` · `pandas`

---

## Table of contents

1. [Headline findings](#headline-findings)
2. [What's in the repository](#whats-in-the-repository)
3. [About the data (read this first)](#about-the-data-read-this-first)
4. [Notebook 1: Pitching Analytics Lab](#notebook-1--pitching-analytics-lab)
5. [Notebook 2: Interactive Visualization Studio](#notebook-2--interactive-visualization-studio)
6. [Notebook 3: Bayesian Hierarchical Modeling](#notebook-3--bayesian-hierarchical-modeling)
7. [Methodology notes and design decisions](#methodology-notes-and-design-decisions)
8. [Validation and diagnostics](#validation-and-diagnostics)
9. [Limitations](#limitations)
10. [Installation and how to run](#installation-and-how-to-run)
11. [Troubleshooting](#troubleshooting)
12. [Project structure](#project-structure)
13. [Roadmap](#roadmap)
14. [Acknowledgments and data notice](#acknowledgments-and-data-notice)

---

## Headline findings

All numbers below come directly from the notebooks' output (fixed random seeds; MCMC results can differ slightly across machines and core counts).

**How much is skill vs. luck? (Notebook 3, hierarchical Bayesian model of whiff-on-swing)**

| Source of variation in swing-and-miss | Share of variance |
|---|---|
| Pure swing-by-swing luck (residual) | **76.3%** |
| Pitch characteristics and context (velocity, spin, location, count, platoon) | 12.2% |
| Batter | 5.7% |
| Pitcher (persistent skill) | **5.5%** |
| Pitcher-season drift (year-to-year change for the same pitcher) | 0.3% |

- **Batters matter as much as pitchers for whiffs, and far more for contact quality.** Exit velocity varies about **2.5x as much between batters (2.6 mph SD) as between pitchers (1.0 mph SD)**.
- **Partial pooling predicts new data better.** On a held-out 2026 season no model saw, adding pitcher and batter effects improved the log score by about 0.01 per swing over a pooled model (AUC 0.70 vs. 0.68).
- **Chase and selectivity are one trait.** The posterior correlation between a batter's chase tendency and his selectivity is **-0.77** (90% CI -0.87 to -0.64). Part of this is mechanical because swing rates are bounded.
- **The simulator reproduces reality.** Posterior-predictive simulated whiff rates correlate **r = 0.96** with each pitcher's real rate across 29 pitchers.

**Machine learning and statistics (Notebook 1)**

- **Pitch height is the dominant whiff driver**, with spin and velocity smaller but significant (game-clustered logistic regression, confirmed by SHAP).
- A LightGBM pitch-outcome model reaches an out-of-time **log loss of 0.99 vs. 1.54** for a league-prior baseline on 2026 pitches it never saw. Whiff-on-swing AUC is 0.71.
- A fastball-only plate appearance ends in a **strikeout 22.1% / walk 7.3%** of the time. 0-2 is the best strikeout count (45%) and 3-0 the most walk-prone (57%).
- Velocity fades about **0.13 mph per 10 fastballs** within an outing (mixed-effects model), and whiff odds fall about **10% per extra trip through the order**.
- In the simulator, an extra mph is worth about **0.06 runs per nine innings** through the strike-zone channel alone.

**Honest negative results (these are reported in the notebooks, not hidden)**

- A shape-only "Stuff+" score has weak validity (leave-pitcher-out AUC 0.547). An earlier version looked impressive only because it was memorizing pitcher identity. That leak was found and fixed.
- Empirical-Bayes shrinkage beat raw prior-season whiff rates in a rolling backtest (RMSE 0.043 vs. 0.053) but did **not** beat simply using the league mean (0.042). True pitcher differences in whiff rate are hard to separate from noise at these sample sizes.

---

## What's in the repository

| File | Cells | Run time* | What it is |
|---|---|---|---|
| `Cubs_Analytics.ipynb` | 87 | ~6 min | Main analysis: EDA, static baseball visuals, statistical models, Bayesian models, machine learning, Monte Carlo simulation |
| `Cubs_Interactive_Visuals.ipynb` | 34 | seconds | 15 interactive Plotly visualizations (game replay, win-probability explorer, Savant-style cards, animated pitch flight, and more) |
| `Cubs_Bayesian_Hierarchical.ipynb` | 36 | ~7.5 min | Deep dive: four hierarchical Bayesian models, model comparison, out-of-time validation, posterior-predictive matchup simulator |
| `cubs_outputs/`, `cubs_outputs_viz/`, `cubs_outputs_bayes/` | n/a | n/a | Standalone HTML versions of every interactive figure, plus result tables as CSV |

\*Measured on a single CPU core. Multi-core machines are faster because MCMC chains run in parallel.

Each notebook is self-contained and ships with its outputs already rendered, so you can read the results without running anything.

---

## About the data (read this first)

**Source:** a Statcast pitch-by-pitch export, `Cubs.csv`.

| Property | Value |
|---|---|
| Rows | 25,000 pitches |
| Columns | 119 raw (about 145 after feature engineering) |
| Games | 472 regular-season games (2023: 62, 2024: 162, 2025: 162, 2026: 86) |
| Date range | 2023-07-26 to 2026-06-30 |
| Cubs pitchers | 71 |
| Opposing batters | 633 |

**Scope.** Every row is a **4-seam fastball thrown by a Cubs pitcher** (opponent batting). The file contains **no other pitch types and no Cubs hitting**. This is therefore a *Cubs fastball and pitching* study. Every model and conclusion is labeled with that scope, and the simulator is explicitly a "fastball-only world".

**Statcast quirks handled in the code**

- `player_name` is the **batter**, not the pitcher. Pitchers are identified by MLBAM ID and mapped to names with `pybaseball` (cached after the first lookup).
- Movement and location are re-expressed so **positive = arm-side** break and **positive = inside** location for either handedness (verified against hit-by-pitch locations and known pitcher profiles).
- `delta_pitcher_run_exp` is exactly `-delta_run_exp`. Run values are reported from the **pitcher's perspective** (positive = good for the pitcher).
- Batted-ball fields exist only when the ball is put in play, and bat-tracking fields cover only about 47% of rows. Fully-empty deprecated columns are dropped.

---

## Notebook 1: Pitching Analytics Lab

`Cubs_Analytics.ipynb`

| Section | Contents |
|---|---|
| **1. Setup and data engineering** | Cleaning, pitcher-name lookup, outcome mapping (ball / called strike / swinging strike / foul / in play), base-out state, run values |
| **2. EDA** | Data-quality profile, season and pitcher summary tables, velocity trends, outcome mix by count |
| **3. Baseball visuals (static)** | Strike-zone surfaces (whiff, called strike, swing), spray chart on a ballpark diagram, movement profiles, release points, exit-velocity vs. launch-angle chart, reusable `pitcher_card(id)` |
| **4. Interactive visuals** | Zone explorer, spray chart, animated 3-D pitch trajectories, pitcher dashboard, velocity tracker, sunburst, opponent explorer |
| **5. Statistical models** | Run values by count; **absorbing Markov chain** on the ball-strike count with a 200K-PA Monte Carlo cross-check; logistic regression with game-clustered errors; spline-logistic umpire-zone model; hypothesis tests with effect sizes; cluster bootstrap; mixed-effects fatigue model |
| **6. Bayesian models (PyMC)** | Hierarchical whiff model, hierarchical logistic regression, Beta-Binomial season comparison, change-point model for velocity |
| **7. Machine learning** | LightGBM pitch-outcome classifier with calibration and SHAP; whiff-on-swing model; shape-only Stuff+ score; KMeans fastball archetypes; velocity counterfactuals |
| **8. Monte Carlo simulation** | Pitch to plate-appearance to inning to game simulator; exact and Monte Carlo base-out engines; RE24 matrix; uncertainty-aware pitcher ranking; velocity scenarios |
| **9. Findings, limitations, exports** | Auto-generated summary (every number computed from the models), CSV and HTML exports |

**Design highlights**

- **Out-of-time validation:** train on 2023-2025, test on 2026. Early stopping uses the *latest* slice of training data. Pitcher identity is deliberately **not** a model feature, so models generalize to new arms.
- **Two independent run engines:** an exact 24-state base-out Markov solution and a vectorized Monte Carlo inning simulator. They agree within 0.018 runs across all 24 states, which validates both.
- **Count-mix-adjusted pitcher inputs:** pitcher rates are shrunk with a Dirichlet empirical-Bayes prior whose strength is estimated from the data, and adjusted for the counts each pitcher actually pitched in.
- **Backtest of shrinkage:** rolling-origin evaluation across three test seasons (see the honest negative result above).

---

## Notebook 2: Interactive Visualization Studio

`Cubs_Interactive_Visuals.ipynb`

Everything here is meant to be clicked, dragged, hovered, filtered and played.

| # | Visualization | Interaction |
|---|---|---|
| 1 | **Game replay** | Play button steps through a game pitch by pitch. The zone map, play-by-play title and Cubs win probability update together. `game_replay(game_pk)` replays any game |
| 2 | **Win-probability explorer** | Dropdown of the most dramatic games. A star marks the biggest swing pitch with the play description |
| 3 | **Zone-grid panels** | Pitcher dropdown. Eight heat-grids: frequency, swing %, whiff %, run value, split by batter handedness |
| 4 | **Savant-style percentile cards** | Pitcher dropdown. Percentile bars versus qualified Cubs pitchers (red = good) |
| 5 | **Radar comparison** | Click legend names to add or remove pitchers |
| 6 | **Pitch-cloud flight** | Animates about 160 fastballs from release to plate and marks the hitter's decision point (~0.17 s before arrival) |
| 7 | **Exit-velocity slider spray chart** | Drag to keep only balls hit at least X mph |
| 8 | **EV x launch-angle outcome map** | Dropdown: hit rate / HR rate / xwOBA / sample size, with the barrel zone outlined |
| 9 | **Season-to-date race** | Animated bubble chart of velocity vs. whiff rate through every month |
| 10 | **Season calendar** | GitHub-style heat calendar per season, with a metric dropdown |
| 11 | **Batter explorer** | Opposing batters by whiff rate and exit velocity, filterable by side |
| 12 | **3-D location landscape** | Rotatable surfaces for whiff, called strike, swing and frequency |
| 13 | **Parallel coordinates and linked scatter-matrix** | Brush any axis to filter pitchers; lasso points in one panel and see them in all |

**Notes**

- Pitch paths are physics-based reconstructions from Statcast velocity and acceleration vectors, calibrated so each pitch lands on its recorded plate location.
- The file only contains fastballs, so the win-probability line moves in steps rather than continuously.
- Every figure is checked programmatically (dropdown visibility masks match trace counts, animation frames reference valid traces) and exported as a standalone HTML file.

---

## Notebook 3: Bayesian Hierarchical Modeling

`Cubs_Bayesian_Hierarchical.ipynb`

### Why hierarchical models first, Monte Carlo second?

A simulator is only as good as the parameters fed into it. Raw averages on small, lopsided samples are mostly noise, and simulating them reproduces noise with false precision. So the notebook estimates true rates with hierarchical models (partial pooling plus honest uncertainty) and **then** simulates from the posterior, so every simulated outcome inherits the full parameter uncertainty. (MCMC is itself a Monte Carlo method, so the two are complementary, not rivals.)

### The models

| Model | Outcome | Structure |
|---|---|---|
| **A** whiff on swing | Bernoulli | A ladder of four models fitted on 2023-2025: **A0** pooled, **A1** + pitcher, **A2** + pitcher + batter, **A3** + pitcher + batter + pitcher-season. Pitchers, batters and seasons are *crossed* |
| **B** swing decision | Bernoulli | Batter-level **correlated varying intercept and slope** (chase vs. selectivity) with an LKJ prior on the correlation, plus a pitcher intercept |
| **C** exit velocity | Student-t | Crossed pitcher and batter effects, in mph |
| **D** velocity decomposition | Normal (meta-analytic) | Pitcher baseline, pitcher-season shift, in-season trend with pitcher-specific slopes, and game-to-game noise, with known within-game sampling variance |

Model A in equation form (for swing *i* by batter *b* against pitcher *p* in season *s*):

```
logit P(whiff_i) = α + x_i·β + u_p + w_ps + v_b
u_p = σ_p·z_p,   w_ps = σ_ps·z_ps,   v_b = σ_b·z_b,   z ~ N(0, 1)     (non-centered)
```

### Model comparison (A ladder, 2026 held-out swings)

| Model | Log score / swing | Brier | AUC |
|---|---|---|---|
| Constant (training whiff rate) | -0.5078 | 0.1631 | 0.500 |
| A0 pooled | -0.4715 | 0.1501 | 0.682 |
| A1 + pitcher | -0.4688 | 0.1492 | 0.687 |
| **A2 + pitcher + batter** | **-0.4618** | **0.1471** | **0.705** |
| A3 + pitcher + batter + pitcher-season | -0.4622 | 0.1472 | 0.703 |

A2 and A3 are effectively tied out-of-time (within 0.0003 per swing), and PSIS-LOO ranks A3 first. A3 is carried forward because its extra term costs almost nothing and quantifies season-to-season drift (it finds very little). 20% of 2026 test swings come from batters never seen in training, which exercises the hierarchical model's handling of new groups.

### Posterior-predictive matchup simulator

For a pitcher *p* and batter *b*, for each posterior draw:

1. Take about 250 of the pitcher's real fastballs (speeds, locations, counts).
2. P(swing) from Model B, then a Bernoulli draw.
3. P(whiff | swing) from Model A, then a Bernoulli draw.
4. P(fair | contact) from a Beta posterior.
5. Exit velocity for balls in play from Model C's Student-t; flag hard-hit at 95 mph or more.

Unseen pitchers or batters are handled the hierarchical way: their effect is drawn fresh from the population distribution. Outputs include a pitcher x batter heatmap (dropdown for whiffs, hard-hit balls and swing %, with 90% intervals) and full predictive distributions for selected matchups. A batter scouting cube combines the effects from A, B and C into one 3-D view per batter.

---

## Methodology notes and design decisions

| Decision | Why |
|---|---|
| Time-based split (train 2023-25, test 2026) | Random splits leak the future and flatter the model; out-of-time evaluation mirrors real use |
| Pitcher ID is not an ML feature | Prevents memorization and lets models score new pitchers |
| Stuff+ validated **leave-pitcher-out** | Pitch shape is almost a fingerprint of the pitcher; game-grouped folds let the model memorize results (this bug occurred, was caught, and was fixed) |
| Non-centered parameterizations | Clean NUTS sampling for hierarchical models with small groups |
| Weakly-informative priors, checked by prior-predictive simulation | Priors cover the observed spread without allowing absurd values |
| Game-clustered standard errors | Pitches within a game are not independent |
| Student-t likelihood for exit velocity | Robust to weak dribblers and crushed balls |
| Two independent run engines | Agreement between exact Markov and Monte Carlo code is a correctness check |
| Explicit core count for MCMC | PyMC's default core detection fails on 1-CPU machines |

---

## Validation and diagnostics

**MCMC convergence** (all final models): max R-hat of 1.010 or lower on the Model A, B, C and D final fits (1.024 or lower on the quick two-chain ladder), no divergent transitions, bulk effective sample sizes well above recommended minimums.

**Predictive checks**
- Out-of-time calibration (reliability curve) of Model A on 2026 swings.
- 90% predictive-interval coverage of each pitcher's actual 2026 whiff rate (20 pitchers with 20 or more swings): 90% for the pooled and pitcher models, 95% for A3.
- Simulator vs. reality: simulated whiffs per 100 fastballs correlate r = 0.96 with real rates; the 90% simulation interval contained the real rate for all 29 pitchers, so the intervals are conservative if anything. The simulator runs about 1 whiff per 100 high on average because it faces an average batter while real pitchers faced a specific mix of hitters.
- Simulated run prevention agrees with Statcast's own run values (Spearman 0.61).

**Sanity checks built into the notebooks**
- Analytic Markov chain vs. 200K-PA Monte Carlo: outcome probabilities agree to about 0.1%.
- Exact RE24 vs. Monte Carlo RE24: max difference 0.018 runs.
- The simulated expected runs for a fresh inning (about 0.45) run about 9% below a typical MLB benchmark, which the notebook explains (fastball-only diet, simplified baserunning) and warns about.

---

## Limitations

1. **Fastballs only.** No other pitch types and no Cubs hitting, so "pitcher quality" means *fastball* quality. A pitcher who lives on breaking balls will look worse than he is. Batter effects describe how hitters handle the fastball, not their overall ability.
2. **Small samples.** Most relievers have a few hundred fastballs at best. Shrinkage guards against overreaction but cannot create information.
3. **Effects are adjusted associations, not causal claims.** Batter effects depend on which pitchers they faced, and vice versa.
4. **Simulator simplifications.** Fixed, documented baserunning assumptions; no defense, park, weather or in-inning fatigue; pitches treated as independent given their context (no sequencing). Absolute run levels are not MLB-like. Use them for relative comparisons.
5. **The velocity counterfactual is a predictive what-if**, not a causal estimate, and only the whiff and strike-zone channel is modeled (not contact quality).
6. **Staff turnover** confounds season-to-season comparisons. Use the within-pitcher models for anything about change.
7. **Statcast fields** such as `delta_run_exp` and `estimated_woba_using_speedangle` are themselves model outputs.
8. **Weak validity of shape-only Stuff+** (see the negative results above).

---

## Installation and how to run

### Recommended: a clean conda environment

```bash
conda create -n cubs -c conda-forge python=3.12 numpy pandas scipy matplotlib seaborn \
      scikit-learn statsmodels pymc jupyter ipykernel -y
conda activate cubs
pip install plotly lightgbm shap arviz pybaseball nbformat
python -m ipykernel install --user --name cubs --display-name "Python (cubs)"
```

Installing PyMC from conda-forge also brings a C++ compiler on Windows. With a plain `pip install pymc`, sampling can fall back to a much slower mode.

### Run

1. Place `Cubs.csv` in the same folder as the notebooks (or `data/Cubs.csv`; the notebooks check both).
2. Open a notebook and select the **Python (cubs)** kernel.
3. **Run All.**

**Settings you may want to change** (in each notebook's setup cell)

| Setting | Where | Purpose |
|---|---|---|
| `pio.renderers.default` | all notebooks | `"plotly_mimetype+notebook_connected"` for Jupyter and VS Code; `"colab"` for Google Colab; `"browser"` for a plain script |
| `FAST = True` | Notebook 3 | Smoke-test mode with tiny MCMC runs (about 2 minutes). Results are not meaningful |
| `DRAWS` | Notebook 3 | MCMC draws, tuning steps and chains per model |
| `MIN_PITCHES` | Notebooks 1-2 | Minimum fastballs for pitcher leaderboards (default 150) |
| `GAME_PK` | Notebook 2 | Which game to replay |

**First run:** the notebooks look up pitcher names with `pybaseball` and cache them to `cubs_outputs/pitcher_names.csv`. If you are offline, pitchers appear as `Pitcher <id>`.

Notebooks 1 and 3 write every interactive figure to HTML and every result table to CSV in their output folders.

---

## Troubleshooting

**`A module that was compiled using NumPy 1.x cannot be run in NumPy 2.x` / `_ARRAY_API not found`**
Your environment has NumPy 2 alongside packages built for NumPy 1 (common in the base Anaconda install). Either upgrade the compiled packages, `pip install --upgrade pyarrow bottleneck numexpr`, then restart the kernel, or (more reliable) use the clean environment above.

**`ZeroDivisionError` when sampling with PyMC**
Caused by default core detection on 1-CPU machines. The notebooks already pass an explicit core count (`N_CORES`), so this should not occur; if you copy the sampling code elsewhere, pass `cores=1`.

**Interactive figures appear blank**
Wrong Plotly renderer for your environment. See the `pio.renderers.default` setting above.

**Very slow sampling**
Check that PyMC found a C++ compiler (install via conda-forge). On Windows, the conda-forge PyMC package handles this.

**Results differ slightly from the README**
MCMC output can vary by a small amount across machines and core counts even with fixed seeds. Conclusions should be stable; the third decimal place may not be.

---

## Project structure

```
cubs-pitching-analytics/
├── README.md
├── data/
│   └── Cubs.csv                          # Statcast export (see data notice below)
├── notebooks/
│   ├── Cubs_Analytics.ipynb              # EDA, stats, Bayesian, ML, simulation
│   ├── Cubs_Interactive_Visuals.ipynb    # 15 interactive visualizations
│   └── Cubs_Bayesian_Hierarchical.ipynb  # hierarchical models + posterior-predictive simulator
└── outputs/
    ├── cubs_outputs/                     # Notebook 1: HTML figures + CSV tables
    ├── cubs_outputs_viz/                 # Notebook 2: standalone interactive HTML
    └── cubs_outputs_bayes/               # Notebook 3: HTML figures + CSV tables
```

(The notebooks look for `Cubs.csv` next to themselves or in `data/`. If you reorganize into `notebooks/` and `data/`, add `"../data/Cubs.csv"` to `DATA_CANDIDATES` in each setup cell.)

---

## Roadmap

- **All pitch types and Cubs hitting.** Re-pull Statcast with everything; this would make the count chain, sequencing models and a real RE24 possible and removes the "fastball-only" caveat.
- **Pitcher x batter interaction terms** (matchup-specific skill) and **time-varying skill** (random-walk or Gaussian-process pitcher effects).
- **Multinomial hierarchical outcome model** across all pitch types.
- **Catcher and umpire effects** for called strikes; park and defense effects.
- **A hierarchical xwOBA-on-contact model** wired into the simulator so velocity changes also affect contact quality.
- **Full plate-appearance and inning simulator** driven by the Bayesian posterior, with lineup-level matchups and a bullpen-usage optimizer.
- **A Streamlit dashboard** wrapping the interactive figures.

---

## Acknowledgments and data notice

- Pitch-level data from **Statcast / Baseball Savant** (MLB Advanced Media). Player-ID lookups via **pybaseball** and the Chadwick Bureau register.
- Built with **PyMC**, **ArviZ**, **LightGBM**, **SHAP**, **statsmodels**, **scikit-learn**, **pandas** and **Plotly**.
- **Data notice:** MLB and Statcast data are subject to MLB's terms of use. Before publishing this repository publicly, check whether you may redistribute the raw `Cubs.csv`; if not, omit it and document how to re-create it (for example with `pybaseball.statcast_pitcher` filtered to Cubs pitchers and 4-seam fastballs).
- This is an independent analytics project and is not affiliated with or endorsed by the Chicago Cubs or Major League Baseball.

---

**Author:** [Your Name] · [LinkedIn] · [Email] · [Portfolio]
