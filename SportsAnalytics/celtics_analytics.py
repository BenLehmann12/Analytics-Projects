#!/usr/bin/env python
# coding: utf-8

# ## 0 · Setup

# In[13]:


import warnings; warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, pandas as pd
import plotly.express as px, plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.io as pio
from scipy import stats
import statsmodels.api as sm, statsmodels.formula.api as smf
from IPython.display import display, Markdown

import celtics_utils as cu
cu.set_theme()
pio.renderers.default = "notebook_connected"   # interactive in Jupyter *and* in the exported HTML

DATA_DIR = Path("C:/Users/Lehma/AnalyticsProj")                 # <- folder holding the 4 CSVs
FIG_DIR  = Path("outputs/figures");  FIG_DIR.mkdir(parents=True, exist_ok=True)
SEED = 42
pd.set_option("display.max_columns", 60, "display.width", 200, "display.float_format", "{:,.3f}".format)

def show(fig, name):
    """Display interactively AND save a standalone HTML copy in outputs/figures."""
    fig.write_html(FIG_DIR / f"{name}.html", include_plotlyjs="cdn")
    fig.show()

SEASONS = ["2021-22", "2022-23", "2023-24", "2024-25", "2025-26"]
SC = cu.SEASON_COLORS


# ## 1 · Data audit & feature engineering
# `celtics_utils.load_all()` merges the three player files on *(season, player)*, strips the `Team Totals` rows into a team table, and builds
# a **game table** whose features are all known *before tip-off* (so they can be used for prediction without leakage).

# In[14]:


games, players, team = cu.load_all(DATA_DIR)
print(f"games   : {games.shape}  ({games.season.nunique()} seasons x 82)")
print(f"players : {players.shape}  ({players.player.nunique()} unique players, {players.season.nunique()} seasons)")
print(f"team    : {team.shape}")
games.head(8)


# In[15]:


# ---- Data-quality audit ---------------------------------------------------------------------
raw_sh = pd.read_csv(next(DATA_DIR.glob("*shooting*.csv")))
raw_sh["season"] = raw_sh["Season"].str[:4] + "-" + raw_sh["Season"].str[7:9]
file_avg = raw_sh[raw_sh.Player == "Team Totals"].set_index("season")["Avg Dist (ft)"]

audit = team[["season", "W", "L", "PTS"]].set_index("season")
audit["sched_PTS"]       = games.groupby("season").pts_for.sum()
audit["sum_player_PTS"]  = players.groupby("season").PTS.sum()
audit["PTS match?"]      = (audit.PTS == audit.sched_PTS) & (audit.PTS == audit.sum_player_PTS)
audit["games/season"]    = games.groupby("season").size()
audit["file_team_avg_dist"] = file_avg
audit["FGA-weighted_avg_dist"] = team.set_index("season")["avg_shot_dist"]
display(audit)

# per-game x games vs total (rounding check)
m = players[players.MP >= 500].assign(chk=lambda d: (d.PTS_pg * d.G - d.PTS).abs() / d.PTS)
print(f"\nMax relative gap between PTS/G x G and total PTS (players >=500 min): {m.chk.max():.2%}  (per-game averages are rounded to 1 dp)")
gap = (audit["file_team_avg_dist"] - audit["FGA-weighted_avg_dist"]).abs()
print("Seasons where the file's team Avg Dist differs from the FGA-weighted player average by > 0.5 ft:", list(gap[gap > 0.5].index))


# **Audit result.** Points reconcile three ways (schedule ↔ team totals ↔ sum of players), every season has 82 games, and per-game × G matches totals within rounding.
# Two quirks worth knowing:
# * In the shooting file, the `Team Totals` row's *Avg Dist* disagrees with the FGA-weighted average of the player rows in **4 of 5 seasons** (e.g. 2025-26: 14.7 ft in the file vs ~16.2 ft from players; only 2024-25 agrees). The player-derived value is the one consistent with the team's 3-point rate, so I **recompute team shot profiles from player rows** (`avg_shot_dist`) and ignore the file's team row for that column.
# * Zone attempts (`fga_0-3`, …) are reconstructed as `FGA × %FGA(zone)` after renormalising the rounded percentages — accurate to ±1 shot.

# In[16]:


display(Markdown("### Engineered game-level features (all pre-game)"))
feat = pd.DataFrame({
  "feature": ["home","days_rest","b2b","three_in_four","run_len","form5 / form10","win_pct_to_date","prev_win","matinee","start_hour","weekend","half","close / blowout"],
  "meaning": ["1 = home game","calendar days since previous game (0 = back-to-back)","second night of a back-to-back",
              "3rd game in 4 nights","game # in the current home-stand / road-trip","avg margin of previous 5 / 10 games (missing games count as 0 => shrinks early-season)",
              "win % before this game","won previous game","tip-off before 5pm ET","tip-off time (decimal hour)","Fri/Sat/Sun","first / last 41 games","|margin| <= 5  /  >= 15"]})
display(feat)
display(Markdown("### Team season scoreboard"))
display(team[["season","W","L","win_pct","mov","ORtg","DRtg","NetRtg","pace","eFG%","TS%","3PAr","3P%","FTr","TOV%","pyth_W","luck_W"]].round(3))


# ## 2 · Interactive EDA

# ### 2.1 Season races — cumulative wins above .500
# Hover for opponent, score and running record. Click legend entries to isolate seasons.

# In[20]:


fig = go.Figure()
for s, d in games.groupby("season"):
    cd = np.stack([d.opponent, np.where(d.home == 1, "vs", "@"), d.pts_for, d.pts_against, d.cum_wins, d.game_no - d.cum_wins], axis=1)
    fig.add_trace(go.Scatter(x=d.game_no, y=d.wins_vs_500, mode="lines", name=f"{s}  ({d.win.sum()}-{82 - d.win.sum()})",
                  line=dict(color=SC[s], width=4 if s == "2023-24" else 2.2, shape="hv"), customdata=cd,
                  hovertemplate="<b>%{customdata[1]} %{customdata[0]}</b><br>%{customdata[2]}-%{customdata[3]}<br>Record: %{customdata[4]}-%{customdata[5]}<extra>" + s + "</extra>"))
fig.add_hline(y=0, line_dash="dot", line_color="grey")
fig.update_layout(title="Wins above .500 through the season", xaxis_title="Game #", yaxis_title="Wins − Losses over .500 (wins − G/2)", height=480, hovermode="closest")
fig.show(renderer="notebook")


# ### 2.2 Rolling form — 10-game average scoring margin

# In[21]:


games["mov10"] = games.groupby("season").margin.transform(lambda m: m.rolling(10, min_periods=10).mean())
fig = px.line(games.dropna(subset=["mov10"]), x="game_no", y="mov10", color="season", color_discrete_map=SC,
              hover_data={"opponent": True, "margin": True, "game_no": True, "season": False, "mov10": ":.1f"},
              labels={"mov10": "10-game avg margin", "game_no": "Game #"}, title="Rolling 10-game margin of victory")
fig.add_hline(y=0, line_dash="dot", line_color="grey"); fig.update_traces(line_width=2.2); fig.update_layout(height=450)
fig.show(renderer="notebook")


# ### 2.3 Margin distributions — by season and home/away
# The box lines show median & quartiles; the violin shows the full shape.

# In[22]:


games["venue"] = np.where(games.home == 1, "Home", "Away")
fig = px.violin(games, x="season", y="margin", color="venue", box=True, points=False, violinmode="group",
                color_discrete_map={"Home": cu.GREEN, "Away": cu.GOLD}, category_orders={"season": SEASONS},
                title="Scoring margin by season & venue", labels={"margin": "Margin (Celtics − Opp)"})
fig.add_hline(y=0, line_dash="dot", line_color="grey"); fig.update_layout(height=480)
show(fig, "03_margin_violin")

# Is the margin distribution ~Normal?  (matters for the win-probability model & t-tests)
x = games.margin
sh = stats.shapiro(x); sk, ku = stats.skew(x), stats.kurtosis(x)
xs = np.linspace(x.min() - 5, x.max() + 5, 300)
fig = go.Figure()
fig.add_histogram(x=x, histnorm="probability density", nbinsx=40, marker_color=cu.GREEN, opacity=.7, name="Observed margins")
fig.add_scatter(x=xs, y=stats.norm.pdf(xs, x.mean(), x.std()), name=f"Normal(μ={x.mean():.1f}, σ={x.std():.1f})", line=dict(color=cu.GOLD, width=3))
fig.update_layout(title=f"Margin distribution (410 games) — skew {sk:.2f}, excess kurtosis {ku:.2f}, Shapiro-Wilk p = {sh.pvalue:.3f}",
                  xaxis_title="Margin", yaxis_title="Density", height=420, bargap=.03)
fig.show(renderer="notebook")


# ### 2.4 Season 'barcode' — every game of five seasons in one picture
# Green = Celtics win (darker = bigger blowout), red = loss. Hover for opponent and score.

# In[23]:


Z = np.full((5, 82), np.nan); T = np.empty((5, 82), dtype=object)
for i, s in enumerate(SEASONS):
    d = games[games.season == s]
    Z[i, :len(d)] = d.margin.values
    T[i, :len(d)] = [f"{'vs' if h else '@'} {o}<br>{a}-{b}{' (' + ot + ')' if isinstance(ot, str) else ''}" for h, o, a, b, ot in zip(d.home, d.opponent, d.pts_for, d.pts_against, d.ot_label)]
fig = go.Figure(go.Heatmap(z=Z, x=np.arange(1, 83), y=SEASONS, text=T, hovertemplate="Game %{x}<br>%{text}<br>Margin %{z:+d}<extra></extra>",
                           colorscale="RdYlGn", zmid=0, zmin=-30, zmax=30, xgap=1, ygap=3, colorbar=dict(title="Margin")))
fig.update_layout(title="Every regular-season game, 2021-22 → 2025-26", xaxis_title="Game #", height=360, yaxis=dict(autorange="reversed"))
fig.show(renderer="notebook")


# ### 2.5 Who did the Celtics beat up on? Average margin vs each opponent × season
# Cell text = W-L record. Sorted by 5-year average margin.

# In[24]:


pv = games.pivot_table(index="opponent", columns="season", values="margin", aggfunc="mean")[SEASONS]
rec = games.pivot_table(index="opponent", columns="season", values="win", aggfunc=lambda s: f"{int(s.sum())}-{int(len(s) - s.sum())}")[SEASONS]
order = games.groupby("opponent").margin.mean().sort_values().index
fig = go.Figure(go.Heatmap(z=pv.loc[order].values, x=SEASONS, y=order, text=rec.loc[order].values, texttemplate="%{text}",
                           colorscale="RdYlGn", zmid=0, hovertemplate="%{y} · %{x}<br>Avg margin %{z:+.1f}<br>Record %{text}<extra></extra>"))
fig.update_layout(title="Average margin vs opponent (labels = W-L)", height=780, xaxis=dict(side="top"))
fig.show(renderer="notebook")


# ### 2.6 How the team's identity changed — efficiency & style
# `ORtg/DRtg` are possession-based **proxies** (DRtg assumes equal possessions for both teams since opponent box-score data isn't in the files).

# In[25]:


specs = [("ORtg","Offensive rating (pts/100 poss)"),("DRtg","Defensive rating proxy (lower = better)"),("NetRtg","Net rating proxy"),
         ("eFG%","Effective FG%"),("3PAr","3PA / FGA"),("pace","Pace (possessions / game)")]
fig = make_subplots(rows=2, cols=3, subplot_titles=[s[1] for s in specs], vertical_spacing=.18)
for k, (c, _) in enumerate(specs):
    r, cc = divmod(k, 3)
    fig.add_trace(go.Scatter(x=team.season, y=team[c], mode="lines+markers+text", text=[f"{v:.3f}" if v < 1 else f"{v:.1f}" for v in team[c]],
                  textposition="top center", line=dict(color=cu.GREEN, width=3), marker=dict(size=10, color=[SC[s] for s in team.season], line=dict(color="white", width=1)),
                  showlegend=False, hovertemplate="%{x}<br>" + c + " = %{y:.3f}<extra></extra>"), row=r + 1, col=cc + 1)
fig.update_layout(height=600, title="Team evolution (2023-24 title team = peak on both ends)")
fig.show(renderer="notebook")


# ### 2.7 Player explorer — shooting volume vs. accuracy through the years (press ▶)

# In[26]:


rot = players[(players.MP >= 500)].dropna(subset=["3P%"]).copy()
rot["3PA/G"] = rot["3PA_pg"]; rot["3P%_pct"] = rot["3P%"] * 100
fig = px.scatter(rot.sort_values("season"), x="3PA/G", y="3P%_pct", size="MPG", color="role", animation_frame="season", hover_name="player",
                 color_discrete_map={"Starter": cu.GREEN, "Bench": cu.GOLD}, size_max=34, text="player",
                 hover_data={"MPG": ":.1f", "PTS_pg": ":.1f", "TS%": ":.3f", "3PA/G": ":.1f", "3P%_pct": ":.1f", "role": False},
                 range_x=[-.3, 10.8], range_y=[15, 56], labels={"3P%_pct": "3P%"}, title="3-point volume vs accuracy (bubble = minutes per game; ≥500 MP)")
fig.update_traces(textposition="top center", textfont_size=9); fig.update_layout(height=620)
fig.add_hline(y=36.5, line_dash="dot", line_color="grey", annotation_text="≈ league-average 3P%")
fig.show(renderer="notebook")


# ### 2.8 Usage vs. efficiency

# In[27]:


rot = players[players.MP >= 800]
fig = px.scatter(rot, x="USG%", y="TS%", color="season", size="MP", color_discrete_map=SC, hover_name="player", category_orders={"season": SEASONS},
                 hover_data={"MPG": ":.1f", "PTS_pg": ":.1f", "GmSc": ":.1f", "USG%": ":.1f", "TS%": ":.3f", "MP": False},
                 title="Usage vs True Shooting (≥800 min) — who carries the load and how efficiently?", size_max=26)
fig.add_hline(y=rot["TS%"].median(), line_dash="dot", line_color="grey"); fig.add_vline(x=rot["USG%"].median(), line_dash="dot", line_color="grey")
fig.show(renderer="notebook")


# ### 2.9 Where did the points come from? Season → role → player (click to drill in)

# In[28]:


fig = px.sunburst(players[players.PTS > 0], path=["season", "role", "player"], values="PTS", color="season", color_discrete_map={**SC, "(?)": "#ddd"},
                  title="Points scored — click a season/role to zoom", maxdepth=3)
fig.update_traces(hovertemplate="<b>%{label}</b><br>%{value} pts (%{percentParent:.1%} of parent)<extra></extra>"); fig.update_layout(height=650)
fig.show(renderer="notebook")


# ### 2.10 Minutes distribution — star-heavy or deep?

# In[29]:


pl = players.copy(); pl["rank"] = pl.groupby("season").MP.rank(ascending=False, method="first")
pl["label"] = np.where(pl["rank"] <= 8, pl.player, "Rest of roster")
agg = pl.groupby(["season", "label"], as_index=False).MP.sum(); agg["share"] = agg.MP / agg.groupby("season").MP.transform("sum")
hhi = players.groupby("season").apply(lambda d: ((d.MP / d.MP.sum()) ** 2).sum(), include_groups=False)
fig = px.bar(agg, x="season", y="share", color="label", title="Share of team minutes (top-8 by minutes each year)", category_orders={"season": SEASONS},
             hover_data={"MP": True, "share": ":.1%"}, color_discrete_sequence=px.colors.qualitative.Safe + px.colors.qualitative.Pastel)
fig.update_layout(height=520, yaxis_tickformat=".0%", barmode="stack")
for s in SEASONS: fig.add_annotation(x=s, y=1.03, text=f"HHI {hhi[s]:.3f}", showarrow=False, font=dict(size=10))
fig.show(renderer="notebook")


# ### 2.11 Radar — rotation profiles, 2025-26 (percentile among all 104 player-seasons; click legend names to add/remove)

# In[30]:


metrics = {"PTS_36": "Scoring/36", "TRB_36": "Rebounding/36", "AST_36": "Playmaking/36", "STL_36": "Steals/36", "BLK_36": "Blocks/36",
           "TS%": "True shooting", "3PAr": "3-pt rate", "USG%": "Usage"}
base = players[players.MP >= 300]
pct = base[list(metrics)].rank(pct=True) * 100
cur = base[(base.season == "2025-26")].assign(**{k: pct.loc[base.season == "2025-26", k] for k in metrics}).sort_values("MP", ascending=False).head(9)
fig = go.Figure()
for i, (_, r) in enumerate(cur.iterrows()):
    fig.add_trace(go.Scatterpolar(r=[r[k] for k in metrics] + [r[list(metrics)[0]]], theta=list(metrics.values()) + [list(metrics.values())[0]],
                                  name=r.player, fill="toself", opacity=.55, visible=True if i < 3 else "legendonly"))
fig.update_layout(polar=dict(radialaxis=dict(range=[0, 100], tickvals=[25, 50, 75, 100])), height=560, title="Player percentile profiles (2025-26)")
fig.show(renderer="notebook")


# ### 2.12 What goes with what? Correlation of rotation-player metrics

# In[31]:


cols = ["PTS_36", "TRB_36", "AST_36", "STL_36", "BLK_36", "TOV_36", "TS%", "3PAr", "FTr", "USG%", "Avg Dist (ft)", "%FGA 0-3", "GmSc", "age"]
cm = players[players.MP >= 500][cols].corr(method="spearman")
fig = px.imshow(cm, text_auto=".2f", color_continuous_scale="RdBu_r", zmin=-1, zmax=1, aspect="auto", title="Spearman correlations (player-seasons ≥500 min)")
fig.update_layout(height=640)
fig.show(renderer="notebook")


# ## 3 · Basketball visuals
# ### 3.1 Interactive half-court shot-zone map
# Pick any **player-season** (≥600 min) or the **whole team** from the dropdown. Zones follow Basketball-Reference's distance bands from the rim
# (0-3, 3-10, 10-16, 16-ft-to-the-arc, 3-pointers; corner threes split out for individuals). **Colour = zone FG% minus the Celtics' team FG% in that zone & season**
# — so green means "better than his teammates from there", red "worse". Labels give FG% and the share of the player's shots. Hover any zone for details.

# In[34]:


court = cu.shot_zone_figure(players, min_minutes=600)
court.show()


# ### 3.2 Shot-profile evolution — *where* the Celtics shoot and *how well*

# In[35]:


rows = []
for _, r in team.iterrows():
    for z in cu.ZONES:
        rows.append(dict(season=r.season, zone_key=z, zone=cu.ZONE_LABELS[z], share=r[f"share_{z}"], fg=r[f"fgpct_{z}"],
                         pps=r[f"fgpct_{z}"] * (3 if z == "3P" else 2), fga=r[f"share_{z}"] * r["FGA"]))
zdf = pd.DataFrame(rows)

fig = make_subplots(rows=1, cols=3, subplot_titles=("Share of all FGA", "FG% by zone", "Points per shot by zone"), horizontal_spacing=.09)
zcol = dict(zip(cu.ZONES, ["#E8590C", "#BA9653", "#8A8D8F", "#4FB477", "#007A33"]))
for z in cu.ZONES:
    d = zdf[zdf.zone_key == z]
    fig.add_bar(x=d.season, y=d.share, name=cu.ZONE_LABELS[z], marker_color=zcol[z], legendgroup=z, text=[f"{v:.0%}" for v in d.share], textposition="inside",
                hovertemplate="%{x}<br>" + cu.ZONE_LABELS[z] + "<br>%{y:.1%} of FGA<extra></extra>", row=1, col=1)
    fig.add_scatter(x=d.season, y=d.fg, mode="lines+markers", name=cu.ZONE_LABELS[z], marker_color=zcol[z], legendgroup=z, showlegend=False,
                    hovertemplate="%{x}<br>FG%% %{y:.1%}<extra></extra>", row=1, col=2)
    fig.add_scatter(x=d.season, y=d.pps, mode="lines+markers", name=cu.ZONE_LABELS[z], marker_color=zcol[z], legendgroup=z, showlegend=False,
                    hovertemplate="%{x}<br>%{y:.2f} pts/shot<extra></extra>", row=1, col=3)
fig.update_layout(barmode="stack", height=480, title="Celtics shot profile 2021-22 → 2025-26 (aggregated from player rows)", legend=dict(orientation="h", y=-.15))
fig.update_yaxes(tickformat=".0%", row=1, col=1); fig.update_yaxes(tickformat=".0%", row=1, col=2)
show(fig, "15_shot_profile_evolution")
display(zdf.pivot(index="zone", columns="season", values="share").loc[[cu.ZONE_LABELS[z] for z in cu.ZONES]].round(3))


# ### 3.3 Shot-distance vs. efficiency — who lives where on the floor?

# In[36]:


d = players[(players.MP >= 500)].dropna(subset=["Avg Dist (ft)"])
fig = px.scatter(d, x="Avg Dist (ft)", y="eFG%", size="FGA", color="season", color_discrete_map=SC, hover_name="player", category_orders={"season": SEASONS},
                 hover_data={"%FGA 0-3": ":.1%", "%FGA 3P": ":.1%", "TS%": ":.3f", "FGA": True, "Avg Dist (ft)": ":.1f", "eFG%": ":.3f"}, size_max=28,
                 title="Average shot distance vs effective FG% (bubble = FGA, ≥500 min)")
fig.update_layout(height=560); show(fig, "16_distance_efg")


# ## 4 · Hypothesis testing & A/B testing
# 
# **Framework used throughout**
# 
# * Unit of analysis: the **game** (n = 410) for schedule questions; the **team-season / player-season** for roster questions.
# * α = 0.05 (two-sided). Every test is run with ≥ 2 methods when feasible (parametric **and** non-parametric / permutation) so conclusions don't hinge on one assumption.
# * Report **effect size with CI**; a p-value alone says nothing about *how much*.
# * Because I run ~14 tests, I apply **Holm** (family-wise error) and **Benjamini-Hochberg** (false discovery rate) corrections in §4.15.
# * *"A/B" here is observational*: schedule factors (home, rest, tip-off time) aren't randomised, so we estimate **associations**; causal wording is avoided.

# In[37]:


log = cu.TestLog()
SD_MARGIN = games.margin.std()

def compare_continuous(a, b, la, lb, id_, question, outcome="margin", n_perm=10_000):
    a, b = np.asarray(a, float), np.asarray(b, float)
    _, p_w = stats.ttest_ind(a, b, equal_var=False)
    _, p_u = stats.mannwhitneyu(a, b, alternative="two-sided")
    _, p_pm = cu.permutation_test(a, b, n_perm=n_perm)
    d = cu.cohens_d(a, b); ci = cu.bootstrap_ci(a, b, n_boot=5000)
    log.add(id_, question, "Welch t-test", p_w, effect=a.mean() - b.mean(), effect_name=f"Δ {outcome}", ci=ci, n=f"{len(a)} vs {len(b)}")
    return pd.DataFrame({"group": [la, lb, "difference (A−B)"], "n": [len(a), len(b), ""], f"mean {outcome}": [a.mean(), b.mean(), a.mean() - b.mean()],
                         "95% bootstrap CI": ["", "", f"[{ci[0]:+.2f}, {ci[1]:+.2f}]"], "Cohen's d": ["", "", f"{d:+.2f}"],
                         "p Welch": ["", "", f"{p_w:.4f}"], "p Mann-Whitney": ["", "", f"{p_u:.4f}"], "p permutation": ["", "", f"{p_pm:.4f}"]})

def compare_props(x1, n1, x2, n2, la, lb, id_, question, log_it=True):
    z, p, diff, ci = cu.two_prop_ztest(x1, n1, x2, n2)
    _, p_f = stats.fisher_exact([[x1, n1 - x1], [x2, n2 - x2]])
    h = cu.cohens_h(x1 / n1, x2 / n2)
    if log_it: log.add(id_, question, "two-proportion z", p, effect=diff, effect_name="Δ win%", ci=ci, n=f"{n1} vs {n2}")
    w1, w2 = cu.wilson(x1, n1), cu.wilson(x2, n2)
    return pd.DataFrame({"group": [la, lb, "difference (A−B)"], "n": [n1, n2, ""], "wins": [x1, x2, ""], "win %": [x1 / n1, x2 / n2, diff],
                         "95% CI": [f"[{w1[0]:.3f}, {w1[1]:.3f}]", f"[{w2[0]:.3f}, {w2[1]:.3f}]", f"[{ci[0]:+.3f}, {ci[1]:+.3f}]"],
                         "Cohen's h": ["", "", f"{h:+.2f}"], "p z-test": ["", "", f"{p:.4f}"], "p Fisher exact": ["", "", f"{p_f:.4f}"]})


# ### 4.1 T1 — Is there a home-court advantage in scoring margin?
# **H₀:** mean margin at home = mean margin on the road. **H₁:** they differ. &nbsp; Outcome: continuous (margin) ⇒ Welch *t* (unequal variances), Mann-Whitney (no normality needed), permutation test (distribution-free), bootstrap CI.

# In[38]:


H, A = games[games.home == 1].margin, games[games.home == 0].margin
display(compare_continuous(H, A, "Home", "Away", "T1", "Home margin ≠ road margin?"))
print("Levene (equal variance) p =", round(stats.levene(H, A).pvalue, 3), "| Shapiro p (home, away) =", round(stats.shapiro(H).pvalue, 3), round(stats.shapiro(A).pvalue, 3))
fig = go.Figure()
for lab, x, c in [("Home", H, cu.GREEN), ("Away", A, cu.GOLD)]:
    fig.add_histogram(x=x, name=lab, opacity=.6, marker_color=c, nbinsx=30, histnorm="probability density")
    fig.add_vline(x=x.mean(), line_color=c, line_width=3, annotation_text=f"{lab} mean {x.mean():+.1f}")
fig.update_layout(barmode="overlay", title="T1 · Margin distribution, home vs away", xaxis_title="Margin", height=400)
show(fig, "17_T1_home_away_margin")


# ### 4.2 T2 — Is the *win rate* higher at home?
# **H₀:** P(win | home) = P(win | away). Binary outcome ⇒ pooled two-proportion *z*, Fisher's exact; Wilson CIs; Cohen's *h* effect size.

# In[39]:


xh, nh = int(games[games.home == 1].win.sum()), int((games.home == 1).sum())
xa, na = int(games[games.home == 0].win.sum()), int((games.home == 0).sum())
display(compare_props(xh, nh, xa, na, "Home", "Away", "T2", "Home win% ≠ road win%?"))
chi = stats.chi2_contingency([[xh, nh - xh], [xa, na - xa]], correction=False)
print(f"Chi-square test: χ²={chi.statistic:.2f}, p={chi.pvalue:.4f}")

# by season (is the home edge stable?)
bs = games.groupby(["season", "venue"]).win.agg(["sum", "count"]).reset_index()
bs["pct"] = bs["sum"] / bs["count"]; lo_hi = [cu.wilson(s, c) for s, c in zip(bs["sum"], bs["count"])]
bs["lo"], bs["hi"] = [l for l, _ in lo_hi], [h for _, h in lo_hi]
fig = px.bar(bs, x="season", y="pct", color="venue", barmode="group", color_discrete_map={"Home": cu.GREEN, "Away": cu.GOLD}, category_orders={"season": SEASONS},
             error_y=bs.hi - bs.pct, error_y_minus=bs.pct - bs.lo, text=bs["sum"].astype(str) + "/" + bs["count"].astype(str), title="T2 · Win % by venue & season (Wilson 95% CI)")
fig.update_layout(yaxis_tickformat=".0%", height=420); show(fig, "18_T2_home_away_win")


# ### 4.3 T3 — Back-to-backs: does the second night hurt?
# **A** = second night of a back-to-back (n = 68), **B** = ≥ 1 day of rest. Both margin and win rate.

# In[40]:


B2B, REST = games[games.b2b == 1], games[games.b2b == 0]
print("T3a  margin:"); display(compare_continuous(B2B.margin, REST.margin, "Back-to-back", "Rested", "T3", "B2B margin ≠ rested margin?"))
print("T3b  win rate (not logged separately to avoid double-counting the same question):")
display(compare_props(int(B2B.win.sum()), len(B2B), int(REST.win.sum()), len(REST), "Back-to-back", "Rested", "T3b", "", log_it=False))
print(f"Minimum detectable margin gap (80% power, α=.05) with these group sizes: ±{cu.mde_two_sample(len(B2B), len(REST), SD_MARGIN):.1f} pts")


# ### 4.4 T4 — Dose-response: does rest length matter? (0, 1, 2, 3+ days)
# One-way ANOVA + Kruskal-Wallis on margin; χ² on win/loss counts.

# In[41]:


order = ["0 (B2B)", "1", "2", "3+"]
groups = [games[games.rest_cat == c].margin for c in order]
f, p_f = stats.f_oneway(*groups); h, p_kw = stats.kruskal(*groups)
tab = pd.crosstab(games.rest_cat, games.win).loc[order]; chi = stats.chi2_contingency(tab)
log.add("T4", "Margin differs across rest categories?", "Kruskal-Wallis", p_kw, effect=np.nan, effect_name="", n=str(len(games)))
display(games.groupby("rest_cat").agg(n=("win", "size"), win_pct=("win", "mean"), mean_margin=("margin", "mean"), sd=("margin", "std")).loc[order])
print(f"ANOVA F={f:.2f} p={p_f:.3f} | Kruskal-Wallis H={h:.2f} p={p_kw:.3f} | χ² (win × rest) = {chi.statistic:.2f} p={chi.pvalue:.3f}")


# ### 4.5 T5 — Were the five seasons really different? (ANOVA / Kruskal / Tukey HSD / χ²)

# In[42]:


from statsmodels.stats.multicomp import pairwise_tukeyhsd
gs = [games[games.season == s].margin for s in SEASONS]
f, p_f = stats.f_oneway(*gs); h, p_kw = stats.kruskal(*gs); lev = stats.levene(*gs)
eta2 = (f * 4) / (f * 4 + 405)                     # eta-squared from F:  df_b=4, df_w=405
tab = pd.crosstab(games.season, games.win); chi = stats.chi2_contingency(tab)
log.add("T5", "Mean margin differs across seasons?", "One-way ANOVA", p_f, effect=eta2, effect_name="η²", n="410")
log.add("T5b", "Win% differs across seasons?", "χ² homogeneity", chi.pvalue, effect=np.sqrt(chi.statistic / 410), effect_name="Cramér's V", n="410")
print(f"ANOVA F={f:.2f}, p={p_f:.4f}, η²={eta2:.3f} | Kruskal H={h:.1f}, p={p_kw:.4f} | Levene p={lev.pvalue:.3f} | χ² win%×season={chi.statistic:.2f}, p={chi.pvalue:.3f}")
tk = pairwise_tukeyhsd(games.margin, games.season, alpha=.05)
display(pd.DataFrame(tk._results_table.data[1:], columns=tk._results_table.data[0]))
m = games.groupby("season").margin.agg(["mean", "sem"]).loc[SEASONS]
fig = go.Figure(go.Scatter(x=m.index, y=m["mean"], mode="markers", marker=dict(size=14, color=[SC[s] for s in m.index]),
                           error_y=dict(type="data", array=1.96 * m["sem"], thickness=2, width=8)))
fig.add_hline(y=0, line_dash="dot"); fig.update_layout(title="T5 · Mean margin by season (95% CI)", yaxis_title="Mean margin", height=400)
show(fig, "19_T5_season_means")


# ### 4.6 T6 & T7 — "Clutch" tests: overtime games and one-possession-ish games
# Overtime/close games are close to coin flips for *any* team. **H₀: P(win) = 0.5** (exact binomial). Rejecting would suggest systematic late-game strength/weakness.

# In[43]:


ot = games[games.ot == 1]; cl = games[(games.close == 1) & (games.ot == 0)]
rows = []
for id_, lab, d in [("T6", "Overtime games", ot), ("T7", "Regulation games decided by ≤5", cl)]:
    k, n = int(d.win.sum()), len(d); bt = stats.binomtest(k, n, .5); ci = bt.proportion_ci(method="exact")
    log.add(id_, f"{lab}: win% ≠ 50%?", "exact binomial", bt.pvalue, effect=k / n - .5, effect_name="win% − 50%", ci=(ci.low - .5, ci.high - .5), n=str(n))
    rows.append(dict(sample=lab, games=n, wins=k, win_pct=k / n, ci95=f"[{ci.low:.3f}, {ci.high:.3f}]", p_binomial=bt.pvalue))
display(pd.DataFrame(rows))
print("Overall win% for reference:", round(games.win.mean(), 3))


# ### 4.7 T8 — Momentum / "hot hand" at the team level
# Does winning the last game raise the chance of winning this one? Three lenses: χ² on (prev result × result), Wald-Wolfowitz **runs test** per season (Stouffer-combined), and **Ljung-Box** autocorrelation test on season-demeaned margins.

# In[44]:


from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.tsa.stattools import acf
g2 = games[games.game_no > 1]
tab = pd.crosstab(g2.prev_win, g2.win); chi = stats.chi2_contingency(tab, correction=False)
print("P(win | won last game) = %.3f   P(win | lost last game) = %.3f" % (g2[g2.prev_win == 1].win.mean(), g2[g2.prev_win == 0].win.mean()))
zs = [cu.runs_test(games[games.season == s].win.values)[0] for s in SEASONS]
z_comb = np.sum(zs) / np.sqrt(len(zs)); p_runs = 2 * stats.norm.sf(abs(z_comb))
dm = games.assign(dm=games.margin - games.groupby("season").margin.transform("mean")).dm.values
lb = acorr_ljungbox(dm, lags=[10], return_df=True).iloc[0]
log.add("T8a", "Result depends on previous result?", "χ² independence", chi.pvalue, effect=g2[g2.prev_win == 1].win.mean() - g2[g2.prev_win == 0].win.mean(), effect_name="Δ win% (after W − after L)", n=str(len(g2)))
log.add("T8b", "Win/loss sequence non-random?", "runs test (Stouffer)", p_runs, effect=z_comb, effect_name="combined z", n="5 seasons")
log.add("T8c", "Margins autocorrelated?", "Ljung-Box (10 lags)", lb.lb_pvalue, effect=np.nan, n=str(len(dm)))
print(f"χ²={chi.statistic:.2f}, p={chi.pvalue:.3f} | runs-test per-season z={np.round(zs, 2)}, combined p={p_runs:.3f} | Ljung-Box(10) Q={lb.lb_stat:.1f}, p={lb.lb_pvalue:.3f}")
a_ = acf(dm, nlags=15)[1:]; band = 1.96 / np.sqrt(len(dm))
fig = go.Figure(go.Bar(x=np.arange(1, 16), y=a_, marker_color=[cu.GREEN if abs(v) < band else cu.RED for v in a_]))
fig.add_hrect(y0=-band, y1=band, fillcolor="grey", opacity=.15, line_width=0)
fig.update_layout(title="T8 · Autocorrelation of game margins (shaded = 95% noise band)", xaxis_title="Lag (games)", yaxis_title="ACF", height=360)
show(fig, "20_T8_acf")


# ### 4.8 T9 & T10 — Scheduling quirks: matinees and the season's two halves
# * **T9** matinee tip-offs (before 5pm ET) vs evening.
# * **T10** first 41 vs last 41 games (load management / fatigue / roster chemistry).

# In[45]:


print("T9  matinee vs evening (margin):")
display(compare_continuous(games[games.matinee == 1].margin, games[games.matinee == 0].margin, "Matinee (<5pm)", "Evening", "T9", "Matinee margin ≠ evening margin?"))
m1, m0 = games[games.matinee == 1], games[games.matinee == 0]
print("   win%%: matinee %.3f (n=%d) vs evening %.3f -> p(Fisher)=%.3f" % (m1.win.mean(), len(m1), m0.win.mean(), stats.fisher_exact([[m1.win.sum(), len(m1) - m1.win.sum()], [m0.win.sum(), len(m0) - m0.win.sum()]])[1]))
print("\nT10 first 41 vs last 41 (margin):")
display(compare_continuous(games[games.half == "First 41"].margin, games[games.half == "Last 41"].margin, "First 41", "Last 41", "T10", "First-half margin ≠ second-half margin?"))
mo = pd.crosstab(games.month, games.win).reindex(["Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"]); chi = stats.chi2_contingency(mo)
log.add("T10b", "Win% differs by calendar month?", "χ² homogeneity", chi.pvalue, effect=np.sqrt(chi.statistic / 410 / 6), effect_name="Cramér's V", n="410")
mo["win%"] = mo[1] / (mo[0] + mo[1]); display(mo.rename(columns={0: "L", 1: "W"}))


# ### 4.9 T11 — Does crowd size track the scoreboard? (road attendance vs margin)

# In[46]:


hg, ag = games[games.home == 1], games[games.home == 0]
print("Home attendance: min/max =", hg.attendance.min(), hg.attendance.max(), "-> constant (listed capacity) for all 205 home games, so it carries no information.")
rho, p_rho = stats.spearmanr(ag.attendance, ag.margin)
log.add("T11", "Road-crowd size associated with margin?", "Spearman ρ", p_rho, effect=rho, effect_name="ρ", n=str(len(ag)))
print(f"Road games: Spearman ρ = {rho:+.3f}, p = {p_rho:.3f} (n={len(ag)}).  Caveat: road attendance mostly reflects arena capacity, i.e. it is confounded with *which* team/arena it is.")


# ### 4.10 T12 — Did the 3-point revolution in Boston happen, and did accuracy follow?
# Team-season counts. **T12a:** 3PA/FGA, 2021-22 vs 2024-25 (peak). **T12b:** χ² for 3P% homogeneity across all five seasons.

# In[47]:


tt = team.set_index("season")
x1, n1, x2, n2 = tt.loc["2021-22", "3PA"], tt.loc["2021-22", "FGA"], tt.loc["2024-25", "3PA"], tt.loc["2024-25", "FGA"]
z, p, diff, ci = cu.two_prop_ztest(x2, n2, x1, n1)
log.add("T12a", "3PA rate 2024-25 ≠ 2021-22?", "two-proportion z", p, effect=diff, effect_name="Δ 3PA/FGA", ci=ci, n=f"{int(n2)} vs {int(n1)}")
print(f"T12a  3PA/FGA: {x1/n1:.3f} → {x2/n2:.3f}  (Δ={diff:+.3f}, 95% CI [{ci[0]:+.3f},{ci[1]:+.3f}], z={z:.2f}, p={p:.2e})")
tab = np.c_[tt["3P"], tt["3PA"] - tt["3P"]]; chi = stats.chi2_contingency(tab)
log.add("T12b", "3P% differs across seasons?", "χ² homogeneity", chi.pvalue, effect=np.sqrt(chi.statistic / tab.sum()), effect_name="Cramér's V", n=str(int(tab.sum())))
print(f"T12b  3P% by season: {np.round(tt['3P%'].values, 3)}  χ²={chi.statistic:.2f}, p={chi.pvalue:.3f}")


# ### 4.11 T13 — Did the *shot distribution* shift? (2021-22 vs 2025-26, χ² on 5 zones)

# In[48]:


zc = {s: [players[players.season == s][f"fga_{z}"].sum() for z in cu.ZONES] for s in SEASONS}
tab = np.array([zc["2021-22"], zc["2025-26"]]); chi = stats.chi2_contingency(tab); V = np.sqrt(chi.statistic / tab.sum())
log.add("T13", "Shot-zone mix 2025-26 ≠ 2021-22?", "χ² (5 zones)", chi.pvalue, effect=V, effect_name="Cramér's V", n=str(int(tab.sum())))
st = pd.DataFrame(tab / tab.sum(axis=1, keepdims=True), index=["2021-22", "2025-26"], columns=[cu.ZONE_LABELS[z] for z in cu.ZONES])
resid = pd.DataFrame(chi.resid if hasattr(chi, "resid") else (tab - chi.expected_freq) / np.sqrt(chi.expected_freq), index=st.index, columns=st.columns)
display(st.round(3)); print(f"χ²={chi.statistic:.1f}, dof={chi.dof}, p={chi.pvalue:.2e}, Cramér's V={V:.3f}")
display(Markdown("Pearson residuals (|r|>2 = zone driving the shift):")); display(resid.round(2))


# ### 4.12 T14 — Starters vs bench efficiency (player-seasons ≥500 min)
# Caveat: the same player can appear in several seasons ⇒ observations aren't fully independent; treat the p-value as indicative.

# In[49]:


r = players[players.MP >= 500]
display(compare_continuous(r[r.role == "Starter"]["TS%"], r[r.role == "Bench"]["TS%"], "Starters", "Bench", "T14", "Starter TS% ≠ bench TS%?", outcome="TS%"))
print(f"Usage (USG%) starters vs bench: median {r[r.role == 'Starter']['USG%'].median():.1f} vs {r[r.role == 'Bench']['USG%'].median():.1f}")


# ### 4.13 T15 — A family of tests done properly: which returning players *really* changed their 3P%?
# Two-proportion z-tests on every player with ≥100 3PA in consecutive seasons, then **Benjamini-Hochberg FDR** across the whole family.

# In[50]:


pp = players[players["3PA"] >= 100][["player", "season", "3P", "3PA"]].copy()
pp["next"] = pp.season.map(lambda s: SEASONS[SEASONS.index(s) + 1] if SEASONS.index(s) < 4 else None)
pairs = pp.merge(pp, left_on=["player", "next"], right_on=["player", "season"], suffixes=("_a", "_b"))
res = []
for _, r in pairs.iterrows():
    z, p, d, ci = cu.two_prop_ztest(r["3P_b"], r["3PA_b"], r["3P_a"], r["3PA_a"])
    res.append(dict(player=r.player, change=f"{r.season_a} → {r.season_b}", pct_a=r["3P_a"] / r["3PA_a"], pct_b=r["3P_b"] / r["3PA_b"], diff=d, ci_lo=ci[0], ci_hi=ci[1], p=p))
fam = pd.DataFrame(res)
from statsmodels.stats.multitest import multipletests
fam["q_BH"] = multipletests(fam.p, method="fdr_bh")[1]; fam["p_Holm"] = multipletests(fam.p, method="holm")[1]
fam["label"] = fam.player + " " + fam.change
fam = fam.sort_values("diff")
fig = go.Figure(go.Scatter(x=fam["diff"], y=fam.label, mode="markers", error_x=dict(type="data", symmetric=False, array=fam.ci_hi - fam["diff"], arrayminus=fam["diff"] - fam.ci_lo),
                           marker=dict(size=9, color=np.where(fam.q_BH < .05, cu.RED, cu.GREEN)), customdata=np.c_[fam.pct_a, fam.pct_b, fam.p, fam.q_BH],
                           hovertemplate="%{y}<br>3P%%: %{customdata[0]:.1%} → %{customdata[1]:.1%}<br>p=%{customdata[2]:.3f}, FDR q=%{customdata[3]:.3f}<extra></extra>"))
fig.add_vline(x=0, line_dash="dot"); fig.update_layout(title=f"T15 · Year-over-year change in 3P% (95% CI) — {len(fam)} comparisons, {(fam.q_BH < .05).sum()} survive FDR (red)", height=max(450, 22 * len(fam)), xaxis_tickformat="+.0%")
show(fig, "21_T15_3p_changes")
print(f"{(fam.p < .05).sum()} of {len(fam)} are 'significant' at raw p<.05; {(fam.p_Holm < .05).sum()} survive Holm; {(fam.q_BH < .05).sum()} survive BH-FDR.")
display(fam[["label", "pct_a", "pct_b", "diff", "p", "p_Holm", "q_BH"]].sort_values("p").head(8).round(4))


# ### 4.14 Statistical power — what effect sizes could these samples ever detect?
# Non-significant ≠ "no effect". The **minimum detectable effect (MDE)** at 80% power tells you what the data were able to see.

# In[51]:


n_grid = np.arange(10, 420, 5)
fig = make_subplots(rows=1, cols=2, subplot_titles=("MDE for margin difference (pts)", "MDE for win-% difference"))
fig.add_scatter(x=n_grid, y=[cu.mde_two_sample(n, n, SD_MARGIN) for n in n_grid], line=dict(color=cu.GREEN, width=3), name="equal groups", row=1, col=1)
fig.add_scatter(x=n_grid, y=[cu.mde_two_prop(n, n, p=games.win.mean()) for n in n_grid], line=dict(color=cu.GOLD, width=3), name="equal groups", row=1, col=2)
cases = {"T1/T2 home vs away": (205, 205), "T3 B2B vs rested": (68, 342), "T9 matinee vs evening": (33, 377), "T10 first vs last 41": (205, 205)}
for lab, (a, b) in cases.items():
    fig.add_scatter(x=[min(a, b)], y=[cu.mde_two_sample(a, b, SD_MARGIN)], mode="markers+text", text=[lab], textposition="top right", marker=dict(size=11, color=cu.BLACK), showlegend=False, row=1, col=1)
    fig.add_scatter(x=[min(a, b)], y=[cu.mde_two_prop(a, b, p=games.win.mean())], mode="markers", marker=dict(size=11, color=cu.BLACK), showlegend=False, row=1, col=2)
fig.update_xaxes(title="smaller group size n"); fig.update_layout(height=440, title="Minimum detectable effect (α=.05, power=.80)", showlegend=False)
show(fig, "22_power_mde")
display(pd.DataFrame([dict(test=k, n_A=a, n_B=b, MDE_margin_pts=cu.mde_two_sample(a, b, SD_MARGIN), MDE_win_pct_pts=100 * cu.mde_two_prop(a, b, games.win.mean())) for k, (a, b) in cases.items()]).round(2))


# ### 4.15 Bayesian A/B view (Beta-Binomial)
# Instead of "reject / fail to reject", get **P(A is better than B)** and a credible interval for the difference. Uniform Beta(1,1) priors; 200k posterior draws.

# In[52]:


rng = np.random.default_rng(SEED)
def bayes_ab(xa, na, xb, nb, N=200_000):
    pa, pb = rng.beta(1 + xa, 1 + na - xa, N), rng.beta(1 + xb, 1 + nb - xb, N); d = pa - pb
    return dict(P_A_better=(d > 0).mean(), mean_diff=d.mean(), cri_lo=np.percentile(d, 2.5), cri_hi=np.percentile(d, 97.5))
w = lambda m: (int(games[m].win.sum()), int(m.sum()))
rows = {
  "Home vs Away win%":            bayes_ab(*w(games.home == 1), *w(games.home == 0)),
  "Rested vs Back-to-back":       bayes_ab(*w(games.b2b == 0), *w(games.b2b == 1)),
  "Evening vs Matinee":           bayes_ab(*w(games.matinee == 0), *w(games.matinee == 1)),
  "2023-24 vs 2025-26 win%":      bayes_ab(*w(games.season == "2023-24"), *w(games.season == "2025-26")),
  "2025-26 vs 2021-22 win%":      bayes_ab(*w(games.season == "2025-26"), *w(games.season == "2021-22")),
  "2024-25 vs 2025-26 win%":      bayes_ab(*w(games.season == "2024-25"), *w(games.season == "2025-26")),
}
bt = pd.DataFrame(rows).T; display(bt.round(3))
xs = np.linspace(.3, 1, 500); fig = go.Figure()
for s in SEASONS:
    k, n = w(games.season == s); fig.add_scatter(x=xs, y=stats.beta.pdf(xs, 1 + k, 1 + n - k), name=f"{s} ({k}-{n - k})", fill="tozeroy", line=dict(color=SC[s]), opacity=.55)
fig.update_layout(title="Posterior of 'true' win probability by season (what the 82-game record can really tell us)", xaxis_title="True win probability", yaxis_title="Posterior density", height=420, xaxis_tickformat=".0%")
show(fig, "23_bayes_season_posteriors")


# ### 4.16 Scorecard — all logged tests with multiple-comparison correction

# In[53]:


tab = log.table()
tab["verdict"] = np.select([tab.sig_holm, tab.sig_fdr, tab.sig_raw], ["✅ robust (Holm)", "◑ survives FDR", "⚠ raw p only"], "—  not significant")
disp = tab[["id", "question", "test", "n", "effect_name", "effect", "ci_low", "ci_high", "p_value", "p_holm", "q_bh", "verdict"]].copy()
display(disp.round(4))
tab["neglogp"] = -np.log10(tab.p_value.clip(lower=1e-12)); tab["neglogp_holm"] = -np.log10(tab.p_holm.clip(lower=1e-12))
fig = go.Figure()
fig.add_bar(y=tab.id + " · " + tab.question.str.slice(0, 48), x=tab.neglogp, orientation="h", name="raw p", marker_color=cu.GOLD)
fig.add_bar(y=tab.id + " · " + tab.question.str.slice(0, 48), x=tab.neglogp_holm, orientation="h", name="Holm-adjusted p", marker_color=cu.GREEN)
fig.add_vline(x=-np.log10(.05), line_dash="dash", line_color=cu.RED, annotation_text="α = .05")
fig.update_layout(barmode="group", title="Evidence against H₀ (−log₁₀ p) — before and after multiple-testing correction", height=max(500, 34 * len(tab)), yaxis=dict(autorange="reversed"), xaxis_title="−log₁₀(p)")
show(fig, "24_scorecard")

# Forest of margin effects (same units: points)
fm = tab[tab.effect_name.str.contains("Δ margin")]
fig = go.Figure(go.Scatter(x=fm.effect, y=fm.id + " · " + fm.question, mode="markers", marker=dict(size=12, color=cu.GREEN),
                           error_x=dict(type="data", symmetric=False, array=fm.ci_high - fm.effect, arrayminus=fm.effect - fm.ci_low)))
fig.add_vline(x=0, line_dash="dot"); fig.update_layout(title="Scoring-margin effects with 95% bootstrap CIs (points per game)", height=320, xaxis_title="Δ margin (A − B)")
show(fig, "25_margin_forest")


# ## 5 · Statistical modelling
# ### 5.1 Logistic regression — what pre-game factors move win probability?
# `win ~ home + back-to-back + 3-in-4 + matinee + form10 + season`, with heteroskedasticity-robust (HC3) standard errors.
# `form10` = average margin over the previous 10 games (a within-season momentum/strength signal); season fixed effects absorb each year's overall strength.

# In[54]:


FORM = "home + b2b + three_in_four + matinee + form10 + C(season)"
logit = smf.glm(f"win ~ {FORM}", data=games, family=sm.families.Binomial()).fit(cov_type="HC3")
null = smf.glm("win ~ 1", data=games, family=sm.families.Binomial()).fit()
lr = 2 * (logit.llf - null.llf); df_ = len(logit.params) - 1
print(f"McFadden pseudo-R² = {1 - logit.llf / null.llf:.3f} | LR test vs intercept-only: χ²({df_}) = {lr:.1f}, p = {stats.chi2.sf(lr, df_):.4f}")

ci = logit.conf_int(); orx = pd.DataFrame({"odds_ratio": np.exp(logit.params), "lo": np.exp(ci[0]), "hi": np.exp(ci[1]), "p": logit.pvalues}).drop("Intercept")
display(orx.round(3))
fig = go.Figure(go.Scatter(x=orx.odds_ratio, y=orx.index, mode="markers", marker=dict(size=11, color=np.where(orx.p < .05, cu.RED, cu.GREEN)),
                           error_x=dict(type="data", symmetric=False, array=orx.hi - orx.odds_ratio, arrayminus=orx.odds_ratio - orx.lo),
                           customdata=orx.p, hovertemplate="%{y}<br>OR = %{x:.2f}<br>p = %{customdata:.3f}<extra></extra>"))
fig.add_vline(x=1, line_dash="dot"); fig.update_xaxes(type="log", title="Odds ratio (log scale)")
fig.update_layout(title="Logistic model — odds ratios with 95% CI (red = p<.05)", height=450); show(fig, "26_logit_forest")

# Average marginal effect of playing at home, bootstrapped
def ame_home(d):
    m = smf.glm(f"win ~ {FORM}", data=d, family=sm.families.Binomial()).fit()
    return (m.predict(d.assign(home=1)) - m.predict(d.assign(home=0))).mean()
rng = np.random.default_rng(SEED)
boots = [ame_home(games.sample(len(games), replace=True, random_state=int(rng.integers(1e9)))) for _ in range(300)]
print(f"Average marginal effect of home court on P(win): {ame_home(games):+.3f}  (bootstrap 95% CI [{np.percentile(boots, 2.5):+.3f}, {np.percentile(boots, 97.5):+.3f}])")


# ### 5.2 OLS on scoring margin (robust SEs) + full diagnostics

# In[55]:


ols = smf.ols(f"margin ~ {FORM}", data=games).fit(cov_type="HC3")
display(pd.DataFrame({"coef": ols.params, "robust_se": ols.bse, "lo": ols.conf_int()[0], "hi": ols.conf_int()[1], "p": ols.pvalues}).round(3))
print(f"R² = {ols.rsquared:.3f}, adj-R² = {ols.rsquared_adj:.3f}, F-test p = {ols.f_pvalue:.4f}  (n={int(ols.nobs)})")

from statsmodels.stats.diagnostic import het_breuschpagan
from statsmodels.stats.stattools import durbin_watson, jarque_bera
from statsmodels.stats.outliers_influence import variance_inflation_factor
plain = smf.ols(f"margin ~ {FORM}", data=games).fit()
bp = het_breuschpagan(plain.resid, plain.model.exog); jb = jarque_bera(plain.resid)
X = plain.model.exog; vif = pd.Series([variance_inflation_factor(X, i) for i in range(1, X.shape[1])], index=plain.model.exog_names[1:])
diag = pd.DataFrame({"check": ["Breusch-Pagan (homoskedasticity)", "Jarque-Bera (normal residuals)", "Durbin-Watson (≈2 = no autocorr.)", "Max VIF"],
                     "result": [f"p = {bp[1]:.3f}", f"p = {jb[1]:.3f}", f"{durbin_watson(plain.resid):.2f}", f"{vif.max():.2f}"]}); display(diag)

osm = sm.ProbPlot(plain.resid, dist=stats.norm); tq, sq = osm.theoretical_quantiles, osm.sample_quantiles
fig = make_subplots(rows=1, cols=2, subplot_titles=("Residuals vs fitted", "Normal Q-Q of residuals"))
fig.add_scatter(x=plain.fittedvalues, y=plain.resid, mode="markers", marker=dict(color=cu.GREEN, opacity=.5), showlegend=False, row=1, col=1)
fig.add_hline(y=0, line_dash="dot", row=1, col=1)
fig.add_scatter(x=tq, y=np.sort(sq), mode="markers", marker=dict(color=cu.GREEN, opacity=.6), showlegend=False, row=1, col=2)
fig.add_scatter(x=[tq.min(), tq.max()], y=[tq.min() * plain.resid.std(), tq.max() * plain.resid.std()], mode="lines", line=dict(color=cu.RED, dash="dash"), showlegend=False, row=1, col=2)
fig.update_layout(height=400, title="OLS diagnostics"); show(fig, "27_ols_diagnostics")


# ### 5.3 Mixed-effects model — how much does *who the opponent is* matter?
# `margin ~ home + b2b + season + (1 | opponent)`. The random-intercept variance relative to total gives the **intra-class correlation (ICC)**:
# the share of game-to-game margin variance attributable to opponent identity. Opponent BLUPs are shrunk estimates of "how much better/worse than average the Celtics do against this team."

# In[56]:


mm = smf.mixedlm("margin ~ home + b2b + C(season)", games, groups=games["opponent"]).fit(reml=False)
vc = float(mm.cov_re.iloc[0, 0]); sc = mm.scale; icc = vc / (vc + sc)
ols0 = smf.ols("margin ~ home + b2b + C(season)", games).fit()
lrt = 2 * (mm.llf - ols0.llf); p_lrt = 0.5 * stats.chi2.sf(max(lrt, 0), 1)       # boundary-corrected χ²(0)/χ²(1) mixture
print(f"Opponent variance = {vc:.2f} | residual variance = {sc:.1f} | ICC = {icc:.3f}")
print(f"LRT for the random effect: χ² = {lrt:.2f}, p = {p_lrt:.3f}   -> {'opponent identity adds signal' if p_lrt < .05 else 'no clear opponent-specific effect beyond noise'}")
re = pd.Series({k: v.iloc[0] for k, v in mm.random_effects.items()}).sort_values()
fig = go.Figure(go.Bar(x=re.values, y=re.index, orientation="h", marker_color=np.where(re.values > 0, cu.GREEN, cu.RED)))
fig.update_layout(title="Opponent random effects (BLUPs, pts of margin vs. average opponent) — heavily shrunk", height=760, xaxis_title="Shrunken opponent effect")
show(fig, "28_opponent_blups")


# ### 5.4 Monte-Carlo season simulation — how much of a win total is luck?
# Model each season's margin as Normal(μ_season, σ_season) ⇒ per-game P(win) = Φ(μ/σ). Simulate 50,000 82-game seasons per year and see where the *actual* win total falls.
# The binomial spread alone (≈ ±4 wins) is a reminder of how noisy a single record is. We compare with the **Pythagorean expectation** (exponent 13.91).

# In[57]:


rng = np.random.default_rng(SEED); N = 50_000
sim_rows = []; fig = go.Figure()
for s in SEASONS:
    d = games[games.season == s]; mu, sd = d.margin.mean(), d.margin.std(); p = stats.norm.cdf(mu / sd)
    wins = rng.binomial(82, p, N); act = int(d.win.sum())
    sim_rows.append(dict(season=s, actual_W=act, margin_expected_W=82 * p, pyth_W=float(team.set_index("season").loc[s, "pyth_W"]),
                         sim_sd=wins.std(), P_sim_ge_actual=(wins >= act).mean(), pctile=(wins <= act).mean()))
    vals, cnt = np.unique(wins, return_counts=True)
    fig.add_bar(x=vals, y=cnt / N, name=s, marker_color=SC[s], opacity=.55, visible=True if s == "2025-26" else "legendonly")
sim = pd.DataFrame(sim_rows); display(sim.round(3))
fig.update_layout(barmode="overlay", title="Simulated win totals given each season's margin profile (click legend to compare seasons)", xaxis_title="Wins in 82 games", yaxis_title="Probability", height=430)
for s, a in zip(sim.season, sim.actual_W): fig.add_vline(x=a, line_dash="dot", line_color=SC[s], opacity=.8)
show(fig, "29_season_simulation")


# ### 5.5 Empirical-Bayes shrinkage of 3-point percentage
# A player who shoots 40% on 60 attempts is *not* a 40% shooter. Fit a **Beta prior** to all rotation players' (3P, 3PA) by maximum likelihood (beta-binomial), then compute each player's **posterior** 3P% — pulling small samples toward the group mean in proportion to how little data they have.
# Then **back-test**: does the shrunk estimate predict next season better than the raw one? (Prior is fitted only on seasons up to *t*, so there's no look-ahead.)

# In[58]:


from scipy.optimize import minimize
def fit_beta_prior(df):
    nll = lambda th: -stats.betabinom.logpmf(df["3P"], df["3PA"], *np.exp(th)).sum()
    r = minimize(nll, [np.log(15), np.log(25)], method="Nelder-Mead"); return np.exp(r.x)

MIN3 = 50
a0, b0 = fit_beta_prior(players[players["3PA"] >= MIN3])
print(f"Fitted prior: Beta(α={a0:.1f}, β={b0:.1f}) -> mean {a0 / (a0 + b0):.3f}, equivalent to ~{a0 + b0:.0f} 'pseudo-attempts' of prior information")

cur = players[(players.season == "2025-26") & (players["3PA"] >= MIN3)].copy()
cur["raw"] = cur["3P"] / cur["3PA"]; A, B = a0 + cur["3P"], b0 + cur["3PA"] - cur["3P"]
cur["eb"] = A / (A + B); cur["lo"], cur["hi"] = stats.beta.ppf(.05, A, B), stats.beta.ppf(.95, A, B); cur = cur.sort_values("eb")
fig = go.Figure()
fig.add_scatter(x=cur.eb, y=cur.player, mode="markers", name="Shrunk (posterior mean, 90% CrI)", marker=dict(size=11, color=cu.GREEN),
                error_x=dict(type="data", symmetric=False, array=cur.hi - cur.eb, arrayminus=cur.eb - cur.lo), customdata=np.c_[cur["3P"], cur["3PA"], cur.raw],
                hovertemplate="%{y}<br>Posterior %{x:.1%}<br>Raw %{customdata[2]:.1%} (%{customdata[0]}/%{customdata[1]})<extra></extra>")
fig.add_scatter(x=cur.raw, y=cur.player, mode="markers", name="Raw 3P%", marker=dict(size=9, color="rgba(0,0,0,0)", line=dict(color=cu.GOLD, width=2.5)))
fig.add_vline(x=a0 / (a0 + b0), line_dash="dot", annotation_text="group mean")
fig.update_layout(title="2025-26 three-point shooting — raw vs. empirical-Bayes posterior", height=max(420, 40 * len(cur)), xaxis_tickformat=".0%"); show(fig, "30_eb_3p")

# ---- Back-test: predict next-season 3P% (weights = next-season attempts) --------------------
bt = []
for i, s in enumerate(SEASONS[:-1]):
    hist = players[(players.season <= s) & (players["3PA"] >= MIN3)]; a, b = fit_beta_prior(hist)
    t0 = players[(players.season == s) & (players["3PA"] >= MIN3)][["player", "3P", "3PA"]]
    t1 = players[(players.season == SEASONS[i + 1]) & (players["3PA"] >= MIN3)][["player", "3P", "3PA"]]
    m = t0.merge(t1, on="player", suffixes=("_t", "_t1"))
    for _, r in m.iterrows():
        bt.append(dict(season=s, player=r.player, w=r["3PA_t1"], y=r["3P_t1"] / r["3PA_t1"], raw=r["3P_t"] / r["3PA_t"],
                       eb=(a + r["3P_t"]) / (a + b + r["3PA_t"]), grand=hist["3P"].sum() / hist["3PA"].sum()))
bt = pd.DataFrame(bt)
wrmse = lambda c: np.sqrt(np.average((bt[c] - bt.y) ** 2, weights=bt.w))
res = pd.DataFrame({"predictor": ["Last season's raw 3P%", "Empirical-Bayes shrunk 3P%", "Pooled team average (ignore player)"], "weighted RMSE": [wrmse("raw"), wrmse("eb"), wrmse("grand")]})
res["improvement vs raw"] = 1 - res["weighted RMSE"] / res["weighted RMSE"].iloc[0]
print(f"{len(bt)} player-season transitions"); display(res.round(4))
sl = smf.wls("y ~ raw", data=bt, weights=bt.w).fit(); print(f"Year-over-year slope of next-3P% on this-3P%: {sl.params['raw']:.2f} (95% CI {sl.conf_int().loc['raw', 0]:.2f} to {sl.conf_int().loc['raw', 1]:.2f})  -> 1.0 would mean 'pure skill', 0 'pure noise'")


# ## 6 · Machine learning
# ### 6.1 Predicting game outcomes from **pre-game information only**
# * Target: win (1) / loss (0). Features: venue, rest/fatigue, schedule density, rolling form, season-to-date win%, tip-off time, and an **opponent-history** feature (shrunken average past margin vs that opponent, computed **only from training seasons**, leave-one-out for training rows).
# * **Validation:** leave-one-season-out CV (5 folds) — each season is predicted by a model that never saw it. A forward-chaining check (train on past seasons only) follows.
# * **Baselines matter:** the Celtics won 70.5% of games, so "always pick the Celtics" is already 70.5% accurate. A model must beat that *and* a home-only model on log-loss/AUC to be useful.

# In[59]:


from sklearn.base import clone
from sklearn.dummy import DummyClassifier
from sklearn.linear_model import LogisticRegression, Ridge
from sklearn.ensemble import RandomForestClassifier, HistGradientBoostingClassifier, RandomForestRegressor, HistGradientBoostingRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import roc_auc_score, log_loss, brier_score_loss, accuracy_score, roc_curve, mean_squared_error, mean_absolute_error
from sklearn.inspection import permutation_importance
from sklearn.calibration import calibration_curve
try:
    from xgboost import XGBClassifier; HAS_XGB = True
except Exception: HAS_XGB = False

BASE = ["home", "days_rest", "b2b", "three_in_four", "games_prev_4d", "run_len", "form5", "form10", "win_pct_to_date", "prev_win", "matinee", "weekend", "start_hour", "game_no"]
ALL = BASE + ["opp_hist_margin"]
models = {
    "Baseline · team win rate": (DummyClassifier(strategy="prior"), ["home"]),
    "Baseline · home-only logit": (LogisticRegression(), ["home"]),
    "Logistic (L2)":            (make_pipeline(StandardScaler(), LogisticRegression(C=.1, max_iter=2000)), ALL),
    "Random forest":            (RandomForestClassifier(n_estimators=400, min_samples_leaf=20, max_features="sqrt", random_state=SEED, n_jobs=-1), ALL),
    "HistGradientBoosting":     (HistGradientBoostingClassifier(max_depth=3, learning_rate=.04, max_iter=120, min_samples_leaf=25, l2_regularization=2., random_state=SEED), ALL),
}
if HAS_XGB:
    models["XGBoost"] = (XGBClassifier(n_estimators=200, max_depth=2, learning_rate=.03, subsample=.8, colsample_bytree=.8, reg_lambda=5., random_state=SEED, verbosity=0), ALL)

def cv_classify(scheme="loso"):
    folds = SEASONS if scheme == "loso" else SEASONS[2:]
    recs, oof, fitted = [], [], {}
    for s in folds:
        tr = games[games.season != s] if scheme == "loso" else games[games.season < s]; te = games[games.season == s]
        tr_enc, te_enc = cu.shrunk_opponent_rate(tr, te)
        Xtr, Xte = tr[BASE].assign(opp_hist_margin=tr_enc), te[BASE].assign(opp_hist_margin=te_enc)
        for name, (est, feats) in models.items():
            m = clone(est).fit(Xtr[feats], tr.win); p = np.clip(m.predict_proba(Xte[feats])[:, 1], 1e-4, 1 - 1e-4); y = te.win.values
            recs.append(dict(fold=s, model=name, AUC=roc_auc_score(y, p) if len(set(p)) > 1 else .5, logloss=log_loss(y, p), brier=brier_score_loss(y, p), accuracy=accuracy_score(y, p > .5)))
            oof.append(pd.DataFrame(dict(fold=s, model=name, y=y, p=p, game_no=te.game_no.values))); fitted[(s, name)] = (m, Xte[feats], y)
    return pd.DataFrame(recs), pd.concat(oof), fitted

res_loso, oof_loso, fitted = cv_classify("loso")
summ = res_loso.groupby("model")[["AUC", "logloss", "brier", "accuracy"]].agg(["mean", "std"]).round(4)
display(summ.sort_values(("logloss", "mean")))


# In[60]:


# ---- visual comparison: per-fold dots + mean -----------------------------------------------
order = res_loso.groupby("model").logloss.mean().sort_values().index.tolist()
fig = make_subplots(rows=1, cols=3, subplot_titles=("ROC AUC  (↑ better, 0.5 = coin flip)", "Log-loss  (↓ better)", "Accuracy @ 0.5"), horizontal_spacing=.08)
for k, met in enumerate(["AUC", "logloss", "accuracy"]):
    mean = res_loso.groupby("model")[met].mean().loc[order]
    fig.add_bar(x=mean.values, y=mean.index, orientation="h", marker_color=[cu.GOLD if "Baseline" in m else cu.GREEN for m in mean.index], opacity=.75, showlegend=False, row=1, col=k + 1)
    fig.add_scatter(x=res_loso[met], y=res_loso.model, mode="markers", marker=dict(color="black", size=6, opacity=.7), text=res_loso.fold, showlegend=False, hovertemplate="%{y}<br>%{text}: %{x:.3f}<extra></extra>", row=1, col=k + 1)
fig.update_yaxes(autorange="reversed"); fig.update_layout(height=430, title="Leave-one-season-out CV (dots = held-out seasons; gold = baselines)")
fig.update_xaxes(range=[.4, .7], row=1, col=1); fig.update_xaxes(range=[.4, .8], row=1, col=3)
show(fig, "31_ml_cv_comparison")

# ---- pooled ROC + calibration for the non-baseline models -------------------------------------
fig = make_subplots(rows=1, cols=2, subplot_titles=("Pooled out-of-fold ROC", "Calibration (reliability) of out-of-fold probabilities"))
for name in [m for m in models if "Baseline" not in m] + ["Baseline · home-only logit"]:
    o = oof_loso[oof_loso.model == name]; fpr, tpr, _ = roc_curve(o.y, o.p)
    fig.add_scatter(x=fpr, y=tpr, name=f"{name} (AUC {roc_auc_score(o.y, o.p):.3f})", row=1, col=1)
    pt, pp = calibration_curve(o.y, o.p, n_bins=6, strategy="quantile"); fig.add_scatter(x=pp, y=pt, mode="lines+markers", name=name, showlegend=False, row=1, col=2)
fig.add_scatter(x=[0, 1], y=[0, 1], mode="lines", line=dict(dash="dash", color="grey"), showlegend=False, row=1, col=1)
fig.add_scatter(x=[0, 1], y=[0, 1], mode="lines", line=dict(dash="dash", color="grey"), showlegend=False, row=1, col=2)
fig.update_xaxes(title="False-positive rate", row=1, col=1); fig.update_yaxes(title="True-positive rate", row=1, col=1)
fig.update_xaxes(title="Predicted P(win)", row=1, col=2); fig.update_yaxes(title="Observed win frequency", row=1, col=2); fig.update_layout(height=480)
show(fig, "32_ml_roc_calibration")


# In[61]:


# ---- Is the best model *significantly* better than the home-only baseline?  (paired bootstrap on pooled OOF) ----
cand = res_loso[~res_loso.model.str.contains("Baseline")].groupby("model").logloss.mean().idxmin()
print("Best non-baseline model by log-loss:", cand)
o1 = oof_loso[oof_loso.model == cand].reset_index(drop=True); o0 = oof_loso[oof_loso.model == "Baseline · home-only logit"].reset_index(drop=True)
rng = np.random.default_rng(SEED); dA, dL = [], []
for _ in range(2000):
    ix = rng.integers(0, len(o1), len(o1))
    dA.append(roc_auc_score(o1.y[ix], o1.p[ix]) - roc_auc_score(o0.y[ix], o0.p[ix]))
    dL.append(log_loss(o0.y[ix], o0.p[ix]) - log_loss(o1.y[ix], o1.p[ix]))          # positive = candidate better
print(f"ΔAUC vs home-only baseline: {np.mean(dA):+.3f}  (95% CI {np.percentile(dA, 2.5):+.3f} … {np.percentile(dA, 97.5):+.3f})")
print(f"Δlog-loss improvement      : {np.mean(dL):+.4f} (95% CI {np.percentile(dL, 2.5):+.4f} … {np.percentile(dL, 97.5):+.4f})")

# ---- Permutation importance (ΔAUC on held-out seasons, averaged over folds) ---------------------
imp = []
for s in SEASONS:
    m, Xte, y = fitted[(s, cand)]; pi = permutation_importance(m, Xte, y, scoring="roc_auc", n_repeats=20, random_state=SEED)
    imp.append(pd.Series(pi.importances_mean, index=Xte.columns))
imp = pd.concat(imp, axis=1).mean(axis=1).sort_values()
fig = go.Figure(go.Bar(x=imp.values, y=imp.index, orientation="h", marker_color=np.where(imp.values > 0, cu.GREEN, cu.GREY)))
fig.update_layout(title=f"Permutation importance — {cand} (mean drop in held-out AUC; ≤0 ≈ useless)", height=480, xaxis_title="ΔAUC when feature is shuffled"); show(fig, "33_ml_perm_importance")

# ---- Robustness: forward-chaining (train on past seasons only) ------------------------------------
res_fwd, _, _ = cv_classify("forward")
print("\nForward-chaining check (train on earlier seasons only; test 2023-24 / 2024-25 / 2025-26):")
display(res_fwd.groupby("model")[["AUC", "logloss", "brier", "accuracy"]].mean().round(4).sort_values("logloss"))


# ### 6.2 Predicting the *margin* of victory (regression)
# Same features & validation. Metric: RMSE/MAE and **out-of-sample R²** relative to a baseline that always predicts the training-set mean margin (R²ₒₒₛ < 0 ⇒ worse than the baseline).

# In[62]:


reg_models = {"Baseline · mean margin": None,
              "Ridge": make_pipeline(StandardScaler(), Ridge(alpha=200.)),
              "Random forest": RandomForestRegressor(n_estimators=400, min_samples_leaf=25, max_features="sqrt", random_state=SEED, n_jobs=-1),
              "HistGradientBoosting": HistGradientBoostingRegressor(max_depth=3, learning_rate=.03, max_iter=100, min_samples_leaf=30, l2_regularization=3., random_state=SEED)}
rr, preds = [], []
for s in SEASONS:
    tr, te = games[games.season != s], games[games.season == s]; tr_enc, te_enc = cu.shrunk_opponent_rate(tr, te)
    Xtr, Xte = tr[BASE].assign(opp_hist_margin=tr_enc), te[BASE].assign(opp_hist_margin=te_enc); y = te.margin.values; base_p = np.full(len(te), tr.margin.mean())
    for name, est in reg_models.items():
        p = base_p if est is None else clone(est).fit(Xtr, tr.margin).predict(Xte)
        rr.append(dict(fold=s, model=name, RMSE=np.sqrt(mean_squared_error(y, p)), MAE=mean_absolute_error(y, p), R2_oos=1 - ((y - p) ** 2).sum() / ((y - base_p) ** 2).sum()))
        preds.append(pd.DataFrame(dict(model=name, fold=s, y=y, p=p, opp=te.opponent.values, game_no=te.game_no.values)))
rr, preds = pd.DataFrame(rr), pd.concat(preds)
display(rr.groupby("model")[["RMSE", "MAE", "R2_oos"]].mean().round(3).sort_values("RMSE"))
best = rr[rr.model != "Baseline · mean margin"].groupby("model").RMSE.mean().idxmin(); d = preds[preds.model == best]
fig = px.scatter(d, x="p", y="y", color="fold", color_discrete_map=SC, hover_data=["opp", "game_no"], opacity=.7, labels={"p": f"Predicted margin ({best})", "y": "Actual margin"},
                 title=f"Predicted vs actual margin (out-of-fold) — correlation r = {np.corrcoef(d.p, d.y)[0, 1]:.2f}")
fig.add_hline(y=0, line_dash="dot"); fig.add_vline(x=0, line_dash="dot"); fig.add_scatter(x=[-10, 25], y=[-10, 25], mode="lines", line=dict(dash="dash", color="grey"), name="perfect"); fig.update_layout(height=480)
show(fig, "34_ml_margin_scatter")


# ### 6.3 Unsupervised learning — player **archetypes** (K-Means + PCA)
# Each rotation player-season (≥400 min) is described by per-36 production, shot profile and efficiency; features are standardised, **k** is chosen by silhouette score, and the result is cross-checked with Ward hierarchical clustering (Adjusted Rand Index) — if two different algorithms agree, the structure is probably real rather than an artefact.

# In[63]:


from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score, calinski_harabasz_score, adjusted_rand_score

FEATS = {"PTS_36": "Scoring", "ORB_36": "Off. rebounds", "DRB_36": "Def. rebounds", "AST_36": "Playmaking", "STL_36": "Steals", "BLK_36": "Rim protection", "TOV_36": "Turnovers",
         "3PAr": "3-pt rate", "FTr": "Free-throw rate", "TS%": "True shooting", "USG%": "Usage", "Avg Dist (ft)": "Shot distance", "%FGA 0-3": "At-rim share", "Dunk %FGA": "Dunk share"}
pl = players[players.MP >= 400].dropna(subset=list(FEATS)).reset_index(drop=True)
Xs = StandardScaler().fit_transform(pl[list(FEATS)]); print(f"{len(pl)} player-seasons, {Xs.shape[1]} features")

ks = range(2, 9); sil, ch, inertia = [], [], []
for k in ks:
    km = KMeans(k, n_init=50, random_state=SEED).fit(Xs); sil.append(silhouette_score(Xs, km.labels_)); ch.append(calinski_harabasz_score(Xs, km.labels_)); inertia.append(km.inertia_)
K = list(ks)[int(np.argmax(sil[1:5])) + 1]      # best silhouette among k = 3..6 (interpretable range)
fig = make_subplots(rows=1, cols=3, subplot_titles=("Silhouette (↑)", "Calinski-Harabasz (↑)", "Inertia / elbow (↓)"))
for i, (v, c) in enumerate([(sil, cu.GREEN), (ch, cu.GOLD), (inertia, cu.BLACK)]): fig.add_scatter(x=list(ks), y=v, mode="lines+markers", line=dict(color=c), showlegend=False, row=1, col=i + 1)
fig.add_vline(x=K, line_dash="dash", line_color=cu.RED); fig.update_layout(height=340, title=f"Choosing k  → k = {K}")
show(fig, "35_cluster_k_selection")

km = KMeans(K, n_init=100, random_state=SEED).fit(Xs); pl["cluster_id"] = km.labels_
ward = AgglomerativeClustering(K, linkage="ward").fit_predict(Xs)
print(f"K-Means vs Ward agreement (Adjusted Rand Index) = {adjusted_rand_score(km.labels_, ward):.2f}   (1 = identical, 0 = random)")
cent = pd.DataFrame(km.cluster_centers_, columns=list(FEATS.values()))
names = {}
for c in range(K):
    top = cent.loc[c].sort_values(ascending=False).head(3).index.tolist(); low = cent.loc[c].sort_values().head(1).index.tolist()
    names[c] = f"C{c}: " + " + ".join(top[:2]) + f" (low {low[0].lower()})"
pl["archetype"] = pl.cluster_id.map(names)
fig = px.imshow(cent.rename(index=names), color_continuous_scale="RdBu_r", zmin=-2, zmax=2, aspect="auto", text_auto=".1f", title="Archetype profiles (z-scores of cluster centres)")
fig.update_layout(height=120 + 90 * K); show(fig, "36_cluster_profiles")


# In[64]:


pca = PCA(3, random_state=SEED).fit(Xs); P = pca.transform(Xs); print("PCA explained variance:", np.round(pca.explained_variance_ratio_, 3), "| cumulative:", np.round(pca.explained_variance_ratio_.cumsum(), 3))
pl[["PC1", "PC2", "PC3"]] = P
fig = px.scatter_3d(pl, x="PC1", y="PC2", z="PC3", color="archetype", hover_name="player", symbol="season", opacity=.85,
                    hover_data={"season": True, "MPG": ":.1f", "PTS_36": ":.1f", "TS%": ":.3f", "USG%": ":.1f", "PC1": False, "PC2": False, "PC3": False},
                    title=f"Player-season archetype map — PCA ({pca.explained_variance_ratio_.sum():.0%} of variance)")
fig.update_traces(marker_size=6); fig.update_layout(height=680, legend=dict(orientation="h", y=-.05)); show(fig, "37_cluster_pca3d")
load = pd.DataFrame(pca.components_.T, index=list(FEATS.values()), columns=["PC1", "PC2", "PC3"]).round(2); display(load.sort_values("PC1"))
display(pl.sort_values(["cluster_id", "MP"], ascending=[True, False]).groupby("archetype").head(6)[["archetype", "player", "season", "MPG", "PTS_36", "TS%", "USG%"]].round(2))


# ### 6.4 Small-n forecasting — how predictable is a player's *next* season?
# Pairs of consecutive Celtics seasons for the same player (≥500 min in both). Target: next-season **TS%**. We compare *persistence* (“he'll do the same”), *league-mean*, an *in-sample regression-to-the-mean line* (optimistic reference, since it is fitted on the same data), and a Ridge model, with **leave-one-player-out** CV (so the same player never sits in train and test).

# In[65]:


from sklearn.model_selection import GroupKFold, cross_val_predict
q = players[players.MP >= 500][["player", "season", "age", "TS%", "3PAr", "USG%", "PTS_36", "min_share", "FTr", "AST_36"]]
q = q.assign(next=q.season.map(lambda s: SEASONS[SEASONS.index(s) + 1] if SEASONS.index(s) < 4 else None))
pairs = q.merge(q[["player", "season", "TS%"]].rename(columns={"TS%": "TS_next", "season": "next"}), on=["player", "next"]).dropna()
print(f"{len(pairs)} player transitions from {pairs.player.nunique()} players")
y = pairs.TS_next.values; groups = pairs.player.values; gkf = GroupKFold(n_splits=min(10, pairs.player.nunique()))
fcols = ["TS%", "3PAr", "USG%", "age", "PTS_36", "min_share", "FTr", "AST_36"]
pred = {"Persistence (TS% last year)": pairs["TS%"].values,
        "League mean (train fold)":    np.concatenate([np.full(len(te), y[tr].mean()) for tr, te in gkf.split(pairs, y, groups)])[np.argsort(np.concatenate([te for _, te in gkf.split(pairs, y, groups)]))],
        "Ridge (8 features)":          cross_val_predict(make_pipeline(StandardScaler(), Ridge(alpha=10.)), pairs[fcols], y, groups=groups, cv=gkf)}
sl = smf.ols("TS_next ~ Q('TS%')", data=pairs).fit(); b = sl.params.iloc[1]
print(f"Regression-to-the-mean: next-year TS% = {sl.params.iloc[0]:.3f} + {b:.2f} × this-year TS%   (slope 95% CI {sl.conf_int().iloc[1, 0]:.2f} … {sl.conf_int().iloc[1, 1]:.2f}; R² = {sl.rsquared:.2f})")
pred["In-sample shrink line (optimistic)"] = sl.fittedvalues.values
rm = pd.DataFrame({"model": list(pred), "RMSE (TS% pts)": [100 * np.sqrt(np.mean((p - y) ** 2)) for p in pred.values()], "MAE": [100 * np.mean(np.abs(p - y)) for p in pred.values()]}).round(3); display(rm)
fig = px.scatter(pairs, x="TS%", y="TS_next", hover_name="player", color="age", color_continuous_scale="YlGn", hover_data={"season": True, "next": True}, trendline="ols",
                 labels={"TS%": "TS% season t", "TS_next": "TS% season t+1"}, title="Regression to the mean: last year's TS% vs next year's (each dot = a player transition)")
fig.add_scatter(x=[.5, .72], y=[.5, .72], mode="lines", line=dict(dash="dash", color="grey"), name="no change (y = x)"); fig.update_layout(height=480); show(fig, "38_yoy_ts")


# ## 7 · Findings, caveats & what the data can (and can't) say
# 
# ### Headline results
# | Area | Finding | Strength of evidence |
# |---|---|---|
# | **Team trajectory** | Wins 51 → 57 → **64** → 61 → 56; proxy net rating 7.3 → 6.4 → **11.4** → 9.3 → 7.9. 2023-24 was the peak on both ends. | Descriptive |
# | **Season-to-season noise** | Despite a 13-win spread, season mean margins are *statistically indistinguishable* (ANOVA p = .27, η² = .013). Binomial luck alone is ≈ ±4 wins (§5.4). | Strong (that a single record is noisy) |
# | **Luck** | 2021-22 won **7.9 fewer** games than its point differential implies (Pythagorean 58.9 vs 51 actual) — the biggest gap of the five years. | Descriptive / simulation |
# | **Home court** | Home win% 75.6% vs road 65.4% (**+10.2 pp**, 95% CI +1.5 to +19.0, raw p = .023; logistic OR ≈ 1.65). Does **not** survive Holm correction across the 14-test family, and the margin gap (+2.8 pts) is borderline (p = .065). Bayesian P(home better) = 98.8%. | Suggestive, not conclusive |
# | **Back-to-backs / rest** | No detectable effect (Δ margin −0.4, CI −4.7 … +3.8). **But** the MDE is ±5.6 pts, so moderate effects can't be ruled out. | Absence of evidence ≠ evidence of absence |
# | **Clutch / momentum** | OT record 15-16 and ≤5-pt games 37-39 are coin-flips; no streak dependence (χ², runs test, Ljung-Box all p > .15). | Moderate (low power on small samples) |
# | **First vs last 41** | Margin was +3.4 pts better in the second half (raw p = .025) — intriguing, fails multiple-testing correction. | Exploratory |
# | **Style shift** | 3PA/FGA rose .425 → .536 (2021-22 → 2024-25; z = 13.4); shot mix moved from rim (21.8% → 16.1%) to mid-range/threes (χ² p ≈ 1e-23). 3P% itself did **not** change significantly across seasons (p = .096). | Very strong |
# | **Opponent identity** | ICC ≈ 0.005 — opponent-specific effects are indistinguishable from noise with ~13 games per opponent. | Strong (given sample) |
# | **3P% "improvements"** | 2 of 26 year-over-year changes are raw-significant, **0** survive FDR. Empirical-Bayes shrinkage cut next-season 3P% prediction error by ~26% vs raw. | Strong (method) |
# | **Predicting wins (ML)** | Best out-of-sample AUC ≈ 0.57 — statistically no better than a home-only model (ΔAUC +0.03, CI −0.03 … +0.10). Margin regression R²ₒₒₛ ≈ 0. | Strong negative result |
# | **Player archetypes** | Three stable groups (K-Means vs Ward ARI = 0.74): perimeter shooters/connectors, high-usage scorers, rim-running bigs. | Moderate |
# | **Player persistence** | TS% regresses toward the mean with slope ≈ 0.75 (CI .48–1.02); Ridge only modestly beats "same as last year" (RMSE 3.5 vs 3.8 TS pts, n = 30). | Weak (small n) |
# 
# ### Why the ML "failed" — and why that is a legitimate result
# Pre-game schedule information (venue, rest, form) simply carries very little signal about NBA outcomes: the outcome variance (σ ≈ 15 pts) dwarfs any rest or form effect.
# The home-court edge is the only robust pre-game feature. Beating a bookmaker-like model needs **opponent strength** (net rating, injuries, rest of *opponent*), which these files don't contain.
# 
# ### Limitations to state in any write-up
# 1. **Observational, not experimental** — "A/B" comparisons of home/rest/tip-off are associations; opponent strength is a confounder.
# 2. **Small samples** — 410 games, 104 player-seasons, 13 players with repeat transitions. See the power analysis: effects under ≈ 4-6 points of margin are undetectable.
# 3. **Multiple comparisons** — ~15 logged tests; borderline raw results (home win%, 2nd-half margin) should be treated as hypotheses, not findings.
# 4. **Data quirks found** — home attendance is a constant 19,156; the shooting file's team-total *Avg Dist* disagrees with player rows in 4/5 seasons; zone attempts are reconstructed from rounded percentages; no opponent box scores (DRtg is a proxy). 2025-26 Tatum played only 16 games, so that season mixes very different rosters.
# 5. **Regular season only** — no playoff data.
# 
# ## 8 · Where to take it next
# * **Add opponent context** (opponent net rating, pace, injuries, rest days) from Basketball-Reference / NBA API → rebuild the win-probability model with an Elo or Bradley-Terry strength rating; this is the step most likely to lift AUC meaningfully.
# * **Game logs per player** → on/off splits, availability effects (e.g. Celtics with vs without Tatum — a genuine natural experiment).
# * **Play-by-play / tracking** → true shot-location heat maps, lineup analysis, clutch definitions by game state.
# * **Bayesian hierarchical models** (PyMC / Stan) for player skill with aging curves.
# * **Causal methods** — difference-in-differences or synthetic control for roster changes (e.g. Holiday/Porziņģis arrival in 2023-24).
