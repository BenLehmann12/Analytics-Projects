"""
celtics_utils.py
================
Helper functions for the Boston Celtics analytics project (2021-22 -> 2025-26 regular seasons).

Sections
--------
1. Data loading, cleaning & feature engineering
2. Plot theme + half-court drawing (interactive Plotly shot-zone chart)
3. Statistical-testing helpers (effect sizes, CIs, a results log with multiple-testing correction)
4. Modelling helpers (leave-one-season-out CV, target encoding)
"""
from __future__ import annotations

from pathlib import Path
from typing import Callable, Iterable

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from scipy import stats
from statsmodels.stats.multitest import multipletests
from statsmodels.stats.proportion import proportion_confint

# --------------------------------------------------------------------------------------
# 0. Brand palette
# --------------------------------------------------------------------------------------
GREEN, GOLD, BLACK, GREY, LIGHT = "#007A33", "#BA9653", "#111111", "#8A8D8F", "#F2F2F2"
RED = "#C8102E"
SEASON_COLORS = {
    "2021-22": "#8A8D8F",  # Finals run, 51 wins
    "2022-23": "#BA9653",
    "2023-24": "#007A33",  # 64 wins, title
    "2024-25": "#4FB477",
    "2025-26": "#111111",
}
ZONES = ["0-3", "3-10", "10-16", "16-3P", "3P"]
ZONE_LABELS = {
    "0-3": "At rim (0-3 ft)",
    "3-10": "Short (3-10 ft)",
    "10-16": "Mid (10-16 ft)",
    "16-3P": "Long 2 (16 ft - 3PT line)",
    "3P": "Three-pointers",
}


# --------------------------------------------------------------------------------------
# 1. DATA LOADING & FEATURE ENGINEERING
# --------------------------------------------------------------------------------------
def _season(s: pd.Series) -> pd.Series:
    """'2021-2022' -> '2021-22'."""
    return s.str.slice(0, 4) + "-" + s.str.slice(7, 9)


def _find(data_dir: Path, key: str) -> Path:
    hits = sorted(Path(data_dir).glob(f"*{key}*.csv"))
    if not hits:
        raise FileNotFoundError(f"No CSV containing '{key}' found in {data_dir}")
    return hits[0]


def load_games(data_dir) -> pd.DataFrame:
    """Game-level table (410 regular-season games) with engineered pre-game features."""
    g = pd.read_csv(_find(data_dir, "celtics_schedule_results_2021-22_to_2025-26"))
    g["season"] = _season(g["Season"])
    g["date"] = pd.to_datetime(g["Date"], format="%a %b %d %Y")
    g = g.sort_values(["date", "G"]).reset_index(drop=True)

    g["home"] = (g["Home/Away"] == "Home").astype(int)
    g["win"] = (g["Result"] == "W").astype(int)
    g["pts_for"], g["pts_against"] = g["Tm"], g["Opp"]
    g["margin"] = g["Margin"]
    g["total_pts"] = g["Tm"] + g["Opp"]
    g["ot"] = g["OT"].notna().astype(int)
    g["opponent"] = g["Opponent"]
    g["game_no"] = g["G"]

    # start time -> decimal hour (ET).  '7:30p' -> 19.5
    t = g["Start (ET)"].str.strip()
    ampm = t.str[-1].map({"a": "AM", "p": "PM"})
    dt = pd.to_datetime(t.str[:-1] + " " + ampm, format="%I:%M %p")
    g["start_hour"] = dt.dt.hour + dt.dt.minute / 60
    g["matinee"] = (g["start_hour"] < 17).astype(int)  # tips before 5pm ET

    # --- schedule / fatigue features (all known BEFORE tip-off) -----------------------
    grp = g.groupby("season", sort=False)
    g["days_rest"] = (grp["date"].diff().dt.days - 1)  # 0 = second night of a back-to-back
    g["days_rest"] = g["days_rest"].fillna(7)  # season opener: fully rested
    g["b2b"] = (g["days_rest"] == 0).astype(int)
    g["rest_cat"] = pd.cut(
        g["days_rest"], [-1, 0, 1, 2, 100], labels=["0 (B2B)", "1", "2", "3+"]
    ).astype(str)

    def games_in_window(d: pd.Series, days: int) -> pd.Series:
        out = []
        arr = d.values
        for i, di in enumerate(arr):
            out.append(int(((arr[:i] >= di - np.timedelta64(days, "D")).sum())))
        return pd.Series(out, index=d.index)

    g["games_prev_4d"] = grp["date"].transform(lambda d: games_in_window(d, 3))
    g["three_in_four"] = (g["games_prev_4d"] >= 2).astype(int)  # this game = 3rd in 4 nights

    # consecutive home / away run length (home-stand / road-trip position)
    loc_change = (g["home"] != grp["home"].shift()) | (g["game_no"] == 1)
    run_id = loc_change.cumsum()
    g["run_len"] = g.groupby(run_id).cumcount() + 1

    # rolling form (shrunk to 0 early in the season because missing games count as 0)
    for k in (5, 10):
        g[f"form{k}"] = grp["margin"].transform(
            lambda m: m.shift().rolling(k, min_periods=1).sum().fillna(0) / k
        )
    g["prev_win"] = grp["win"].shift().fillna(0).astype(int)
    g["win_pct_to_date"] = grp["win"].transform(lambda w: w.shift().expanding().mean()).fillna(0.5)

    g["month"] = g["date"].dt.month_name().str.slice(0, 3)
    g["dow"] = g["date"].dt.day_name()
    g["weekend"] = g["date"].dt.dayofweek.isin([4, 5, 6]).astype(int)
    g["half"] = np.where(g["game_no"] <= 41, "First 41", "Last 41")
    g["close"] = (g["margin"].abs() <= 5).astype(int)
    g["blowout"] = (g["margin"].abs() >= 15).astype(int)
    g["attendance"] = g["Attend."]
    g["cum_wins"] = grp["win"].cumsum()
    g["cum_margin"] = grp["margin"].cumsum()
    g["wins_vs_500"] = g["cum_wins"] - g["game_no"] / 2

    keep = [
        "season", "game_no", "date", "opponent", "home", "win", "pts_for", "pts_against", "margin",
        "total_pts", "ot", "OT", "start_hour", "matinee", "days_rest", "rest_cat", "b2b",
        "games_prev_4d", "three_in_four", "run_len", "form5", "form10", "prev_win", "win_pct_to_date",
        "month", "dow", "weekend", "half", "close", "blowout", "attendance", "cum_wins",
        "cum_margin", "wins_vs_500", "Streak",
    ]
    return g[keep].rename(columns={"OT": "ot_label"}).reset_index(drop=True)


def load_players(data_dir):
    """
    Returns (players, team_totals)
      players      : one row per player-season, per-game + totals + shooting-zone + advanced metrics
      team_totals  : the 'Team Totals' rows from the totals file (one per season)
    """
    pg = pd.read_csv(_find(data_dir, "celtics_per_game_stats_2021-22_to_2025-26"))
    tot = pd.read_csv(_find(data_dir, "celtics_total_stats_2021-22_to_2025-26"))
    sh = pd.read_csv(_find(data_dir, "celtics_shooting_stats_2021-22_to_2025-26"))
    for d in (pg, tot, sh):
        d["season"] = _season(d["Season"])
        d.drop(columns=["Season"], inplace=True)

    team_totals = tot[tot["Player"] == "Team Totals"].drop(columns=["Rk", "Age"]).reset_index(drop=True)
    pg, tot, sh = (d[d["Player"] != "Team Totals"].copy() for d in (pg, tot, sh))

    # --- merge the three sources on (season, player) ----------------------------------
    pg_cols = ["season", "Player", "Pos", "MP", "FG", "FGA", "3P", "3PA", "FT", "FTA", "ORB", "DRB",
               "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS"]
    pg_ = pg[pg_cols].rename(columns={c: f"{c}_pg" for c in pg_cols[3:]}).rename(columns={"MP_pg": "MPG"})
    sh_cols = ["season", "Player"] + [c for c in sh.columns if c not in tot.columns and c != "Pos"]
    sh_cols += ["Dunks #"] if "Dunks #" not in sh_cols else []
    sh_ = sh[[c for c in sh_cols if c in sh.columns]]
    p = tot.merge(pg_, on=["season", "Player"], how="left").merge(sh_, on=["season", "Player"], how="left")
    p = p.rename(columns={"Player": "player", "Age": "age", "Pos": "pos"})
    p = p.drop(columns=["Rk"])

    # --- advanced metrics -----------------------------------------------------------
    p["TS%"] = p["PTS"] / (2 * (p["FGA"] + 0.44 * p["FTA"]))
    p["3PAr"] = p["3PA"] / p["FGA"]
    p["FTr"] = p["FTA"] / p["FGA"]
    p["AST/TOV"] = p["AST"] / p["TOV"].replace(0, np.nan)
    for c in ["PTS", "TRB", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "FGA", "3PA", "FTA", "PF"]:
        p[f"{c}_36"] = p[c] / p["MP"] * 36
    # Hollinger Game Score (per-game average)
    p["GmSc"] = (p["PTS_pg"] + 0.4 * p["FG_pg"] - 0.7 * p["FGA_pg"] - 0.4 * (p["FTA_pg"] - p["FT_pg"])
                 + 0.7 * p["ORB_pg"] + 0.3 * p["DRB_pg"] + p["STL_pg"] + 0.7 * p["AST_pg"]
                 + 0.7 * p["BLK_pg"] - 0.4 * p["PF_pg"] - p["TOV_pg"])
    tt = team_totals.set_index("season")
    tm_mp, tm_fga, tm_fta, tm_tov = (tt.loc[p["season"], c].values for c in ["MP", "FGA", "FTA", "TOV"])
    p["USG%"] = 100 * ((p["FGA"] + 0.44 * p["FTA"] + p["TOV"]) * (tm_mp / 5)) / (
        p["MP"] * (tm_fga + 0.44 * tm_fta + tm_tov))
    p["min_share"] = p["MP"] / tm_mp * 5  # share of the five on-court slots
    p["role"] = np.where(p["GS"] / p["G"] >= 0.5, "Starter", "Bench")

    # --- reconstruct shot-zone attempts / makes from the % columns --------------------
    zone_pct = {"0-3": "%FGA 0-3", "3-10": "%FGA 3-10", "10-16": "%FGA 10-16", "16-3P": "%FGA 16-3P",
                "3P": "%FGA 3P"}
    zone_fg = {"0-3": "FG% 0-3", "3-10": "FG% 3-10", "10-16": "FG% 10-16", "16-3P": "FG% 16-3P",
               "3P": "FG% 3P"}
    frac = p[[zone_pct[z] for z in ZONES]]
    frac = frac.div(frac.sum(axis=1), axis=0)  # %FGA columns are rounded to 3 dp -> renormalise to sum to 1
    for z in ZONES:
        p[f"fga_{z}"] = (p["FGA"] * frac[zone_pct[z]]).round()
        p[f"fgm_{z}"] = (p[f"fga_{z}"] * p[zone_fg[z]].fillna(0)).round()
    return p.reset_index(drop=True), team_totals


def build_team_seasons(games: pd.DataFrame, players: pd.DataFrame, team_totals: pd.DataFrame) -> pd.DataFrame:
    """One row per season: record + efficiency proxies + shot-profile aggregates."""
    gs = games.groupby("season").agg(
        W=("win", "sum"), games=("win", "size"), ppg=("pts_for", "mean"), opp_ppg=("pts_against", "mean"),
        mov=("margin", "mean"), mov_sd=("margin", "std"), ot_games=("ot", "sum"),
        home_w=("win", lambda s: s[games.loc[s.index, "home"] == 1].sum()),
        attendance=("attendance", "mean"),
    )
    gs["L"] = gs["games"] - gs["W"]
    gs["win_pct"] = gs["W"] / gs["games"]
    tt = team_totals.set_index("season")
    out = gs.join(tt[["MP", "FG", "FGA", "3P", "3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF", "PTS"]])
    out["poss"] = out["FGA"] - out["ORB"] + out["TOV"] + 0.44 * out["FTA"]
    out["pace"] = out["poss"] / out["games"]
    out["ORtg"] = 100 * out["PTS"] / out["poss"]
    out["DRtg"] = 100 * (out["opp_ppg"] * out["games"]) / out["poss"]  # proxy: assumes equal possessions
    out["NetRtg"] = out["ORtg"] - out["DRtg"]
    out["eFG%"] = (out["FG"] + 0.5 * out["3P"]) / out["FGA"]
    out["TS%"] = out["PTS"] / (2 * (out["FGA"] + 0.44 * out["FTA"]))
    out["3P%"] = out["3P"] / out["3PA"]
    out["3PAr"] = out["3PA"] / out["FGA"]
    out["FTr"] = out["FTA"] / out["FGA"]
    out["TOV%"] = out["TOV"] / out["poss"]
    out["ORB_pg"] = out["ORB"] / out["games"]
    # pythagorean expectation (Morey exponent 13.91) -- needs total points
    pf = games.groupby("season")["pts_for"].sum()
    pa = games.groupby("season")["pts_against"].sum()
    out["pyth_win_pct"] = pf**13.91 / (pf**13.91 + pa**13.91)
    out["pyth_W"] = out["pyth_win_pct"] * out["games"]
    out["luck_W"] = out["W"] - out["pyth_W"]
    # shot-zone mix aggregated from player rows
    zone = players.groupby("season")[[f"fga_{z}" for z in ZONES] + [f"fgm_{z}" for z in ZONES]].sum()
    for z in ZONES:
        out[f"share_{z}"] = zone[f"fga_{z}"] / zone[[f"fga_{k}" for k in ZONES]].sum(axis=1)
        out[f"fgpct_{z}"] = zone[f"fgm_{z}"] / zone[f"fga_{z}"]
    ad = players.dropna(subset=["Avg Dist (ft)"])
    out["avg_shot_dist"] = ad.groupby("season").apply(lambda d: np.average(d["Avg Dist (ft)"], weights=d["FGA"]),
                                                       include_groups=False)
    return out.reset_index()


def load_all(data_dir):
    games = load_games(data_dir)
    players, team_totals = load_players(data_dir)
    team = build_team_seasons(games, players, team_totals)
    return games, players, team


# --------------------------------------------------------------------------------------
# 2. PLOT THEME & COURT
# --------------------------------------------------------------------------------------
def set_theme():
    import plotly.io as pio
    tpl = go.layout.Template(pio.templates["plotly_white"])
    tpl.layout.colorway = [GREEN, GOLD, BLACK, "#4FB477", GREY, RED, "#2E86AB"]
    tpl.layout.font = dict(family="Inter, Helvetica, Arial, sans-serif", size=13, color="#222")
    tpl.layout.title = dict(x=0.02, xanchor="left", font=dict(size=18))
    tpl.layout.hoverlabel = dict(font_size=12)
    tpl.layout.margin = dict(l=60, r=30, t=70, b=55)
    pio.templates["celtics"] = tpl
    pio.templates.default = "celtics"


HOOP = (0.0, 5.25)  # basket centre (ft) with origin at the centre of the baseline
R3, X_CORNER = 23.75, 22.0
Y_CORNER_END = HOOP[1] + np.sqrt(R3**2 - X_CORNER**2)  # y where corner line meets the arc (~14.2)


def court_shapes(line="#444", width=1.5) -> list[dict]:
    """Half-court line markings as Plotly layout shapes (units = feet)."""
    L = dict(color=line, width=width)
    S = []
    rect = lambda x0, y0, x1, y1: dict(type="rect", x0=x0, y0=y0, x1=x1, y1=y1, line=L, layer="above")
    S += [rect(-25, 0, 25, 47)]  # boundary
    S += [rect(-8, 0, 8, 19), rect(-6, 0, 6, 19)]  # paint (outer + inner key lines)
    S += [dict(type="circle", x0=-6, y0=13, x1=6, y1=25, line=L, layer="above")]  # FT circle
    S += [dict(type="circle", x0=-0.75, y0=HOOP[1] - 0.75, x1=0.75, y1=HOOP[1] + 0.75, line=dict(color="#E8590C", width=2.2))]
    S += [dict(type="line", x0=-3, y0=4, x1=3, y1=4, line=dict(color=line, width=3))]  # backboard
    S += [dict(type="path", path=f"M -4,{HOOP[1]} A 4 4 0 0 1 4,{HOOP[1]}", line=L)]  # restricted arc
    S += [dict(type="line", x0=-4, y0=4, x1=-4, y1=HOOP[1], line=L), dict(type="line", x0=4, y0=4, x1=4, y1=HOOP[1], line=L)]
    S += [dict(type="line", x0=-X_CORNER, y0=0, x1=-X_CORNER, y1=Y_CORNER_END, line=L),
          dict(type="line", x0=X_CORNER, y0=0, x1=X_CORNER, y1=Y_CORNER_END, line=L)]
    S += [dict(type="path", path=f"M {-X_CORNER},{Y_CORNER_END} A {R3} {R3} 0 0 1 {X_CORNER},{Y_CORNER_END}", line=L)]
    S += [dict(type="path", path="M -6,47 A 6 6 0 0 0 6,47", line=L)]  # half-court circle
    return S


def _clip(theta: np.ndarray) -> np.ndarray:
    """Max radius from the hoop before crossing the baseline (y=0)."""
    s = np.sin(theta)
    with np.errstate(divide="ignore"):
        return np.where(s < 0, HOOP[1] / np.abs(s), np.inf)


def _r3_boundary(theta: np.ndarray) -> np.ndarray:
    with np.errstate(divide="ignore"):
        return np.minimum(R3, X_CORNER / np.maximum(np.abs(np.cos(theta)), 1e-9))


def zone_polygons() -> dict[str, tuple[np.ndarray, np.ndarray]]:
    """x, y outlines (feet) of the five distance-based zones used by Basketball-Reference + corner 3 / above-break 3."""
    th = np.linspace(0, 2 * np.pi, 361)
    clip = _clip(th)
    hx, hy = HOOP

    def ring(r_in, r_out):
        ro = np.minimum(r_out if np.isscalar(r_out) else r_out, clip)
        ri = np.minimum(r_in, clip)
        xo, yo = hx + ro * np.cos(th), hy + ro * np.sin(th)
        ri_r = ri[::-1]
        xi, yi = hx + ri_r * np.cos(th[::-1]), hy + ri_r * np.sin(th[::-1])
        return np.r_[xo, xi], np.r_[yo, yi]

    polys = {
        "0-3": ring(0 * th, 3.0 + 0 * th),
        "3-10": ring(3.0 + 0 * th, 10.0 + 0 * th),
        "10-16": ring(10.0 + 0 * th, 16.0 + 0 * th),
        "16-3P": ring(16.0 + 0 * th, _r3_boundary(th)),
    }
    # three-point area, split into the two corners + above-the-break
    a = np.linspace(np.arccos(X_CORNER / R3), np.pi - np.arccos(X_CORNER / R3), 120)
    arc_x, arc_y = hx + R3 * np.cos(a), hy + R3 * np.sin(a)
    ytop = 47
    polys["3P-ATB"] = (np.r_[arc_x, -25, -25, 25, 25], np.r_[arc_y, Y_CORNER_END, ytop, ytop, Y_CORNER_END])
    polys["3P-CR"] = (np.array([X_CORNER, 25, 25, X_CORNER]), np.array([0, 0, Y_CORNER_END, Y_CORNER_END]))
    polys["3P-CL"] = (-polys["3P-CR"][0], polys["3P-CR"][1])
    return polys


def _zone_centroids():
    return {"0-3": (0, 5.25), "3-10": (0, 11.5), "10-16": (0, 18.2), "16-3P": (0, 27.5),
            "3P-ATB": (0, 36.5), "3P-CR": (23.5, 7.0), "3P-CL": (-23.5, 7.0)}


def team_zone_table(players: pd.DataFrame) -> pd.DataFrame:
    """Team FG% per zone per season, aggregated from player rows (benchmark for 'vs team' colouring)."""
    z = players.groupby("season")[[f"fga_{k}" for k in ZONES] + [f"fgm_{k}" for k in ZONES]].sum()
    return z


def shot_zone_figure(players: pd.DataFrame, min_minutes: int = 600) -> go.Figure:
    """
    Interactive half-court. Pick a player-season (or whole team) in the dropdown.
    Fill colour = zone FG% minus Celtics' team FG% in that zone-season (green = better, red = worse).
    Labels show FG% and share of that player's attempts.
    """
    zt = team_zone_table(players)
    team_fg = {(s, z): zt.loc[s, f"fgm_{z}"] / zt.loc[s, f"fga_{z}"] for s in zt.index for z in ZONES}
    polys, cents = zone_polygons(), _zone_centroids()

    # entries: (label, season, {zone: (fga, fgm)}, extras)
    entries = []
    for s in sorted(zt.index, reverse=True):
        entries.append((f"TEAM · {s}", s, {z: (zt.loc[s, f"fga_{z}"], zt.loc[s, f"fgm_{z}"]) for z in ZONES}, None))
    sub = players[players["MP"] >= min_minutes].sort_values(["season", "MP"], ascending=[False, False])
    for _, r in sub.iterrows():
        entries.append((f"{r['player']} · {r['season']}", r["season"],
                        {z: (r[f"fga_{z}"], r[f"fgm_{z}"]) for z in ZONES},
                        dict(c3_share=r["Corner3 %3PA"], c3_pct=r["Corner3 3P%"], fg3=r["FG% 3P"], fga3=r["fga_3P"])))

    def colour(delta):
        # diverging scale: red (-15pp) -> white -> green (+15pp)
        v = float(np.clip(delta, -0.15, 0.15) / 0.15)
        if v >= 0:
            c0, c1 = (245, 245, 245), (0, 122, 51)
        else:
            c0, c1 = (245, 245, 245), (200, 16, 46)
        w = abs(v)
        rgb = [int(a + (b - a) * w) for a, b in zip(c0, c1)]
        return f"rgb({rgb[0]},{rgb[1]},{rgb[2]})"

    fig = go.Figure()
    trace_groups = []
    for label, season, zdat, ex in entries:
        n_before = len(fig.data)
        tot_fga = sum(v[0] for v in zdat.values())
        txt_x, txt_y, txt = [], [], []
        for key, (px, py) in polys.items():
            if key.startswith("3P-"):
                zk = "3P"
            else:
                zk = key
            fga, fgm = zdat[zk]
            if key in ("3P-CR", "3P-CL", "3P-ATB") and ex is not None:
                # split three into corner / above-the-break using Corner3 columns
                c_share = 0 if np.isnan(ex["c3_share"]) else ex["c3_share"]
                c_pct = ex["c3_pct"]
                if key.startswith("3P-C"):
                    a = fga * c_share / 2
                    pct = c_pct if not np.isnan(c_pct) else np.nan
                    m = a * pct if not np.isnan(pct) else 0
                else:
                    a = fga * (1 - c_share)
                    m = fgm - (fga * c_share * (0 if np.isnan(c_pct) else c_pct))
                    pct = m / a if a > 0 else np.nan
                fga_, fgm_ = a, m
            elif key.startswith("3P-C") and ex is None:
                fga_, fgm_ = fga * 0.0, 0
                pct = np.nan
            elif key == "3P-ATB" and ex is None:
                fga_, fgm_, pct = fga, fgm, (fgm / fga if fga else np.nan)
            else:
                fga_, fgm_ = fga, fgm
                pct = fgm / fga if fga else np.nan
            ref = team_fg[(season, zk)]
            fill = colour(pct - ref) if not (pct is None or np.isnan(pct)) else "rgb(235,235,235)"
            hover = (f"<b>{label}</b><br>{ZONE_LABELS[zk]}" +
                     (" – corner" if key.startswith("3P-C") else (" – above the break" if key == "3P-ATB" else "")) +
                     f"<br>FG%: {pct:.1%}<br>vs team: {(pct - ref) * 100:+.1f} pp<br>"
                     f"Attempts: {fga_:.0f} ({fga_ / tot_fga:.1%} of FGA)" if not np.isnan(pct) else
                     f"<b>{label}</b><br>{ZONE_LABELS[zk]}<br>no attempts")
            fig.add_trace(go.Scatter(x=px, y=py, mode="lines", fill="toself", fillcolor=fill,
                                     line=dict(color="rgba(255,255,255,0.0)", width=0), hoverinfo="text",
                                     text=hover, hoveron="fills", showlegend=False, visible=False))
            if not np.isnan(pct) and fga_ / tot_fga >= 0.015:
                cx, cy = cents[key]
                txt_x.append(cx); txt_y.append(cy)
                txt.append(f"<b>{pct:.0%}</b><br><span style='font-size:10px'>{fga_ / tot_fga:.0%} of FGA</span>")
        fig.add_trace(go.Scatter(x=txt_x, y=txt_y, mode="text", text=txt, hoverinfo="skip", showlegend=False,
                                 textfont=dict(size=12, color="#111"), visible=False))
        trace_groups.append((label, n_before, len(fig.data)))

    for i in range(trace_groups[0][1], trace_groups[0][2]):
        fig.data[i].visible = True
    n = len(fig.data)
    buttons = []
    for label, a, b in trace_groups:
        vis = [False] * n
        for i in range(a, b):
            vis[i] = True
        buttons.append(dict(label=label, method="update", args=[{"visible": vis}, {"title": f"Shot profile — {label}"}]))
    fig.update_layout(
        shapes=court_shapes(), width=760, height=720, plot_bgcolor="#FAF7F0",
        title=f"Shot profile — {trace_groups[0][0]}",
        xaxis=dict(range=[-26, 26], visible=False, scaleanchor="y", constrain="domain"),
        yaxis=dict(range=[-1, 40], visible=False),
        updatemenus=[dict(buttons=buttons, direction="down", x=0.0, xanchor="left", y=1.12, yanchor="top",
                          showactive=True, bgcolor="white")],
        annotations=[dict(x=0.5, y=-0.02, xref="paper", yref="paper", showarrow=False, font=dict(size=11, color=GREY),
                          text="Colour = zone FG% minus Celtics team FG% in that zone & season (green better · red worse). "
                               "Corner-3 split only for individual players.")],
    )
    return fig


# --------------------------------------------------------------------------------------
# 3. STATISTICAL TESTING HELPERS
# --------------------------------------------------------------------------------------
def cohens_d(a, b) -> float:
    a, b = np.asarray(a, float), np.asarray(b, float)
    sp = np.sqrt(((len(a) - 1) * a.var(ddof=1) + (len(b) - 1) * b.var(ddof=1)) / (len(a) + len(b) - 2))
    return (a.mean() - b.mean()) / sp


def cohens_h(p1: float, p2: float) -> float:
    return 2 * np.arcsin(np.sqrt(p1)) - 2 * np.arcsin(np.sqrt(p2))


def bootstrap_ci(a, b=None, stat: Callable = np.mean, n_boot=10_000, alpha=0.05, seed=0):
    """Percentile bootstrap CI for stat(a) or stat(a)-stat(b)."""
    rng = np.random.default_rng(seed)
    a = np.asarray(a)
    if b is None:
        d = [stat(rng.choice(a, len(a))) for _ in range(n_boot)]
    else:
        b = np.asarray(b)
        d = [stat(rng.choice(a, len(a))) - stat(rng.choice(b, len(b))) for _ in range(n_boot)]
    return tuple(np.percentile(d, [100 * alpha / 2, 100 * (1 - alpha / 2)]))


def permutation_test(a, b, stat: Callable = np.mean, n_perm=20_000, seed=0) -> tuple[float, float]:
    """Two-sided permutation test of stat(a) - stat(b). Returns (observed, p)."""
    rng = np.random.default_rng(seed)
    a, b = np.asarray(a, float), np.asarray(b, float)
    obs = stat(a) - stat(b)
    pool, na = np.r_[a, b], len(a)
    cnt = 0
    for _ in range(n_perm):
        rng.shuffle(pool)
        cnt += abs(stat(pool[:na]) - stat(pool[na:])) >= abs(obs) - 1e-12
    return obs, (cnt + 1) / (n_perm + 1)


def two_prop_ztest(x1, n1, x2, n2):
    """Pooled two-proportion z-test. Returns z, p, diff, (lo, hi) Wald CI for the difference."""
    p1, p2 = x1 / n1, x2 / n2
    p = (x1 + x2) / (n1 + n2)
    se0 = np.sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
    z = (p1 - p2) / se0 if se0 > 0 else 0.0
    se = np.sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    d = p1 - p2
    return z, 2 * stats.norm.sf(abs(z)), d, (d - 1.96 * se, d + 1.96 * se)


def wilson(x, n, alpha=0.05):
    return proportion_confint(x, n, alpha=alpha, method="wilson")


def runs_test(binary) -> tuple[float, float]:
    """Wald-Wolfowitz runs test for randomness of a 0/1 sequence. Returns (z, p)."""
    x = np.asarray(binary).astype(int)
    n1, n0 = x.sum(), (1 - x).sum()
    runs = 1 + (x[1:] != x[:-1]).sum()
    mu = 2 * n1 * n0 / (n1 + n0) + 1
    var = (mu - 1) * (mu - 2) / (n1 + n0 - 1)
    z = (runs - mu) / np.sqrt(var)
    return z, 2 * stats.norm.sf(abs(z))


def mde_two_sample(n1: int, n2: int, sd: float, alpha=0.05, power=0.8) -> float:
    """Minimum detectable raw mean difference for a two-sample t-test (normal approx)."""
    z = stats.norm.ppf(1 - alpha / 2) + stats.norm.ppf(power)
    return z * sd * np.sqrt(1 / n1 + 1 / n2)


def mde_two_prop(n1: int, n2: int, p=0.5, alpha=0.05, power=0.8) -> float:
    z = stats.norm.ppf(1 - alpha / 2) + stats.norm.ppf(power)
    return z * np.sqrt(p * (1 - p) * (1 / n1 + 1 / n2))


class TestLog:
    """Collect every hypothesis test in one tidy table + apply multiple-comparison correction."""

    def __init__(self):
        self.rows: list[dict] = []

    def add(self, id_, question, test, p, effect=np.nan, effect_name="", ci=(np.nan, np.nan), n="", family="A/B"):
        self.rows.append(dict(id=id_, question=question, test=test, p_value=float(p), effect=effect,
                              effect_name=effect_name, ci_low=ci[0], ci_high=ci[1], n=n, family=family))

    def table(self, alpha=0.05) -> pd.DataFrame:
        df = pd.DataFrame(self.rows)
        if df.empty:
            return df
        ok = df["p_value"].notna()
        df["p_holm"], df["q_bh"] = np.nan, np.nan
        df.loc[ok, "p_holm"] = multipletests(df.loc[ok, "p_value"], method="holm")[1]
        df.loc[ok, "q_bh"] = multipletests(df.loc[ok, "p_value"], method="fdr_bh")[1]
        df["sig_raw"] = df["p_value"] < alpha
        df["sig_holm"] = df["p_holm"] < alpha
        df["sig_fdr"] = df["q_bh"] < alpha
        return df


# --------------------------------------------------------------------------------------
# 4. MODELLING HELPERS
# --------------------------------------------------------------------------------------
def shrunk_opponent_rate(train: pd.DataFrame, test: pd.DataFrame, col="margin", k=10.0):
    """
    Target-encode `opponent` using ONLY training rows: shrunken mean of `col` vs. each opponent.
    k = pseudo-observations of the global mean (larger k => more shrinkage). Returns (train_enc, test_enc).
    """
    gm = train[col].mean()
    stats_ = train.groupby("opponent")[col].agg(["sum", "count"])
    enc = (stats_["sum"] + k * gm) / (stats_["count"] + k)
    # for the TRAIN rows use leave-one-out so the encoding isn't contaminated by the row's own outcome
    s = train["opponent"].map(stats_["sum"]) - train[col]
    c = train["opponent"].map(stats_["count"]) - 1
    tr = (s + k * gm) / (c + k)
    te = test["opponent"].map(enc).fillna(gm)
    return tr.values, te.values
