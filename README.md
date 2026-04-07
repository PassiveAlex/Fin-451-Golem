# Energy Risk Dashboard

A Shiny dashboard for energy commodity risk managers, built with the [Golem](https://thinkr-open.github.io/golem/) framework. Covers WTI (Cushing + Houston), Brent Crude, Heating Oil, RBOB Gasoline, and Natural Gas futures traded on the CME.

---

## What it does

The dashboard gives a risk manager a single place to monitor market structure, volatility, co-movement, and hedge effectiveness across the energy complex — without having to pull data manually or maintain spreadsheets.

**Tabs:**

| Tab | What it shows |
|---|---|
| **Market Process** | Price series, log-returns, and basic distributional stats for any contract |
| **Forward Curve** | Full futures strip (C1–C36) with contango/backwardation classification and optional EIA inventory overlay |
| **Volatility** | Rolling historical vol (configurable window), vol term structure across contracts, and GARCH-style regime context |
| **Seasonality** | Average seasonal return patterns by month/quarter, useful for identifying calendar spreads and refinery margin seasonality |
| **Hedge Ratios** | Rolling OLS minimum-variance hedge ratios between contracts or across markets, with confidence intervals, R², and basis risk decomposition |
| **Yield Curve** | U.S. Treasury zero curve (1M–30Y) with cubic spline interpolation, par yields, and implied forward rates — relevant for carry-adjusted positions |
| **Correlations** | Rolling correlation matrix across the full market set, useful for cross-commodity hedging and portfolio construction |
| **EIA Fundamentals** | Weekly inventory, production, consumption, and import/export data by PADD region, plotted with historical comparison |

---

## Data sources

| Source | What it provides | Update cadence |
|---|---|---|
| **[RTL](https://github.com/risktoollib/RTL)** | CME futures price history (all markets, all contracts) | Live / on-load |
| **[EIA API](https://www.eia.gov/opendata/)** | U.S. petroleum and natural gas fundamentals | Nightly (cached feather file) |
| **[FRED](https://fred.stlouisfed.org/)** (via St. Louis Fed CSV endpoint) | U.S. Treasury Constant Maturity yields (DGS series, H.15) | Nightly (cached feather file) |

EIA and FRED data are pre-fetched nightly by GitHub Actions and bundled as Apache Arrow feather files so the app starts instantly without waiting on API calls at runtime.

---

## Running it

### Docker (recommended)

```bash
docker pull ghcr.io/<owner>/fin452golem:latest
docker run -p 3838:3838 -e EIA_API_KEY=your_key ghcr.io/<owner>/fin452golem:latest
```

Open `http://localhost:3838`.

### Local (R)

```r
# Install dependencies
remotes::install_deps()

# Run
fin452golem::run_app()
```

Requires an `EIA_API_KEY` in a `.env` file or environment variable if you want live EIA data fetched. The bundled feather cache works without it.

---

## Practical use cases

- **Curve structure monitoring** — quickly see whether the strip is in backwardation or contango and by how much, across all markets simultaneously.
- **Rolling hedge ratio calibration** — determine how many contracts of C3 or C6 are needed to hedge a C1 position, and track how stable that ratio has been. Cross-market hedges (e.g. WTI vs. Brent, or RBOB vs. HO) are supported.
- **Basis risk assessment** — the hedge ratio tab shows the fraction of exposure variance that cannot be hedged away, helping size residual risk reserves.
- **Volatility regime awareness** — before entering or unwinding a position, check where realized vol sits relative to its own history and how the vol term structure is shaped.
- **Inventory-price relationship** — overlay EIA stock draws/builds on the forward curve to contextualize current curve shape with supply fundamentals.
- **Calendar spread seasonality** — historical seasonal patterns inform entry timing on calendar spread trades (e.g. crude summer driving demand, nat gas winter storage injection/withdrawal cycles).
- **Discount rate context** — the yield curve tab provides the risk-free term structure for mark-to-model valuations and carry calculations on longer-dated strips.
