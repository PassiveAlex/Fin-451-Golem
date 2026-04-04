# Fin-452-Golem: Energy Commodity Risk Management Dashboard
## Project Roadmap

---

## Project Overview

A Golem-based Shiny app (R) for a senior risk manager responsible for six CME-traded energy commodity futures markets. The app's purpose is to tell the story of individual market dynamics and cross-market relationships across forward curves, volatility, co-dynamics, seasonality, and hedge ratios.

**Markets:**
| Ticker | Name | Unit | Seasonal Driver |
|---|---|---|---|
| CL | WTI Cushing Crude | $/bbl | Refinery runs, driving season |
| BRN | Brent Crude | $/bbl | Refinery runs, global demand |
| HTT | WTI Houston Crude | $/bbl | Pipeline economics, export capacity |
| HO | Heating Oil (No. 2) | $/gal | Winter demand, distillate inventory |
| RBOB | RBOB Gasoline | $/gal | Driving season, summer blend specs |
| NG | Natural Gas (Henry Hub) | $/mmBtu | Heating/cooling degree days, storage cycles |

**Data sources:**
- `RTL::dflong`: long-format data frame with daily futures settlement prices. Contract naming: `{TICKER}{NN}` (e.g., `CL01`–`CL36`), up to 36 contracts per market, 2007–present
- FRED zero curve data provided externally by the user. `fct_fred_data.R` implements cubic spline interpolation to derive yields at arbitrary maturities

**Framework:** Golem small-r strategy (`app_ui.R` + `app_server.R`), modules for every feature (`mod_*.R`), business logic in helper functions (`fct_*.R`).

---

## What a Risk Manager Needs vs. Does Not Need

This section governs all feature prioritization and UI decisions throughout the project.

### Needs
- **Price in context, not in isolation.** Where does today's price sit in its 1-, 3-, and 5-year historical distribution? Z-scores and percentile rankings are more actionable than raw prices.
- **Curve shape as market signal.** Whether a market is in contango or backwardation reflects current supply/demand balance and directly determines the cost of maintaining futures exposure.
- **Roll cost visibility.** A hedge that costs 2% per month in roll yield is not free. Roll yield must be quantified and visible, not assumed away.
- **Hedge ratio stability over time.** A ratio that drifts requires rebalancing. Showing the time series with confidence intervals tells you whether your hedge is reliable.
- **Correlation breakdown risk.** Hedges work until they don't. The moments when CL-BRN or crude-product correlations collapse are exactly the moments risk models fail. Regime changes in correlation must be visible.
- **Seasonal patterns as baseline expectations.** NG rallies October–January. RBOB rallies February–April. A manager who doesn't know this trades against the calendar.
- **Tail risk (VaR and ES), not just average risk.** The P&L distribution in the 95th–99th percentile, not its mean and variance.
- **Basis risk quantification.** The standard deviation of the spread between the exposure and the hedge is the unhedgeable component. It must be shown alongside the hedge ratio.
- **Speed and clarity at open.** The morning briefing view must load fast and require no configuration. Defaults should reflect the most common practitioner windows (63-day rolling = one quarter).

### Does Not Need
- **Absolute price forecasts.** Out of scope. The app describes the market; it does not predict it.
- **Unannotated statistical outputs.** An R² of 0.73 or a p-value of 0.03 means nothing without context. Every number needs a label, units, and a reference range.
- **Excessive configuration.** A senior manager is not tuning 15 parameters to get a chart. Two or three key inputs (window, market, contract) cover 90% of use cases.
- **Academic model complexity for its own sake.** Rolling realized correlations communicate nearly the same information as DCC-GARCH with far less implementation risk. Add complexity only when it adds material interpretive value.
- **Decorative chart elements.** Animations, 3D surfaces, and excessive color gradients that do not encode data add noise. Charts communicate; they do not impress.
- **Intraday or tick data.** Daily settlement prices are the unit of analysis throughout.
- **Statistical tests without plain-English interpretation.** An ADF test result belongs in a research notebook, not a risk dashboard, unless it is translated to an actionable statement (e.g., "This spread shows evidence of mean reversion over the selected period").
- **Excess precision.** Crude oil to four decimal places adds no value. Round appropriately: $/bbl to 2 decimal places, NG $/mmBtu to 3.

---

## Golem File Architecture

```
R/
├── app_ui.R                  # navset_pill layout; assembles all module UIs
├── app_server.R              # reactive data load; wires all module servers; holds shared state
├── global.R                  # MARKETS metadata list, shared constants, package imports
│
├── # DATA LAYER
├── fct_data_loader.R         # load RTL::dflong; parse {TICKER}{NN} series; reshape wide + long
├── fct_fred_data.R           # cubic spline interpolation for user-provided FRED zero curves
├── fct_data_transforms.R     # log returns, unit conversion helpers, contract labeling
│
├── # FEATURE HELPERS (pure functions, no Shiny dependencies)
├── fct_forward_curve.R       # curve slope, regime classification, roll yield, animation data
├── fct_volatility.R          # rolling realized vol, EWMA vol, vol cone, Samuelson effect
├── fct_correlation.R         # rolling correlations, spread calcs, z-scores, CCF
├── fct_seasonality.R         # monthly returns, seasonal index, STL decomposition, YoY normalization
├── fct_hedge_ratios.R        # rolling OLS, hedge effectiveness, term structure, basis risk
├── fct_var.R                 # VaR (hist sim + parametric), Expected Shortfall  [Stage 6]
├── fct_roll_calendar.R       # CME roll dates, roll yield time series              [Stage 6]
├── fct_stress.R              # scenario date ranges, stress impact metrics         [Stage 6]
│
├── # MODULES (one per feature)
├── mod_market_selector.R     # shared: market checkboxes, contract slider, date range, window
├── mod_forward_curve.R       # forward curve snapshot, animated history, regime indicator
├── mod_volatility.R          # vol term structure, vol cone, EWMA, regime shading
├── mod_correlation.R         # rolling heatmap, spread z-scores, CCF — grows each stage
├── mod_seasonality.R         # monthly return distributions, STL, YoY overlay
├── mod_hedge_ratios.R        # rolling OLS beta + R², term structure, basis risk
├── mod_yield_curve.R         # FRED yield curve, 2s10s, correlation with energy returns
├── mod_market_process.R      # upstream-to-downstream production walkthrough + contract specs
├── mod_cross_market.R        # morning briefing table — all markets at a glance [Stage 6]
├── mod_var_dashboard.R       # VaR/ES surface, P&L distribution, position inputs  [Stage 6]
├── mod_roll_calendar.R       # roll schedule calendar, roll yield tracker          [Stage 6]
└── mod_stress_overlay.R      # scenario selector; emits shared date range reactive [Stage 6]

www/
└── custom.css                # brand colors, chart sizing overrides

tests/testthat/
├── test_fct_forward_curve.R
├── test_fct_volatility.R
├── test_fct_hedge_ratios.R
└── test_fct_seasonality.R
```

### `global.R` — MARKETS metadata list
All modules reference this single source of truth:
```r
MARKETS <- list(
  CL  = list(label = "WTI Cushing",    unit = "$/bbl",   color = "#1f77b4"),
  BRN = list(label = "Brent Crude",    unit = "$/bbl",   color = "#ff7f0e"),
  HTT = list(label = "WTI Houston",    unit = "$/bbl",   color = "#2ca02c"),
  HO  = list(label = "Heating Oil",    unit = "$/gal",   color = "#d62728"),
  RBOB= list(label = "RBOB Gasoline",  unit = "$/gal",   color = "#9467bd"),
  NG  = list(label = "Natural Gas",    unit = "$/mmBtu", color = "#8c564b")
)
```
Column naming convention in the data: `{MARKET}_C{N}` (e.g., `CL_C1`, `NG_C3`).

---

## Module Specifications

### `mod_market_selector.R` — Shared UI Controls
Used in every feature tab. Outputs: selected markets vector, contract range, date range, rolling window.
- Market checkbox group (disabled until the market's stage is reached)
- Contract range slider: C1 to C10
- Date range picker (default: trailing 3 years; "Full History" button for 2007–present)
- Rolling window radio buttons: 21d / 63d (default) / 126d / 252d

---

### `mod_forward_curve.R` — Term Structure
**Purpose:** Visualize the shape of the forward curve at any point in time and track how it evolves.

**Displays:**
- **Curve snapshot:** plotly line chart, x = contract (C1–C10), y = settlement price, for a user-selected date. Optional second date overlaid for comparison.
- **Animated history:** time slider to scrub through dates and watch the curve reshape.
- **Curve shape indicator:** scalar metrics updated daily — slope (C1 minus C10), carry proxy (annualized C1-C3 spread), categorical label (steep backwardation / mild backwardation / flat / mild contango / steep contango).
- **Regime history chart:** stacked bar showing fraction of trading days per year the market spent in each regime. Gives storage economics context at a glance.
- **Roll yield time series:** annualized roll yield (C1 to C2) as a percentage of spot price, plotted over time.

**Key `fct_forward_curve.R` functions:**
- `compute_curve_slope(prices_wide, market, date)` → scalar
- `classify_curve_shape(slope, thresholds)` → factor
- `compute_roll_yield(prices_wide, market)` → daily time series
- `build_curve_animation_data(prices_wide, market, date_range)` → list for plotly frames

---

### `mod_volatility.R` — Volatility Analysis
**Purpose:** Monitor realized vol across the term structure (Samuelson effect) and over time.

**Displays:**
- **Realized vol term structure:** bar chart of annualized vol for C1–C10 at a selected date. Near-month vol should exceed deferred vol — this is the Samuelson effect.
- **Vol time series:** rolling realized vol for user-selected contracts (C1, C3, C6) over the full history. EWMA vol toggle.
- **Vol regime shading:** background shading on the time series when 21d vol exceeds the trailing 1-year median.
- **Vol cone:** for each contract, the historical distribution of realized vol at different lookback horizons (10th, 25th, 50th, 75th, 90th percentile). Contextualizes whether current vol is historically elevated or suppressed.
- **Vol ratio surface:** heatmap — time on x-axis, contract on y-axis, color = ratio of near-month vol to that contract's vol. Makes the Samuelson effect visible across history.

**Key `fct_volatility.R` functions:**
- `compute_log_returns(prices_wide, market, contracts)` — vectorized
- `rolling_realized_vol(returns, window, annualize = TRUE)` — uses `slider::slide_dbl`
- `ewma_vol(returns, lambda = 0.94)` — RiskMetrics EWMA
- `vol_term_structure_snapshot(prices_wide, market, date, window)` → named vector
- `build_vol_cone(returns, horizons = c(10, 21, 63, 126, 252))` → percentile matrix

---

### `mod_correlation.R` — Co-dynamics and Spreads
**Purpose:** Understand co-movement between markets and quantify spread relationships. This module grows with each stage as markets are added.

**Displays:**
- **Rolling correlation heatmap:** full pairwise matrix across front-month prices for all active markets. Window-selectable. Color scale −1 to 1.
- **Pairwise rolling correlation chart:** select any two markets; plot their rolling correlation over time with regime breakdown.
- **Spread time series:** for each relevant spread pair, front-month spread with statistical bands (mean ± 1 and 2 SD over trailing window). Color the inter-band region.
- **Spread z-score:** plotted alongside the raw spread. Reveals mean-reversion signals.
- **Spread term structure:** for a selected pair, spread across C1–C10 at a selected date. Does the spread widen or compress along the curve?
- **Cross-correlation function (CCF):** lagged CCF (lags −20 to +20 trading days) for any pair. Identifies lead-lag relationships.

**Spread pairs by stage:**
- Stage 2: CL minus BRN (Brent-WTI differential)
- Stage 3: CL minus HTT (Midland/Houston pipeline basis)
- Stage 4: HO crack (HO×42 − CL), RBOB crack (RBOB×42 − CL), 3-2-1 crack
- Stage 5: Heat-to-gas spread (HO vs. NG, BTU-adjusted)
- Stage 6: Full cross-market matrix; crack margin tracker

**Key `fct_correlation.R` functions:**
- `rolling_correlation_matrix(returns_wide, window)` — uses `slider`
- `compute_spread(prices_wide, mkt_a, mkt_b, contract_n = 1, unit_adjust = 1)` — handles $/gal × 42
- `spread_zscore(spread_ts, window)` → z-score series
- `cross_correlation_function(x, y, max_lag = 20)` → data.frame

---

### `mod_seasonality.R` — Seasonal Patterns
**Purpose:** Identify and quantify seasonal tendencies relevant to demand cycles, storage, and roll behavior.

**Displays:**
- **Monthly return distribution:** box plots by calendar month across all years. Immediately reveals driving-season rally, winter heating demand, summer refinery run patterns.
- **Seasonal index:** average normalized price level by day-of-year with confidence band. The "typical year" shape of each market.
- **Year-over-year overlay:** one line per year (or selected years), x = day of year, y = price normalized to Jan 1 = 100. Enables visual comparison of current year vs. history.
- **STL decomposition:** trend, seasonal, and remainder components as stacked panels for the front-month price series.
- **Seasonal vol chart:** the same monthly box plot structure applied to realized vol rather than returns. NG and HO vol are strongly seasonal.
- **Multi-market seasonal index:** all active markets on one chart, normalized for comparability. Shows the contrast between NG's strong seasonality and crude's weaker pattern.

**Key `fct_seasonality.R` functions:**
- `compute_monthly_returns(prices, market, contract_n = 1)` → data.frame with month column
- `seasonal_index(prices, market, method = c("ratio_to_moving_average", "STL"))` → daily index
- `stl_decompose(prices, market, contract_n = 1, s_window = "periodic")` — wraps `stats::stl`
- `yoy_normalized_prices(prices, market, contract_n = 1)` → wide data.frame by year

---

### `mod_hedge_ratios.R` — Hedge Ratio Dynamics
**Purpose:** Quantify optimal hedge ratios, their stability over time, and residual basis risk.

**Displays:**
- **Rolling OLS hedge ratio:** for a selected exposure contract and hedge contract, plot the rolling OLS beta with confidence interval. This is the minimum-variance hedge ratio.
- **Hedge effectiveness chart:** R² from the same rolling OLS, plotted alongside beta. A hedge ratio is only useful if the hedge works; show both together.
- **Hedge ratio term structure:** at a selected date, plot the hedge ratio for hedging C1 exposure using each of C2–C10. Shows how the ratio varies along the curve.
- **Cross-market hedge ratio table:** for all active market pairs, a DT table of rolling hedge ratios and R² values. Updated per window selection.
- **Dollar hedge ratio display:** statistical hedge ratio converted to number of contracts per unit exposure (accounting for contract sizes and tick values).
- **Basis risk decomposition:** standard deviation of the OLS residual expressed as a percentage of total price risk. This is the unhedgeable component — always show it.

**Key `fct_hedge_ratios.R` functions:**
- `ols_hedge_ratio(x_returns, y_returns, window)` → data.frame: beta, r_squared, residual_sd
- `minimum_variance_hedge_ratio(sigma_x, sigma_y, rho)` — analytical formula
- `hedge_ratio_term_structure(prices_wide, market, exposure_contract = 1, window)` → vector across C2–C10
- `basis_risk_decomposition(x_returns, y_returns, hedge_ratio)` → hedged vol, unhedged vol, basis risk pct

---

### `mod_yield_curve.R` — Macro Interest Rate Context
**Purpose:** Provide the interest rate context that affects commodity carry costs and the futures-physical basis.

**Displays:**
- **Current yield curve:** bar chart of Treasury yields (3m, 6m, 1y, 2y, 5y, 10y, 30y) from FRED.
- **Yield curve history:** heatmap — time on x-axis, maturity on y-axis, yield as color. Inversion episodes are immediately visible.
- **2s10s spread time series:** classic inversion indicator with NBER recession shading.
- **Correlation with energy returns:** rolling correlation between changes in the 2s10s spread and energy front-month returns. Brief UI note explaining the carry cost channel.

---

## Market Process Tab

**Purpose:** A reference tab providing a structured walkthrough of each commodity's physical value chain — from extraction to end-user delivery — alongside CME contract specifications. Designed to onboard a manager who is new to a market without requiring external research.

Each commodity gets a sub-tab with three sections:

### 1. Production Chain
A narrative + structured flowchart showing the physical journey of the commodity:
- Source nodes (extraction/production)
- Transportation and logistics infrastructure
- Storage and hub nodes
- Downstream consumption / refining
- End-product distribution

For petroleum products: shared upstream crude source feeds into refinery, which yields HO, RBOB, and other products. This shared origin is why correlations are high and crack spreads exist.

### 2. Contract Specifications
A rendered table of CME contract terms:

| Field | CL | BRN | HTT | HO | RBOB | NG |
|---|---|---|---|---|---|---|
| Exchange | NYMEX | NYMEX* | NYMEX | NYMEX | NYMEX | NYMEX |
| Symbol | CL | BB | HCL | HO | RB | NG |
| Contract Size | 1,000 bbl | 1,000 bbl | 1,000 bbl | 42,000 gal | 42,000 gal | 10,000 mmBtu |
| Price Unit | $/bbl | $/bbl | $/bbl | $/gal | $/gal | $/mmBtu |
| Tick Size | $0.01/bbl ($10) | $0.01/bbl ($10) | $0.01/bbl ($10) | $0.0001/gal ($4.20) | $0.0001/gal ($4.20) | $0.001/mmBtu ($10) |
| Delivery | Cushing, OK | FOB North Sea | Houston, TX | New York Harbor | New York Harbor | Henry Hub, LA |
| Delivery Grade | Light sweet (API 37–42, S ≤0.42%) | Brent blend (API ~38) | Light sweet | No. 2 heating oil | Reformulated gasoline | Pipeline quality dry |
| Contract Months | All 36 months | All 36 months | All 36 months | All 18 months | All 18 months | All 12 months |
| Last Trading Day | 3rd biz day prior to 25th of month preceding delivery | 1st biz day of delivery month | 3rd biz day prior to 25th | Last biz day of month prior to delivery | Last biz day of month prior to delivery | 3rd-to-last biz day of month prior |
| Settlement | Physical delivery | Physical delivery | Physical delivery | Physical delivery | Physical delivery | Physical delivery |

*BRN (Brent) on CME is the BB contract; ICE BRN is the benchmark. The CME and ICE contracts converge at settlement.

### 3. Market Dynamics Notes
Concise bullet points covering:
- Key supply/demand drivers specific to this market
- How this market relates to others in the complex (e.g., HO and RBOB compete for refinery yields)
- What the forward curve shape typically signals for this market
- Seasonal patterns to watch
- Major price event history (e.g., Cushing storage overhang 2015-2016, Henry Hub polar vortex spikes)

### CL (WTI Cushing) — Production Chain Detail
```
Upstream Production               Transportation              Storage & Hub           Downstream
─────────────────────             ──────────────              ──────────────          ──────────────────
Permian Basin (TX/NM)  ─────┐                                                        Gulf Coast Refineries
Eagle Ford (TX)        ─────┤    Pipeline Network   →    Cushing, Oklahoma    →      Midwest Refineries
Bakken (ND/MT)         ─────┤    (Keystone, Seaway,       (Pipeline Crossroads,      → Gasoline, Diesel,
DJ/Niobrara (CO)       ─────┘    Longhorn, etc.)           ~50M bbl capacity)          Jet Fuel, HO, etc.
                                                            ↑
                                                       NYMEX CL Delivery Point
                                                       (Landlocked — determines
                                                        WTI discount to Brent)
```

### BRN (Brent) — Production Chain Detail
```
Upstream Production               Transportation              Trading Hub             Downstream
─────────────────────             ──────────────              ──────────────          ──────────────────
North Sea Fields:                                                                     European Refineries
 Brent, Forties,       ─────┐    North Sea Pipelines  →    Sullom Voe Terminal  →   Med Refineries
 Oseberg, Ekofisk      ─────┤    + FPSO vessels             (Shetland Islands)       Asian Refineries
 (BFOET blend)         ─────┘                               ↑                        (Brent = global
                                                        ICE/CME delivery basis         benchmark price)
                                                        Dated Brent → spot
```

### HTT (WTI Houston) — Production Chain Detail
```
Upstream Production               Transportation              Hub                     Export
─────────────────────             ──────────────              ──────────────          ──────────────────
Permian Basin          ─────┐    Longview / Permian   →    Houston Ship Channel →   Global export
  (same source as CL)  ─────┤    Express Pipelines         (ECHO terminal,           terminals
                       ─────┘    (Midland → Houston)        Magellan East Houston)    (VLCC loading)
                                                            ↑
                                                       HTT Delivery Point
                                                       (No landlocked discount;
                                                        HTT trades near Brent
                                                        when export arb is open)
                                CL-HTT spread = Midland
                                basis + pipeline tariff
```

### HO (Heating Oil) — Production Chain Detail
```
Crude Input                       Refinery Process            Distribution            End Users
─────────────────────             ──────────────              ──────────────          ──────────────────
WTI / Brent / Mars     ─────┐                               Barges / Trucks    →    Residential heating
  (heavy/sour or        ─────┤    Atmospheric     →    NY Harbor (delivery)          (Northeast US)
   light/sweet crude)   ─────┘    Distillation              Colonial Pipeline    →   Industrial boilers
                                  ↓                          (Southeast/Midwest)      Diesel (ultra-low
                                Heating oil /                                          sulfur variant)
                                Diesel cut                                            Marine fuel
                                (middle distillate)
```

### RBOB (Reformulated Gasoline) — Production Chain Detail
```
Crude Input                       Refinery Process            Blending                End Users
─────────────────────             ──────────────              ──────────────          ──────────────────
Light sweet crude      ─────┐    Fluid Catalytic    →    RBOB (blendstock)   →      Branded gasoline
  (gasoline yield       ─────┤    Cracking (FCC)          + 10% ethanol              (after ethanol add)
   ~ 45-50% of barrel)  ─────┘    ↓                        at terminal                → Retail pump
                                Reformer (octane           NY Harbor delivery
                                  upgrading)               ↑
                                RBOB is blendstock;        CME RB delivery point
                                ethanol added at
                                  terminal (not traded)
```

### NG (Natural Gas) — Production Chain Detail
```
Upstream Production               Gathering                   Processing              Transmission / End Use
─────────────────────             ──────────────              ──────────────          ──────────────────
Marcellus/Utica (PA/WV) ───┐                                                         Utilities (power gen)
Haynesville (LA/TX)    ───┤    Low-pressure        →    Gas Processing Plant →       Industrial users
Permian associated gas  ───┤    gathering lines           (removes NGLs:              LNG export terminals
Appalachia             ───┘    to central point           ethane, propane,            Storage fields
                                                           butane, etc.)              (seasonal injection/
                               ↓                          ↓                            withdrawal)
                          Henry Hub, LA               Dry pipeline-quality            ↑
                          (CME NG delivery point)      natural gas                 Henry Hub = US
                          Interconnection of 9+                                     benchmark spot
                          interstate pipelines
```

### Implementation Notes for `mod_market_process.R`
- Use `bslib::navset_tab()` within the tab — one sub-tab per market
- Production chains rendered as styled HTML (`div`/`pre` blocks with monospace font, or `grViz` from `DiagrammeR` if added as a dependency)
- Contract specs rendered as `DT::datatable()` or `gt::gt()` for readability
- Market dynamics notes rendered as `shiny::HTML()` or `shiny::markdown()`
- CL is implemented in Stage 1; remaining markets added in their respective stages
- No reactive computation in this module — entirely static reference content

---

## Enrichment Features (Stage 6)

### 1. Cross-Market Morning Briefing (`mod_cross_market.R`)
A single-screen DT table showing all six markets simultaneously: front-month price, 1-day return (green/red), 5-day return, 21d realized vol, vol deviation from 1-year median, curve slope category, current roll yield. Color-coded with `DT::formatStyle`.

**Why:** Senior managers need a weather-map view before diving into detail. This tells them which markets are in unusual states today and where to focus. No existing module provides cross-market context on one screen.

### 2. Value at Risk / Expected Shortfall (`mod_var_dashboard.R`)
Historical simulation VaR and ES for individual markets and a user-defined portfolio (enter contract quantities per market). VaR term structure showing tail exposure across C1–C10. Parametric VaR as a cross-check.

**Why:** VaR is the universal language of risk limits. A manager needs tail exposure at the 95th and 99th percentile, not just average risk. The term structure VaR surface reveals concentration risk that a single-number VaR misses. ES matters because commodity returns are fat-tailed — the average loss given a breach is the number that defines capital at risk.

### 3. Roll Calendar and Roll Yield Tracker (`mod_roll_calendar.R`)
Visual calendar of first notice dates and last trading days for all six markets. Roll yield time series showing the annualized cost/benefit of rolling C1 to C2 historically. Current roll yield dashboard.

**Why:** Rolling futures is not free. In steep contango (common in NG and crude during storage overhangs), rolling costs 1–3% per month — material over a year for any hedger maintaining front-month exposure. The roll calendar also prevents the operational risk of holding into delivery.

### 4. Historical Stress Scenario Overlays (`mod_stress_overlay.R`)
Named scenario library: 2008 financial crisis, 2014–2016 crude supply glut, 2020 COVID demand collapse, 2022 Russia-Ukraine shock. When a scenario is selected, all charts shade that date range. A scenario impact panel shows max drawdown, vol spike, and spread dislocation during that window for each market.

**Why:** Stress testing is standard risk management practice. A manager needs to know not just current risk but what plausible tail scenarios look like — specifically, how hedges and spreads behaved the last time something like this occurred.

**Architecture note:** The selected scenario emits a shared reactive date range in `app_server.R` passed to all modules. It is not internal state inside this module.

### 5. Crack Spread Margin Tracker (enhancement to `mod_correlation.R`)
Dedicated sub-panel for the 3-2-1 crack spread with seasonal norm overlaid, z-score, and historical distribution. Shows correlation between crack spread and crude price.

**Why:** Refiners are the natural buyers of crude. The crack spread is their gross margin. Whether the crack is wide, narrow, or collapsing is the most important cross-commodity relationship in the petroleum complex — it determines demand for crude and directly affects the relative value of CL, HO, and RBOB positions.

### 6. Realized vs. Implied Vol Panel (enhancement to `mod_volatility.R`)
If CME options settlement data is provided, plot realized vol alongside ATM implied vol. Show the vol risk premium (implied minus realized) over time and by market. If options data is not available, this panel renders as a labeled placeholder that accepts a CSV upload.

**Why:** Options are a core hedging tool. The vol risk premium — whether buying protection is cheap or expensive relative to history — determines the cost-effectiveness of options strategies. A manager buying puts when implied vol is 2 standard deviations above realized is systematically overpaying.

---

## Package Dependencies

### Already present (from existing codebase patterns)
`shiny`, `golem`, `bslib`, `plotly`, `tidyquant`, `dplyr`, `tidyr`, `ggplot2`, `lubridate`, `DT`, `shinyjs`

### Additional packages required

| Package | Purpose |
|---|---|
| `slider` | Rolling window computations — more flexible than `zoo::rollmean` for arbitrary functions and irregular time series |
| `zoo` | `na.locf` for forward-filling missing prices; time series coercion |
| `roll` | Vectorized rolling OLS via `roll_lm` — 10–50× faster than a `slider` loop over `lm` for hedge ratio calculations |
| `TTR` | `EMA`, `BBands` for vol overlay options; well-tested financial rolling functions |
| `xts` | Required by several financial packages; fast date-based subsetting |
| `scales` | `dollar_format`, `percent_format` for chart axis labels |
| `forcats` | Month factor ordering in seasonal plots |
| `glue` | String interpolation for chart titles and annotations |
| `purrr` | `map` over market lists in multi-market computations |
| `PerformanceAnalytics` | Battle-tested ES/VaR implementations for Stage 6 |

---

## Reactivity Architecture

### Data flow (established in Stage 1, never broken)
```
Pre-provided dataframe (static file)
    └── fct_data_loader.R
            ├── prices_wide  ──┐
            ├── prices_long    ├── reactive objects in app_server.R
            └── returns_wide ──┘    passed as arguments to every module server

FRED API
    └── fct_fred_data.R
            └── yield_data ──── reactive, refreshes daily
```

### Rules
1. The main data load uses `eventReactive` tied to an Update button — not reactive to every input change. Prevents expensive reloads on slider drags.
2. Within modules, heavy chart renders use `bindEvent`. Module server functions receive reactive objects as arguments; they do not access parent-scope variables directly.
3. Rolling computations over the full 2007–present history are pre-computed on the reactive update event and stored as reactive values — not recomputed inside render functions.
4. The stress scenario date range is a shared `reactiveVal` in `app_server.R` passed to every module that renders charts. Modules use it to add `plotly` shape annotations.

### Module call pattern
```r
# app_server.R
mod_volatility_server("vol", prices = prices_reactive, returns = returns_reactive, stress = stress_dates)
```
Each module's server function signature: `function(id, prices, returns, stress = NULL)`.

---

## Stage-by-Stage Build Plan

### Stage 1: CL — Full Feature Set and Module Infrastructure
> Largest stage. Every reusable pattern is established here. Do not shortcut.

- [x] `golem::create_golem()` scaffolding, `DESCRIPTION`, `bslib` theme in `app_ui.R`
- [x] `global.R`: `MARKETS` list, shared constants, all package imports
- [x] `fct_data_loader.R`: load `RTL::dflong`, parse `{TICKER}{NN}` series, reshape wide/long, compute log returns
- [x] `mod_market_selector.R`: all inputs defined; all 6 markets enabled
- [x] `fct_forward_curve.R` + `mod_forward_curve.R`: snapshot, regime classifier, regime history, roll yield, forward curve fan chart (C1 over time + sampled forward curves)
- [x] `fct_volatility.R` + `mod_volatility.R`: vol term structure, vol cone, EWMA, regime shading
- [x] `fct_seasonality.R` + `mod_seasonality.R`: monthly returns, seasonal index, STL, YoY overlay
- [x] `fct_hedge_ratios.R` + `mod_hedge_ratios.R`: rolling OLS, effectiveness, term structure, basis risk
- [x] `fct_fred_data.R`: `build_zero_curve_spline`, `interpolate_zero_yield`, `discount_factors`, `get_date_spline`, `interpolated_term_structure`, `forward_rates`, `par_yield`; bug fix: robustness to `tq_get` returning NULL or wrong column names
- [x] `mod_yield_curve.R`: zero curve chart, forward rates overlay, 2s10s spread
- [x] `mod_market_process.R`: CL production chain, all 6 market contract specs, market dynamics notes
- [x] `app_ui.R`: `page_navbar` — Market Process, Forward Curve, Volatility, Seasonality, Hedge Ratios, Yield Curve, Correlation
- [x] `app_server.R`: reactive data load via `build_market_data()`, `zero_curve_panel` reactiveVal, all modules wired
- [ ] `tests/testthat/`: `testthat` unit tests for all `fct_*.R` functions using CL data

**Deliverable:** Fully functional single-market CL dashboard. A risk manager could use it in production for CL.

---

### Stage 2: BRN (Brent Crude) — Location Spread Introduction

- [x] Enable BRN in `global.R` (`enabled = TRUE`); columns validated via `fct_data_loader.R` pattern match
- [ ] Multi-market forward curve: `mod_forward_curve.R` accepts a vector of markets; market-specific colors from `MARKETS` list
- [ ] Multi-market vol: CL and BRN vol time series on the same chart with market toggle
- [x] **`mod_correlation.R`**: rolling correlation heatmap (N×N), pairwise rolling correlation, spread time series, spread z-score; all markets; note: correlation logic is inline in the module rather than split into a separate `fct_correlation.R`; bug fix: spread column lookup now uses correct `{PREFIX}_C1` naming
- [ ] Cross-market hedge ratio section in `mod_hedge_ratios.R`: rolling CL/BRN beta and effectiveness
- [x] Correlation tab activated in `app_ui.R` and wired in `app_server.R`

**Deliverable:** Two-market dashboard. CL-BRN differential is the primary new analytical object.

---

### Stage 3: HTT (WTI Houston) — Pipeline Basis

- [x] Enable HTT in `global.R`; columns validated
- [ ] Three-way curve comparison with "spread mode" toggle (y-axis switches to spread vs. CL)
- [ ] CL-HTT basis sub-panel in `mod_correlation.R`: dedicated panel with historical distribution and z-score
- [x] N×N correlation heatmap active (all enabled markets)
- [x] HTT available in market selector for seasonality, volatility, hedge ratio tabs

**Deliverable:** Three crude markets. Fundamental crude basis relationships visible.

---

### Stage 4: HO + RBOB — Crack Spread Analysis
> Unit conversion introduced here. HO and RBOB are in $/gal; all spread calculations involving crude require ×42. The `bbl_factor` field in `MARKETS` handles this in `spread_prices()`.

- [x] Enable HO and RBOB in `global.R`; $/gal unit registered; `bbl_factor = 42` set
- [ ] Unit-aware forward curve: toggle for native units vs. barrel-equivalent (default: barrel-equivalent)
- [ ] Crack spread suite in `mod_correlation.R`: dedicated HO crack, RBOB crack, 3-2-1 crack panels with seasonal norm overlaid — current spread/zscore tabs support any pair but lack dedicated crack UI
- [ ] Crack seasonality sub-panel: RBOB crack is strongly seasonal (spring driving season)
- [ ] Hedge ratio for refinery margin position in `mod_hedge_ratios.R`

**Deliverable:** Full petroleum complex. Refinery margin analysis available.

---

### Stage 5: NG — Natural Gas Unique Dynamics
> NG has materially different dynamics: highest vol, strongest seasonality, camel-hump curve shape, different spread logic. Do not treat it as just another commodity.

- [x] Enable NG in `global.R`; $/mmBtu unit registered; `bbl_factor = 5.8` set
- [ ] NG-specific forward curve features: "seasonal shape" overlay highlighting winter premium; summer-winter spread metric (C1 vs. C7 during summer) replacing the simple slope metric
- [ ] NG seasonality sub-panel: strongest seasonal signal of all six markets; STL seasonal component will be prominent
- [ ] Vol spike detection in `mod_volatility.R`: flag days where 5-day realized vol exceeds trailing 1-year 95th percentile; mark as points on the vol chart
- [ ] Heat-to-gas spread in `mod_correlation.R`: HO vs. NG on BTU-adjusted basis; label as "fuel switching spread"
- [ ] Multi-market seasonal index chart: all six markets normalized on one chart; contrast between NG strength and crude weakness is the key story

**Deliverable:** All six markets active. Full cross-commodity dashboard possible.

---

### Stage 6: Enrichment Features

- [ ] `fct_var.R` + `mod_var_dashboard.R`: historical simulation VaR/ES; parametric cross-check; portfolio VaR with position input; VaR term structure surface
- [ ] `fct_roll_calendar.R` + `mod_roll_calendar.R`: CME first notice and last trading dates for all six markets; monthly calendar heatmap; roll yield tracker
- [ ] `fct_stress.R` + `mod_stress_overlay.R`: hardcode named scenario date ranges; shared `reactiveVal` in `app_server.R`; scenario impact summary panel; all chart modules receive and apply the stress date range
- [ ] `mod_cross_market.R`: DT morning briefing table — price, daily return, 5-day return, 21d vol, vol deviation, curve regime, roll yield; `formatStyle` color coding
- [ ] Crack margin tracker sub-panel in `mod_correlation.R`: 3-2-1 crack with seasonal norm, z-score, distribution
- [ ] Realized vs. implied vol stub in `mod_volatility.R`: placeholder if options data not provided; CSV upload activates the panel
- [ ] Performance audit: review reactive graph; wrap any computation that re-runs unnecessarily in `bindEvent`; consider caching full rolling correlation matrices as reactive values

**Deliverable:** Production-grade risk management platform across all six markets.

---

## UI/UX Guidelines

- **Default date range:** trailing 3 years. "Full History" button for 2007–present. Most day-to-day risk work uses 1–3 year windows.
- **Default rolling window:** 63 trading days (one calendar quarter) — the most common window in energy risk reporting.
- **Y-axis:** do not force zero as the baseline for price charts. Commodity prices are never zero; zero baseline wastes chart real estate.
- **Hover tooltips:** date, value, and where appropriate the z-score or percentile rank of that value.
- **Color conventions:** blue = primary series, orange = comparison series, green = backwardation regime, red/orange = contango regime, grey = historical reference.
- **Chart downloads:** plotly's built-in camera icon is sufficient. No custom export infrastructure needed.
- **Mobile:** not a priority. This dashboard is used at a desktop workstation.
