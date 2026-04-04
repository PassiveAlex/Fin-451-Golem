# mod_market_process.R
# Static reference tab: upstream-to-downstream production chains + CME contract specs.
# No reactive computation — purely informational content.
# Stage 1: CL content implemented. Other markets added in their respective stages.

# ── Contract specification tables ────────────────────────────────────────────

.contract_specs <- list(

  CL = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Last Trading Day", "First Notice Day",
               "Delivery Location", "Delivery Grade",
               "Settlement Method"),
    Value  = c("NYMEX (CME Group)", "CL",
               "1,000 U.S. barrels (42,000 gallons)",
               "U.S. dollars and cents per barrel",
               "$0.01 / barrel", "$10.00 per contract",
               "All months, out to 36+ months",
               "3rd business day prior to the 25th calendar day of the month preceding the delivery month",
               "First business day following the last trading day of the prior contract",
               "Cushing, Oklahoma",
               "Light sweet crude: API gravity 37°–42°, sulfur ≤ 0.42% by weight",
               "Physical delivery (pipeline injection or tank delivery)"),
    stringsAsFactors = FALSE
  ),

  BRN = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Last Trading Day", "Delivery Location", "Settlement Method"),
    Value  = c("NYMEX (CME Group) — see also ICE Brent", "BB",
               "1,000 barrels", "U.S. dollars and cents per barrel",
               "$0.01 / barrel", "$10.00 per contract",
               "All months, out to 36+ months",
               "1st business day of the delivery month",
               "FOB Sullom Voe, Shetland Islands (EFP basis)",
               "Physical delivery via exchange for physical (EFP)"),
    stringsAsFactors = FALSE
  ),

  HTT = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Delivery Location", "Settlement Method"),
    Value  = c("NYMEX (CME Group)", "HCL",
               "1,000 barrels", "U.S. dollars and cents per barrel",
               "$0.01 / barrel", "$10.00 per contract",
               "All months, out to 36+ months",
               "Houston, Texas (ECHO terminal / Magellan East Houston)",
               "Physical delivery"),
    stringsAsFactors = FALSE
  ),

  HO = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Last Trading Day", "Delivery Location",
               "Delivery Grade", "Settlement Method"),
    Value  = c("NYMEX (CME Group)", "HO",
               "42,000 U.S. gallons (1,000 barrels equivalent)",
               "U.S. dollars and cents per gallon",
               "$0.0001 / gallon", "$4.20 per contract",
               "All months out to 18 months",
               "Last business day of the month prior to the delivery month",
               "New York Harbor (NYH)",
               "No. 2 heating oil / ultra-low sulfur diesel",
               "Physical delivery (barge/pipeline)"),
    stringsAsFactors = FALSE
  ),

  RBOB = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Last Trading Day", "Delivery Location",
               "Note", "Settlement Method"),
    Value  = c("NYMEX (CME Group)", "RB",
               "42,000 U.S. gallons (1,000 barrels equivalent)",
               "U.S. dollars and cents per gallon",
               "$0.0001 / gallon", "$4.20 per contract",
               "All months out to 18 months",
               "Last business day of the month prior to the delivery month",
               "New York Harbor (NYH)",
               "RBOB = Reformulated Gasoline Blendstock for Oxygenate Blending. Ethanol is NOT included — it is blended at the terminal. Traded price does not reflect ethanol component.",
               "Physical delivery"),
    stringsAsFactors = FALSE
  ),

  NG = data.frame(
    Field  = c("Exchange", "Symbol", "Contract Size", "Price Quotation",
               "Min Tick", "Tick Value", "Contract Months",
               "Last Trading Day", "Delivery Location",
               "Delivery Grade", "Settlement Method"),
    Value  = c("NYMEX (CME Group)", "NG",
               "10,000 MMBtu", "U.S. dollars and cents per MMBtu",
               "$0.001 / MMBtu", "$10.00 per contract",
               "All months out to 12 years",
               "3rd-to-last business day of the month prior to the delivery month",
               "Henry Hub, Louisiana (Sabine Pipe Line LLC)",
               "Pipeline-quality natural gas (BTU content, heating value standards per tariff)",
               "Physical delivery"),
    stringsAsFactors = FALSE
  )
)

# ── Production chain HTML content ────────────────────────────────────────────

.prod_chains <- list(

  CL = list(
    title    = "WTI Cushing (CL) — U.S. Crude Oil Value Chain",
    chain    = '
UPSTREAM PRODUCTION          GATHERING & TRANSPORT         STORAGE HUB              DOWNSTREAM
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
Permian Basin (TX/NM) ──┐                                                           Gulf Coast Refineries
Eagle Ford (TX)       ──┤   Low-pressure gathering   →   Cushing, Oklahoma    →    (Motiva, ExxonMobil,
Bakken (ND/MT)        ──┤   Pipeline gathering to          "Pipeline Crossroads"     Valero, Marathon)
DJ/Niobrara (CO)      ──┘   major trunk lines              ~75 MMbbl capacity       ↓
                                ↓                          ↑                         Gasoline (CL-RBOB crack)
                       Midstream Pipelines:           NYMEX CL Delivery Point        Diesel / ULSD
                       Keystone (TransCanada)         Pipelines intersecting:        Jet Fuel
                       Seaway (Enbridge/ETP)          Cushing include Seaway,        Heating Oil (HO)
                       Longhorn (Magellan)             Keystone, Cushing-to-          Asphalt, Residuals
                       West Texas Gulf (PBF)          Patoka, etc.
                                                           ↓
                                                      Export via pipeline to
                                                      Gulf Coast ports (for
                                                      WTI Houston = HTT)
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Cushing storage:</strong> WTI price is heavily influenced by crude
          inventory at Cushing, Oklahoma. High storage → contango; low storage →
          backwardation. Weekly EIA storage reports (Wednesday) move the market.</li>
        <li><strong>Landlocked discount:</strong> CL trades at a discount to Brent (BRN)
          because Cushing is inland. The CL-BRN spread reflects the cost/economics of
          moving crude from the U.S. midcontinent to global markets. When U.S. export
          capacity tightens, this spread widens.</li>
        <li><strong>Shale production cycle:</strong> U.S. tight oil (Permian, Bakken)
          responds to price with a 3–6 month lag. Production growth → storage builds →
          contango. Production cuts → draws → backwardation.</li>
        <li><strong>Seasonality:</strong> Demand peaks twice annually: summer driving
          season (gasoline) and winter heating season (distillates). Refinery turnarounds
          in spring and fall temporarily reduce crude demand.</li>
        <li><strong>Notable events:</strong> 2014–2016 supply glut (shale boom), negative
          prices April 2020 (COVID + storage full), 2022 supply shock (Russia-Ukraine).</li>
      </ul>
    ')
  ),

  BRN = list(
    title    = "Brent Crude (BRN) — Global Benchmark",
    chain    = '
UPSTREAM (North Sea)         LOADING TERMINAL              WATERBORNE TRADE         DOWNSTREAM
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
Brent Field           ──┐                                                           European Refineries
Forties Field         ──┤   North Sea Pipeline      →   Sullom Voe Terminal   →    (Rotterdam, ARA)
Oseberg Field         ──┤   + FPSO (floating          (Shetland Islands)            Mediterranean
Ekofisk Field         ──┘   production storage)      "Dated Brent" = physical       Asian buyers (VLCC)
(BFOET blend)                                          spot benchmark               ↓
                                                       ICE/CME futures settle        80% of global crude
                                                       against Dated Brent           trade uses Brent
                                                                                      as the benchmark
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Global benchmark:</strong> Brent prices approximately 80% of
          the world\'s traded crude. It is the reference for Middle Eastern, African,
          and other international crudes (via differentials to Dated Brent).</li>
        <li><strong>CL-BRN spread:</strong> Historically Brent traded at a discount to
          WTI; since 2011 the relationship inverted as U.S. shale output grew and
          Cushing filled. The spread reflects U.S. export economics and relative
          supply/demand imbalances across the Atlantic Basin.</li>
        <li><strong>OPEC+ influence:</strong> Saudi Arabia, Russia, and OPEC+ production
          decisions primarily affect Brent pricing. Watch OPEC meetings and compliance
          data.</li>
      </ul>
    ')
  ),

  HTT = list(
    title    = "WTI Houston (HTT) — Export-Linked Crude",
    chain    = '
UPSTREAM (Permian)           PIPELINE TO COAST             HOUSTON HUB              EXPORT / REFINERY
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
Permian Basin         ──┐                                                           VLCC Loading:
  (same crude as CL)  ──┤   Permian Express       →   ECHO Terminal         →      Enterprise Beaumont
                      ──┘   Longview Pipeline           Magellan East Houston        Targa Export (Galena Park)
                            BridgeTex                   Seaway Twin                  Phillips 66 Beaumont
                            Cactus I & II               ↑                            ↓
                            Gray Oak                  HTT Delivery Point             Asia / Europe
                                                                                      (avoids Cushing
                                                                                       landlocked discount)
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Export parity:</strong> HTT trades close to Brent when the U.S.
          crude export arb is open, reflecting Houston\'s direct waterborne access.
          CL-HTT basis narrows when export capacity is ample; widens when pipelines
          are full or export terminals are congested.</li>
        <li><strong>Pipeline basis:</strong> The spread between CL and HTT reflects
          Midland-to-Houston pipeline tariffs plus supply/demand at Houston Ship Channel.
          Permian production growth pressures this basis periodically.</li>
        <li><strong>Post-2015:</strong> U.S. crude export ban lifted December 2015.
          Since then HTT has grown in importance as a global pricing hub.</li>
      </ul>
    ')
  ),

  HO = list(
    title    = "Heating Oil (HO) — Middle Distillate",
    chain    = '
CRUDE INPUT                  REFINERY PROCESS              DISTRIBUTION             END USERS
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
WTI / Brent / Mars    ──┐   Atmospheric Distillation  →  New York Harbor (NYH) →  Residential heating
  (various crudes     ──┤   ↓                              Barges, pipelines         (Northeast U.S.)
   at Gulf Coast /    ──┘   Middle distillate cut          Colonial Pipeline     →  Industrial boilers
   East Coast refs)          (HO / diesel / jet)           Buckeye Pipeline          Diesel (ULSD variant)
                             ↓                                                        Marine diesel fuel
                             Hydrotreating (sulfur
                             removal for ULSD)
                             ↓
                             NYH NYMEX delivery
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Winter demand:</strong> The dominant seasonal driver. Northeast U.S.
          heating oil demand surges November–February. Cold snaps cause price spikes
          disproportionate to the physical volume required.</li>
        <li><strong>Distillate competition:</strong> HO competes with diesel (ULSD) and
          jet fuel for refinery yield allocation. Tight distillate supply benefits all
          three; a refinery can shift yield between them within limits.</li>
        <li><strong>Crack spread:</strong> HO crack = (HO × 42) − CL. This is the
          refinery gross margin for distillate. When it widens, refiners run harder
          to capture the margin, eventually rebuilding supply and compressing the crack.</li>
      </ul>
    ')
  ),

  RBOB = list(
    title    = "RBOB Gasoline (RB) — Motor Fuel Blendstock",
    chain    = '
CRUDE INPUT                  REFINERY PROCESS              BLENDING / DISTRIBUTION  END USERS
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
Light sweet crude     ──┐   Fluid Catalytic        →   RBOB (blendstock)    →     Terminal blending:
  (gasoline yield ~   ──┤   Cracking (FCC)             + 10% ethanol               RBOB + ethanol
   45–50% of barrel)  ──┘   Reformer (octane             (added at terminal,        = finished gasoline
                             upgrading)                   NOT at refinery or        ↓
                             Alkylation unit              in the futures)            Retail pump price
                             ↓                            New York Harbor           Branded fuel supply
                             RBOB: blendstock only        delivery point             agreements
                             (must add ethanol
                              before retail sale)
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Driving season:</strong> RBOB demand peaks April–September (summer
          driving). EPA RVP (Reid Vapour Pressure) specifications shift from winter to
          summer blend in spring, tightening supply and lifting RBOB above HO
          seasonally.</li>
        <li><strong>Ethanol mandate:</strong> The RFS (Renewable Fuel Standard) mandates
          a minimum volume of ethanol. The ethanol RIN (renewable identification number)
          cost is a meaningful input into total fuel cost but is NOT in the futures price.</li>
        <li><strong>Crack spread:</strong> RBOB crack = (RB × 42) − CL. Follows a
          pronounced seasonal pattern: narrows in autumn/winter, widens into driving
          season.</li>
      </ul>
    ')
  ),

  NG = list(
    title    = "Natural Gas (NG) — Henry Hub",
    chain    = '
UPSTREAM PRODUCTION          GATHERING & PROCESSING        TRANSMISSION             END USERS
──────────────────────       ───────────────────────       ─────────────────────    ──────────────────────
Marcellus/Utica (PA/WV) ─┐  Low-pressure gathering  →   Interstate Pipelines  →   Electric Utilities
Haynesville (LA/TX)     ─┤  to central compressor         (Tennessee, Texas          (power generation)
Permian (associated)    ─┤  station                        Eastern, Transco,         Industrial users
Appalachia              ─┘  ↓                              ANR, etc.)                LNG Export terminals
                            Gas Processing Plant:           ↓                         (Sabine Pass, etc.)
                            Remove NGLs (ethane,       Henry Hub, Louisiana     →   Storage fields:
                            propane, butane, etc.)     9+ pipelines intersect         Seasonal injection
                            ↓                          NYMEX NG delivery point        (Apr–Oct)
                            Residue (dry) gas           ↑                             Withdrawal (Nov–Mar)
                            enters transmission         Spot price benchmark for
                            system                      all North American NG
    ',
    dynamics = HTML('
      <h5>Key Market Dynamics</h5>
      <ul>
        <li><strong>Storage cycle:</strong> The most important fundamental driver. EIA
          weekly storage reports (Thursday) move the market. The market compares current
          storage to the 5-year average range. Below-average storage = bullish; surplus
          storage = bearish.</li>
        <li><strong>Weather sensitivity:</strong> NG is the most weather-sensitive
          energy commodity. Heating degree days (HDD) in winter and cooling degree days
          (CDD) in summer drive demand. Polar vortex events (2014, 2021) can spike
          front-month prices dramatically.</li>
        <li><strong>LNG export growth:</strong> Since 2016, U.S. LNG exports have grown
          from near zero to >14 Bcf/day, linking Henry Hub prices to global LNG markets
          (JKM, TTF). This has increased Henry Hub price correlations with European and
          Asian gas prices.</li>
        <li><strong>Samuelson effect:</strong> NG has the most pronounced Samuelson
          effect of the six markets — near-month vol is dramatically higher than
          deferred vol, reflecting weather uncertainty that dissipates over time.</li>
        <li><strong>Seasonality:</strong> Strongest of the six markets. Forward curve
          shows a "camel hump" shape in summer: summer months flat, winter C2–C5 at a
          significant premium (winter storage premium).</li>
      </ul>
    ')
  )
)

# ── Module UI ─────────────────────────────────────────────────────────────────

mod_market_process_ui <- function(id) {
  ns <- NS(id)

  tabs <- purrr::map(ENABLED_MARKETS, function(mkt) {
    info  <- .prod_chains[[mkt]]
    specs <- .contract_specs[[mkt]]

    bslib::nav_panel(
      title = MARKETS[[mkt]]$label,
      bslib::accordion(
        open = c("Production Chain", "Contract Specifications"),
        bslib::accordion_panel(
          "Production Chain",
          shiny::h5(info$title),
          shiny::div(class = "prod-chain", info$chain)
        ),
        bslib::accordion_panel(
          "Market Dynamics",
          info$dynamics
        ),
        bslib::accordion_panel(
          "Contract Specifications",
          DT::DTOutput(ns(paste0("specs_", mkt)))
        )
      )
    )
  })

  do.call(bslib::navset_card_pill, c(tabs, list(id = ns("market_tabs"))))
}

# ── Module server ─────────────────────────────────────────────────────────────

mod_market_process_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    purrr::walk(ENABLED_MARKETS, function(mkt) {
      local({
        m     <- mkt
        specs <- .contract_specs[[m]]
        output[[paste0("specs_", m)]] <- DT::renderDT({
          DT::datatable(
            specs,
            rownames  = FALSE,
            options   = list(
              dom          = "t",
              pageLength   = nrow(specs),
              ordering     = FALSE,
              scrollX      = TRUE
            ),
            class = "contract-specs table-sm table-striped"
          )
        })
      })
    })
  })
}
