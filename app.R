################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## IS-MP-PC Model: Interactive Shiny App                                      ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Open app.R in RStudio and click Run App, or from this folder:
##     shiny::runApp()
##   Needs R 4.1 or later with shiny, bslib and ggplot2 installed. A hosted
##   copy runs in the browser at https://sam-deegan.com/toy-models/is-mp-pc/
##   The lecture selector adds one layer of the model at a time:
##     1.1  IS, PC and MP as three separate curves
##     1.2  the solved model with adaptive expectations
##     1.3  the zero lower bound and an output-gap term in the rule
##     1.4  the Taylor principle relaxed; a bank lending rate
##     1.5  a financial accelerator in the bank mark-up
##   All text (scenarios, prompts, equations, notation) lives in B_03.
##
## Inputs:
##   R/model.R (the solver) and R/toolkit.R (shared layout and helpers),
##   both sourced automatically by Shiny. www/ holds the two images.
##
## Version:
##   B_03_25_version_chr; history in CHANGELOG.md; git tag vX.Y.Z.
##
## Outputs:
##   None. Figures for the slides are drawn by the same builders.
##
## Packages:
##   shiny, bslib, ggplot2.
##
## References:
##   Whelan, K. (2023). Lecture Notes on Macroeconomics. Ch. 1 (IS-MP-PC),
##     ch. 2 (solution and theta), ch. 4 (the zero lower bound).
##   Peia, O. (2024/25). ECON42550 lecture slides; cited as [M1 nn].
##   Romer, D. (2019). Advanced Macroeconomics, 5th ed. Ch. 6 and 12.
##   Clarida, Gali and Gertler (2000), QJE, for the pre-Volcker beta_pi.
##   Bernanke and Gertler (1995), JEP, for the bank lending channel.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: Section C (the model) is R/model.R so the slides can source it alone.
#
#   B: Setup
#     B_01  Packages
#     B_02  Settings
#     B_03  Soft-coded objects
#     B_04  Paths
#   C: Model (R/model.R)
#   D: Plots
#     D_01  Theme and helpers
#     D_02  Lecture 1.1 building blocks
#     D_03  IS-MP / PC diagram, time paths, stability
#     D_04  Narration
#   E: User Interface
#   F: Server
#   G: Run

################################################################################
## B: Setup ####################################################################
################################################################################
# Note: Packages, options and every soft-coded value.

#### B_01: Packages ############################################################
# Note: Shiny, bslib for the layout, ggplot2 for the figures.

###### B_01_01: Load Packages ##################################################
# Note: All three run under shinylive.

library(shiny)
library(bslib)
library(ggplot2)

###### B_01_02: Load the Model #################################################
# Note: Shiny sources R/ itself; this covers sourcing app.R by hand.

if (!exists("C_01_02_solve_period_fn")) {
  source(file.path("R", "model.R"))
}

#### B_02: Settings ############################################################
# Note: Standard options.

###### B_02_01: Global Options #################################################
# Note: No scientific notation; three digits in the console.

options(scipen = 999, digits = 3)

###### B_02_02: Seed ###########################################################
# Note: Nothing here is random; kept for consistency.

set.seed(42)

#### B_03: Soft-Coded Objects ##################################################
# Note: Calibration, stages, scenarios, palette, controls, equations, text.

###### B_03_01: Input Defaults #################################################
# Note: Starting value of every control; Reset returns here. Whelan ch. 1
#   calibration; alpha gamma < 1 is needed for an equilibrium at the ZLB
#   (Whelan ch. 4).

B_03_01_defaults_lst <- list(
  shk_demand = 0,     # eps^y, demand shock
  shk_supply = 0,     # eps^pi, inflation (supply) shock
  shk_expect = 0,     # one-off jump in expected inflation
  shk_markup = 0,     # mubar^B, bank mark-up shock
  shk_start  = 2,     # first period of the shock
  shk_length = 1,     # number of periods the shock lasts
  alpha      = 1,     # interest sensitivity of demand (IS)
  gamma      = 0.5,   # slope of the Phillips curve
  beta_pi    = 1.5,   # response of the policy rate to inflation
  beta_y     = 0,     # response of the policy rate to the output gap
  pi_star    = 2,     # inflation target
  r_star     = 1,     # natural real rate of interest
  lambda     = 0,     # anchoring: weight on pi* in expectations
  phi        = 0,     # financial accelerator: mark-up response to output
  zlb        = TRUE   # impose i_t >= 0
)

###### B_03_02: Horizon ########################################################
# Note: Number of periods simulated after the steady-state period 0.

B_03_02_n_periods_int <- 20L

###### B_03_03: Stages #########################################################
# Note: Labels follow _shared/econ42550-roadmap.tex.

B_03_03_stages_vec <- c(
  "Stage 1: Introducing IS-MP-PC"   = "1.1",
  "Stage 2: Analysing the Model"    = "1.2",
  "Stage 3: The Zero Lower Bound"   = "1.3",
  "Stage 4: Policy Rules and Banks" = "1.4",
  "Stage 5: The Financial Cycle"    = "1.5"
)

###### B_03_04: Scenarios ######################################################
# Note: Worked examples. Each sets a stage and overrides some defaults;
#   unlisted controls return to B_03_01. Story order and wording follow
#   CONVENTIONS.md 3 to 5. The beta_pi = 0.8 preset is the
#   Clarida, Gali and Gertler (2000) pre-Volcker estimate of 0.83.

B_03_04_scenarios_lst <- list(
  datacentre = list(
    label  = "AI Data Centre Boom",
    stage  = "1.1",
    values = list(shk_demand = 3),
    story  = paste(
      "Firms build data centres for AI, as in the buildout running since 2023,",
      "so investment demand rises for its own reasons and not because the real",
      "interest rate fell: the IS curve shifts <i>right</i>",
      "(\u03b5<sup>y</sup> = +3), and the dashed line is where it was. The",
      "economy climbs the Phillips curve: output (y) rises above potential",
      "(y*) and inflation (\u03c0) rises above target (\u03c0*) with it, both",
      "axes moving the same way, which is what marks a demand shock. How far",
      "depends on the interest sensitivity of demand (\u03b1) and on how hard",
      "the bank leans against inflation (\u03b2<sub>\u03c0</sub>). The policy",
      "rule itself does not move: the central bank slides along it."
    ),
    prompt = paste(
      "Watch the IS curve sit to the right of its dashed baseline while the",
      "policy rule stays exactly where it is. Now raise alpha: the faded curve",
      "is where IS was, so you can see the same shock open a wider gap. Which",
      "parameter rotates the Phillips curve rather than shifting it?"
    )
  ),
  covid = list(
    label  = "Covid Shock",
    stage  = "1.2",
    values = list(shk_demand = -4, shk_start = 2, shk_length = 2),
    story  = paste(
      "Households and firms cut spending for two periods, as in the 2020",
      "lockdowns, so the IS-MP curve shifts <i>left</i>",
      "(\u03b5<sup>y</sup> = \u22124, from period 2). The economy slides down",
      "the Phillips curve: output (y) falls below potential (y*) and inflation",
      "(\u03c0) falls below target (\u03c0*) with it, both axes moving the",
      "same way. How far depends on the interest sensitivity of demand",
      "(\u03b1) and on how hard the bank leans against inflation",
      "(\u03b2<sub>\u03c0</sub>); how long, on expectations (\u03bb = 0, so",
      "they follow last period's inflation). Watch both after the shock has",
      "gone."
    ),
    prompt = paste(
      "Press play and read the commentary. Why is inflation still below target",
      "after the shock has gone? Now raise beta_pi: the faded curve is where",
      "the example had it."
    )
  ),
  energy = list(
    label  = "Energy Shock",
    stage  = "1.2",
    values = list(shk_supply = 2, shk_start = 2, shk_length = 2),
    story  = paste(
      "Energy prices raise firms' costs for two periods, as after the 2022",
      "invasion of Ukraine, so the Phillips curve shifts <i>up</i>",
      "(\u03b5<sup>\u03c0</sup> = +2). The economy now moves along the",
      "downward-sloping IS-MP curve instead: inflation (\u03c0) rises above",
      "target (\u03c0*) while output (y) falls below potential (y*), the two",
      "axes moving in <i>opposite</i> directions, which a demand shock never",
      "forces. The severity of the trade-off is set by the slope of the",
      "Phillips curve (\u03b3) and by how hard the bank responds",
      "(\u03b2<sub>\u03c0</sub>); its persistence by the anchoring of",
      "expectations (\u03bb)."
    ),
    prompt = paste(
      "Press play. The central bank raises rates even as output falls. Now",
      "lower beta_pi and compare against the faded curve: less recession, but",
      "how much more inflation?"
    )
  ),
  expect = list(
    label  = "Increase in Inflation Expectations",
    stage  = "1.2",
    values = list(shk_expect = 3, shk_start = 2, shk_length = 1),
    story  = paste(
      "For one period the public simply expects higher inflation \u2014 no",
      "change in costs, none in spending \u2014 and that alone shifts the",
      "Phillips curve up (\u0394\u03c0<sup>e</sup> = +3). Inflation",
      "(\u03c0) rises and output (y) falls below potential (y*), as under a",
      "cost shock, but \u03c0 rises by <i>less</i> than three points: a bank",
      "that moves its rate more than one-for-one",
      "(\u03b2<sub>\u03c0</sub> &gt; 1) raises the real interest rate",
      "(i \u2212 \u03c0) and pulls the economy back down the IS-MP curve. The",
      "slope of the Phillips curve (\u03b3) decides how much of that",
      "correction arrives as lost output."
    ),
    prompt = paste(
      "Press play. Why does inflation rise by less than the jump in",
      "expectations? Set beta_pi to 1 and run it again: what changes?"
    )
  ),
  zlb_spiral = list(
    label  = "Deep Recession at the ZLB",
    stage  = "1.3",
    values = list(shk_demand = -6, shk_start = 2, shk_length = 3),
    story  = paste(
      "Demand collapses for three periods (\u03b5<sup>y</sup> = \u22126), far",
      "enough that the policy rate (i) reaches its floor of zero. Below that",
      "point the IS-MP curve changes <i>slope</i>: with i stuck, falling",
      "inflation (\u03c0) <i>raises</i> the real interest rate",
      "(i \u2212 \u03c0), so lower \u03c0 now means lower output (y) rather",
      "than higher. Both axes then fall together with nothing to stop them,",
      "and y keeps sliding further below potential (y*) after the shock has",
      "gone. The room the bank had before the floor is set by the natural real",
      "rate (r*) and the target (\u03c0*); whether the spiral runs at all, by",
      "the anchoring of expectations (\u03bb)."
    ),
    prompt = paste(
      "Press play. Once the rate hits zero the IS-MP curve slopes upwards.",
      "How much anchoring (lambda) does it take to stop the spiral?"
    )
  ),
  zlb_anchor = list(
    label  = "Same Recession, Anchored Expectations",
    stage  = "1.3",
    values = list(shk_demand = -6, shk_start = 2, shk_length = 3,
                  lambda = 0.6),
    story  = paste(
      "The same collapse in demand and the same floor, but expectations now",
      "hold sixty per cent of their weight on the target (\u03bb = 0.6).",
      "Inflation (\u03c0) stops falling, so the real interest rate",
      "(i \u2212 \u03c0) stops rising, and the economy climbs back up the",
      "Phillips curve: output (y) returns towards potential (y*) and \u03c0",
      "towards target (\u03c0*), both axes recovering instead of diverging.",
      "One parameter separates this run from the last; a higher target",
      "(\u03c0*) is the other way to keep the floor further off."
    ),
    prompt = paste(
      "Set lambda back to 0 and compare against the faded curve. Then try a",
      "higher target, pi* = 3: does it keep the economy off the floor?"
    )
  ),
  great_inflation = list(
    label  = "Great Inflation (beta_pi Below One)",
    stage  = "1.4",
    values = list(shk_supply = 1, shk_start = 2, shk_length = 1,
                  beta_pi = 0.8),
    story  = paste(
      "One small cost shock, for a single period (\u03b5<sup>\u03c0</sup> =",
      "+1) \u2014 but the central bank moves its rate <i>less</i> than",
      "one-for-one with inflation (\u03b2<sub>\u03c0</sub> = 0.8, close to",
      "Clarida, Gal\u00ed and Gertler's pre-Volcker estimate of 0.83). That",
      "reverses the slope of the IS-MP curve: a rise in inflation (\u03c0) now",
      "<i>lowers</i> the real interest rate (i \u2212 \u03c0), which pushes",
      "output (y) above potential (y*), which raises \u03c0 again. Both axes",
      "walk away from target together, the faster the steeper the Phillips",
      "curve (\u03b3). The model has no resting point to return to."
    ),
    prompt = paste(
      "Press play and watch inflation drift up for good. Now raise beta_pi",
      "above one, as Volcker did: how fast does inflation come back to the",
      "faded curve?"
    )
  ),
  bank_crisis = list(
    label  = "Banking Crisis (Mark-Up Shock)",
    stage  = "1.4",
    values = list(shk_markup = 2, shk_start = 2, shk_length = 4),
    story  = paste(
      "Banks widen the spread between the policy rate (i) and the rate they",
      "lend at (\u2113), for four periods, as in 2008\u201312",
      "(\u03bc\u0304<sup>B</sup> = +2). The rate in the IS curve is",
      "\u2113, not i, so the IS-MP curve shifts left <i>while the bank is",
      "cutting</i>. Output (y) falls below potential (y*) and inflation",
      "(\u03c0) below target (\u03c0*), as under a demand shock; the",
      "difference is that policy cannot offset it, because the instrument and",
      "the rate borrowers face have come apart. The damage depends on the",
      "interest sensitivity of demand (\u03b1) and on the room the bank had",
      "(r*)."
    ),
    prompt = paste(
      "Press play. The policy rate falls, but what happens to the lending rate",
      "firms pay? Try a longer crisis of eight periods."
    )
  ),
  accelerator = list(
    label  = "Demand Shock with a Financial Accelerator",
    stage  = "1.5",
    values = list(shk_demand = -2, shk_start = 2, shk_length = 2,
                  phi = 0.3),
    story  = paste(
      "A modest fall in demand shifts the IS-MP curve left",
      "(\u03b5<sup>y</sup> = \u22122), but bank mark-ups now widen as output",
      "(y) falls below potential (y*), which shifts it left again",
      "(\u03c6 = 0.3). Each round of falling y tightens credit and lowers y",
      "further, so y and inflation (\u03c0) both end up further from their",
      "targets than the shock alone would deliver. Set \u03c6 to zero and read",
      "the gap to the faded curve: that is how much of the depth is the",
      "accelerator, and the rest is demand working through the interest rate",
      "(\u03b1)."
    ),
    prompt = paste(
      "Set phi to 0 and read the gap to the faded curve: how much of the",
      "recession is the accelerator? Then make the demand shock -3."
    )
  )
)

###### B_03_05: Palette ########################################################
# Note: Dublin Beamer theme colours (dublin-theme.tex).

B_03_05_palette_vec <- c(
  navy  = "#04204C",
  blue  = "#0056A4",
  light = "#9FC4E0",
  green = "#61B77C",
  ink   = "#212529",
  muted = "#6C757D",
  rule  = "#D8E0E6",
  wash  = "#F2F6F9"
)

###### B_03_06: Plot Text Size #################################################
# Note: Readable when projected.

B_03_06_base_size_int <- 14L

###### B_03_07: Block Plot Height ##############################################
# Note: Height of each lecture 1.1 building-block plot in the browser.

B_03_07_block_height_chr <- "360px"

###### B_03_08: Diagram Height #################################################
# Note: Height of the IS-MP / PC diagram in the browser.

B_03_08_diagram_height_chr <- "460px"

###### B_03_09: Time Path Height ###############################################
# Note: Height of the four-panel time-path figure in the browser.

B_03_09_paths_height_chr <- "560px"

###### B_03_10: Axis Cap #######################################################
# Note: Diagram axes stop this far from the steady state.

B_03_10_diagram_cap_num <- 12

###### B_03_11: Path Cap #######################################################
# Note: Time-path values beyond +/- this are dropped.

B_03_11_path_cap_num <- 40

###### B_03_12: Prompts ########################################################
# Note: One "what to try" prompt per lecture, shown above the figures.

B_03_12_prompts_lst <- list(
  "1.1" = paste(
    "Move the demand shock and watch the IS curve shift while the policy",
    "rule stays put. Raise expected inflation: the whole Phillips curve",
    "moves up one-for-one. Which parameter rotates which curve?"
  ),
  "1.2" = paste(
    "Choose 'COVID: demand collapse' and press play above the diagram.",
    "Why is inflation still below target after the shock has gone? Raise",
    "beta_pi and compare against the faded curve."
  ),
  "1.3" = paste(
    "Load 'Deep recession at the ZLB'. Once the policy rate hits zero the",
    "IS-MP curve slopes upwards, and falling expectations drag the economy",
    "down. How much anchoring (lambda) does it take to stop the spiral?"
  ),
  "1.4" = paste(
    "Set beta_pi below one: a supply shock now lowers the real interest",
    "rate and inflation takes off. Then try a mark-up shock: the policy",
    "rate falls, but what happens to the lending rate?"
  ),
  "1.5" = paste(
    "Load 'Demand shock with a financial accelerator' and set phi to 0. The",
    "faded curve is the example with the accelerator on, so the gap to it is",
    "what the accelerator does. Now make the shock -3: it turns a brief visit",
    "to the lower bound into a spiral."
  )
)

###### B_03_13: Controls #######################################################
# Note: One entry per numeric control: label, range, step and the lecture
#   from which it appears.

B_03_13_controls_lst <- list(
  shk_demand = list(label = "Demand Shock (ε<sup>y</sup>)",
                    min = -8, max = 8, step = 0.5, from = 1.1),
  shk_supply = list(label = "Inflation Shock (ε<sup>π</sup>)",
                    min = -4, max = 4, step = 0.25, from = 1.1),
  shk_expect = list(label = "Jump in Expected Inflation (Δπ<sup>e</sup>)",
                    min = -4, max = 4, step = 0.25, from = 1.1),
  shk_markup = list(label = "Bank Mark-Up Shock (μ<sup>B</sup>)",
                    min = 0, max = 4, step = 0.25, from = 1.4),
  shk_start  = list(label = "Shock Starts in Period",
                    min = 1, max = 10, step = 1, from = 1.2),
  shk_length = list(label = "Shock Length (Periods)",
                    min = 1, max = 10, step = 1, from = 1.2),
  alpha      = list(label = "Interest Sensitivity of Demand (α)",
                    min = 0.1, max = 2, step = 0.1, from = 1.1),
  gamma      = list(label = "Slope of the Phillips Curve (γ)",
                    min = 0.05, max = 1.5, step = 0.05, from = 1.1),
  beta_pi    = list(label = "Response to Inflation (β<sub>π</sub>)",
                    min = 0.2, max = 3, step = 0.05, from = 1.1),
  beta_y     = list(label = "Response to the Output Gap (β<sub>y</sub>)",
                    min = 0, max = 1.5, step = 0.05, from = 1.3),
  pi_star    = list(label = "Inflation Target (π*)",
                    min = 0, max = 4, step = 0.25, from = 1.1),
  r_star     = list(label = "Natural Real Interest Rate (r*)",
                    min = -1, max = 3, step = 0.25, from = 1.1),
  lambda     = list(label = "Anchoring of Expectations (λ)",
                    min = 0, max = 1, step = 0.05, from = 1.2),
  phi        = list(label = "Mark-Up Response to the Output Gap (φ)",
                    min = 0, max = 0.4, step = 0.05, from = 1.5)
)

###### B_03_14: Lecture of the ZLB Switch ######################################
# Note: The ZLB checkbox is not numeric, so it is not in B_03_13.

B_03_14_zlb_from_num <- 1.3

###### B_03_15: Parameter Explanations #########################################
# Note: Tooltip text: what each control is and what raising it does.

B_03_15_help_lst <- list(
  shk_demand = paste(
    "A shift in spending not caused by interest rates: confidence,",
    "fiscal policy, lockdowns. Shifts the IS and IS-MP curves."
  ),
  shk_supply = paste(
    "A cost-push shock, such as energy prices or supply chains, that raises",
    "inflation at any output gap. Shifts the Phillips curve."
  ),
  shk_expect = paste(
    "A one-off jump in the inflation the public expects. Shifts the",
    "Phillips curve up one-for-one."
  ),
  shk_markup = paste(
    "An exogenous rise in banks' lending mark-up (credit risk, bank losses,",
    "less competition). Raises the lending rate at any policy rate."
  ),
  shk_start  = "The period in which the shocks first hit.",
  shk_length = "How many periods the shocks last before switching off.",
  alpha = paste(
    "A slope: a 1-point rise in the real rate lowers the output gap by α",
    "points (per cent of potential). Higher α makes monetary policy more",
    "powerful."
  ),
  gamma = paste(
    "A slope: each point of output gap adds γ points to inflation. Higher γ",
    "means inflation reacts more to slack, so bringing it down costs less",
    "output."
  ),
  beta_pi = paste(
    "A response coefficient: the policy rate moves β<sub>π</sub> points per",
    "point of inflation above target. Above one the real rate rises with",
    "inflation (the Taylor principle); below one it falls."
  ),
  beta_y = paste(
    "A response coefficient: the policy rate moves β<sub>y</sub> points per",
    "point of output gap. Higher values cushion output but let inflation",
    "drift more after supply shocks."
  ),
  pi_star = paste(
    "The inflation target. A higher target means higher normal interest",
    "rates, and more room to cut before hitting zero."
  ),
  r_star = paste(
    "The natural real interest rate: the real rate that keeps output at",
    "potential. A lower r* leaves less room to cut before the lower bound."
  ),
  lambda = paste(
    "The weight people put on the target when forming expectations.",
    "λ = 0: expectations follow last period's inflation (the slides'",
    "adaptive case). λ = 1: fully anchored at the target."
  ),
  phi = paste(
    "A slope: bank mark-ups widen by φ points per point of negative output",
    "gap. Higher φ amplifies downturns: a financial accelerator."
  )
)

###### B_03_16: The Model, Lecture by Lecture ##################################
# Note: The equations panel. "versions" maps the lecture a form first applies
#   to its LaTeX; the panel shows the latest version at the chosen lecture and
#   flags what is new or changed. Whelan ch. 1: PC eq. 1.3, IS eq. 1.5, rule
#   eq. 1.8, IS-MP eq. 1.18; ch. 2 eqs 2.5 to 2.10 for theta and the solved
#   forms; ch. 4 for the ZLB forms. The lending rate, mark-up and lambda are
#   this app's extensions and are labelled as such on screen.

B_03_16_equations_lst <- list(

  # --- The model's equations --------------------------------------------------
  list(
    group = "model", label = "Investment\u2013Saving (IS) Curve",
    versions = list(
      "1.1" = "y_t = y_t^* - \\alpha\\,(i_t - \\pi_t - r^*) + \\epsilon_t^y",
      "1.4" = "y_t = y_t^* - \\alpha\\,(\\ell_t - \\pi_t - r^*) + \\epsilon_t^y"
    ),
    notes = list(
      "1.1" = paste("Spending falls when the real interest rate is above its",
                    "natural level r*."),
      "1.4" = paste("Spending now depends on the rate banks charge, not the",
                    "policy rate.")
    )
  ),
  list(
    group = "model", label = "Phillips Curve (PC)",
    versions = list(
      "1.1" = "\\pi_t = \\pi_t^e + \\gamma\\,(y_t - y_t^*) + \\epsilon_t^\\pi"
    ),
    notes = list(
      "1.1" = paste("Inflation moves one-for-one with expected inflation and",
                    "rises with the output gap.")
    )
  ),
  list(
    group = "model", label = "Monetary Policy (MP) Rule",
    versions = list(
      "1.1" = "i_t = r^* + \\pi^* + \\beta_\\pi(\\pi_t - \\pi^*)",
      "1.3" = paste0("\\begin{aligned} i_t = \\max\\big[\\,",
                     "& r^* + \\pi^* + \\beta_\\pi(\\pi_t - \\pi^*) \\\\",
                     "& + \\beta_y(y_t - y_t^*),\\; 0\\,\\big]",
                     "\\end{aligned}")
    ),
    notes = list(
      "1.1" = paste("The central bank raises its rate when inflation is",
                    "above target."),
      "1.3" = paste("Rates cannot fall below zero. The rule can also respond",
                    "to the output gap (β<sub>y</sub>; leave it at 0 for the",
                    "simple rule).")
    )
  ),
  list(
    group = "model", label = "Lending Rate",
    versions = list("1.4" = "\\ell_t = i_t + \\mu_t^B"),
    notes = list(
      "1.4" = "Banks add a mark-up; a mark-up shock tightens credit."
    )
  ),
  list(
    group = "model", label = "Bank Mark-Up",
    versions = list(
      "1.4" = "\\mu_t^B = \\bar{\\mu}_t^B",
      "1.5" = "\\mu_t^B = \\bar{\\mu}_t^B - \\phi\\,(y_t - y_t^*)"
    ),
    notes = list(
      "1.4" = "Set by the banks, outside the model: the mark-up shock.",
      "1.5" = paste("Mark-ups widen when output falls: a financial",
                    "accelerator. An extension beyond the previous slides.")
    )
  ),
  # --- Assumptions ------------------------------------------------------------
  list(
    group = "assumption", label = "Expectations",
    versions = list(
      "1.1" = "\\pi_t^e = \\pi^* + \\Delta\\pi^e",
      "1.2" = "\\pi_t^e = \\lambda\\,\\pi^* + (1 - \\lambda)\\,\\pi_{t-1}"
    ),
    notes = list(
      "1.1" = "For now, expected inflation is a number you set.",
      "1.2" = paste("Expectations carry last period's inflation forward,",
                    "which gives the model its dynamics. λ = 0 is the",
                    "slides' adaptive case, π<sup>e</sup><sub>t</sub> =",
                    "π<sub>t−1</sub>.")
    )
  ),
  list(
    group = "assumption", label = "Shocks",
    versions = list(
      "1.1" = "\\epsilon_t^y,\\ \\epsilon_t^\\pi \\text{ are temporary}"
    ),
    notes = list(
      "1.1" = paste("Demand and supply shocks are temporary deviations from",
                    "zero; they switch off after the shock window.")
    )
  ),
  list(
    group = "assumption", label = "Given",
    versions = list(
      "1.1" = "y_t^*,\\ r^*,\\ \\pi^* \\text{ fixed}"
    ),
    notes = list(
      "1.1" = paste("Potential output, the natural rate and the target are",
                    "set outside the model.")
    )
  ),
  list(
    group = "assumption", label = "Taylor Principle",
    versions = list(
      "1.1" = "\\beta_\\pi > 1",
      "1.4" = "\\beta_\\pi > 0"
    ),
    notes = list(
      "1.1" = paste("Assumed for now: the policy rate rises more than",
                    "one-for-one with inflation."),
      "1.4" = "Relaxed: β<sub>π</sub> can now be below one."
    )
  ),
  list(
    group = "assumption", label = "Equilibrium at the Floor",
    versions = list("1.3" = "\\alpha\\gamma < 1"),
    notes = list(
      "1.3" = paste("Needed for the curves to cross at the lower bound: the",
                    "Phillips curve must be flatter than the IS-MP curve",
                    "there.")
    )
  ),

  # --- Solved forms -----------------------------------------------------------
  list(
    group = "solved", label = "IS-MP Curve",
    versions = list(
      "1.2" = paste0("y_t - y_t^* = -\\alpha(\\beta_\\pi - 1)",
                     "(\\pi_t - \\pi^*) + \\epsilon_t^y"),
      "1.4" = paste0("y_t - y_t^* = -\\alpha(\\beta_\\pi - 1)",
                     "(\\pi_t - \\pi^*) - \\alpha\\mu_t^B + \\epsilon_t^y")
    ),
    notes = list(
      "1.2" = paste("The rule substituted into the IS curve. With",
                    "β<sub>π</sub> > 1, higher inflation means a higher real",
                    "rate and lower output."),
      "1.4" = "A higher mark-up shifts the IS-MP curve left."
    )
  ),
  list(
    group = "solved", label = "Output Gap",
    versions = list(
      "1.2" = paste0("y_t - y_t^* = \\theta\\epsilon_t^y - ",
                     "\\alpha\\theta(\\beta_\\pi - 1)",
                     "\\big[(\\pi_t^e - \\pi^*) + ",
                     "\\epsilon_t^\\pi\\big]")
    ),
    notes = list(
      "1.2" = paste("The other half of the solution, and the one the diagram",
                    "is drawn in. A demand shock passes through at \u03b8; a",
                    "supply shock or a jump in expectations costs output only",
                    "because the bank responds, which is why both arrive",
                    "multiplied by \u03b1\u03b8(\u03b2<sub>\u03c0</sub>",
                    "\u2212 1).")
    )
  ),
  list(
    group = "solved", label = "Inflation",
    versions = list(
      "1.2" = paste0("\\pi_t = \\theta\\pi_t^e + (1 - \\theta)\\pi^* + ",
                     "\\theta(\\gamma\\epsilon_t^y + \\epsilon_t^\\pi)")
    ),
    notes = list(
      "1.2" = paste("IS-MP and PC solved together. Inflation moves less than",
                    "one-for-one with expectations when θ < 1.")
    )
  ),
  list(
    group = "solved", label = "IS-MP at the ZLB",
    versions = list(
      "1.3" = paste0("y_t - y_t^* = \\alpha r^* + \\alpha\\pi_t + ",
                     "\\epsilon_t^y")
    ),
    notes = list(
      "1.3" = paste("Below the trigger i<sub>t</sub> = 0, so lower inflation",
                    "means a higher real rate: the curve slopes upwards.")
    )
  ),
  list(
    group = "solved", label = "Inflation at the ZLB",
    versions = list(
      "1.3" = paste0("\\pi_t = \\frac{\\pi_t^e + \\alpha\\gamma r^* + ",
                     "\\gamma\\epsilon_t^y + \\epsilon_t^\\pi}",
                     "{1 - \\alpha\\gamma}")
    ),
    notes = list(
      "1.3" = paste("The coefficient on π<sup>e</sup>, 1/(1 − αγ), is above",
                    "one, so falling expectations feed on themselves: the",
                    "liquidity trap.")
    )
  ),

  # --- Thresholds and descriptors ---------------------------------------------
  list(
    group = "descriptor", label = "θ",
    versions = list(
      "1.2" = "\\theta = \\frac{1}{1 + \\alpha\\gamma(\\beta_\\pi - 1)}"
    ),
    notes = list(
      "1.2" = "How much of a change in expectations passes into inflation."
    )
  ),
  list(
    group = "descriptor", label = "Shocks Die Out If",
    versions = list("1.2" = "(1 - \\lambda)\\,\\theta < 1"),
    notes = list(
      "1.2" = "Inflation persistence. With λ = 0 this is just θ < 1."
    )
  ),
  list(
    group = "descriptor", label = "ZLB Trigger",
    versions = list(
      "1.3" = paste0("\\pi^{ZLB} = \\frac{\\beta_\\pi - 1}{\\beta_\\pi}",
                     "\\pi^* - \\frac{r^*}{\\beta_\\pi}")
    ),
    notes = list(
      "1.3" = paste("Below this inflation rate the rule asks for a negative",
                    "rate (with β<sub>y</sub> = 0).")
    )
  ),
  list(
    group = "descriptor", label = "Stability and β<sub>π</sub>",
    versions = list(
      "1.4" = paste0("\\beta_\\pi > 1 \\iff \\theta < 1")
    ),
    notes = list(
      "1.4" = paste("β<sub>π</sub> can now go below one. Then the real rate",
                    "falls when inflation rises, and inflation runs away.")
    )
  )
)

###### B_03_17: Equation Group Titles ##########################################
# Note: Group headings for the equations tabs.

B_03_17_groups_vec <- c(
  model      = "Model Equations",
  assumption = "Assumptions",
  solved     = "Solved Forms",
  descriptor = "Thresholds and Simplifications"
)

###### B_03_18: Recalculation Delay ############################################
# Note: Milliseconds to wait before recalculating after a change.

B_03_18_debounce_ms_int <- 250L

###### B_03_19: Animation Speed ################################################
# Note: Milliseconds per period when play is pressed.

B_03_19_play_interval_int <- 2500L

###### B_03_20: Author Credit ##################################################
# Note: Title bar, browser tab and footer.

B_03_20_author_chr <- "Sam Deegan"

###### B_03_21: Author Website #################################################
# Note: Linked from the byline and footer.

B_03_21_site_chr <- "https://sam-deegan.com"

###### B_03_22: Course Line ####################################################
# Note: Footer text.

B_03_22_course_chr <- "ECON42550 Macroeconomics, University College Dublin"

###### B_03_23: Notation Key ###################################################
# Note: Notation tab. Groups: var, par, tgt (targets and thresholds), shk.
#   Symbols follow Whelan ch. 1.

B_03_23_notation_lst <- list(
  list(grp = "var", sym = "y_t", txt = "output (log, per cent)", from = 1.1),
  list(grp = "var", sym = "y_t^*", txt = "potential output", from = 1.1),
  list(grp = "var", sym = "y_t - y_t^*", txt = "output gap", from = 1.1),
  list(grp = "var", sym = "\\pi_t", txt = "inflation", from = 1.1),
  list(grp = "var", sym = "\\pi_t^e", txt = "expected inflation", from = 1.1),
  list(grp = "tgt", sym = "\\pi^*", txt = "inflation target", from = 1.1),
  list(grp = "var", sym = "i_t", txt = "policy (nominal) interest rate",
       from = 1.1),
  list(grp = "var", sym = "i_t - \\pi_t", txt = "real interest rate",
       from = 1.1),
  list(grp = "tgt", sym = "r^*", txt = "natural real interest rate",
       from = 1.1),
  list(grp = "shk", sym = "\\epsilon_t^y", txt = "demand shock", from = 1.1),
  list(grp = "shk", sym = "\\epsilon_t^\\pi", txt = "inflation (supply) shock",
       from = 1.1),
  list(grp = "par", sym = "\\alpha", txt = "interest sensitivity of demand",
       from = 1.1),
  list(grp = "par", sym = "\\gamma", txt = "slope of the Phillips curve",
       from = 1.1),
  list(grp = "par", sym = "\\beta_\\pi", txt = "policy response to inflation",
       from = 1.1),
  list(grp = "par", sym = "\\lambda", txt = "anchoring of expectations",
       from = 1.2),
  list(grp = "par", sym = "\\theta",
       txt = "pass-through of expectations to inflation", from = 1.2),
  list(grp = "par", sym = "\\beta_y", txt = "policy response to the output gap",
       from = 1.3),
  list(grp = "tgt", sym = "\\pi^{ZLB}", txt = "inflation that triggers the ZLB",
       from = 1.3),
  list(grp = "var", sym = "\\ell_t", txt = "bank lending rate", from = 1.4),
  list(grp = "var", sym = "\\mu_t^B", txt = "bank mark-up", from = 1.4),
  list(grp = "shk", sym = "\\bar{\\mu}_t^B", txt = "mark-up shock", from = 1.4),
  list(grp = "par", sym = "\\phi", txt = "mark-up response to the output gap",
       from = 1.5)
)

###### B_03_24: Size of a Curve's Name #########################################
# Note: Matches the axis tick labels: 0.8 x 14 pt = 11.2 pt = 3.9 mm.

B_03_24_name_size_num <- 3.9

###### B_03_25: Version ########################################################
# Note: Semantic version, shown in the footer; CHANGELOG.md has the history.

B_03_25_version_chr <- "1.0.8"

###### B_03_26: Source Repository ##############################################
# Note: The GitHub repo, linked from the footer.

B_03_26_repo_chr <- paste0("https://github.com/Sam-Deegan/",
                        "Interactive-Model-IS-MP-PC")

#### B_04: Paths ###############################################################
# Note: The QR code only.

###### B_04_01: QR Code Source #################################################
# Note: Embedded as a data URI because shinylive's export drops www/.

B_04_01_qr_src_chr <- paste0(
  "data:image/png;base64,",
  "iVBORw0KGgoAAAANSUhEUgAAAdAAAAHQCAIAAACeP6xXAAAG6UlEQVR42u3cwW",
  "0cMRBFQa9BB6KAFK0DciA+0DdDJwEEppe/yaoAJE5r5oHQoV9zzh8A1PtpBACC",
  "CyC4AAgugOACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgCCCyC4AAgugO",
  "ACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgDVRvUv+PXxacoP+vvnd+n8",
  "V39+9ftQ/bzV8/d9nf19ueEChBJcAMEFEFwABBdAcAEE1wgABBdAcAEQXADBBR",
  "BcAAQXQHABEFyAVCPtQGn7Rqul7TO9bZ/sbe+b78sNF+AKggsguACCC4DgAggu",
  "gOAaAYDgAgguAIILILgAgguA4AIILgCCC5BqdH+AtH2X3feNrp4/bf5p70/398",
  "H35YYLILgACC6A4AIILgCCCyC4AAgugOACCC4AggsguAAILoDgAgguAA8aRnC2",
  "6v2h1ftz086/Ku15ccMFEFwABBdAcAEQXADBBRBcAAQXQHABEFwAwQUQXAAEF0",
  "BwARBcgI3swz1c9T7W7vtt7avFDRdAcAEQXADBBUBwAQQXQHABEFwAwQVAcAEE",
  "F0BwARBcAMEFQHAB3qb9Plz7SffOp/rnd99X2/399H254QIILgCCCyC4AIILgO",
  "ACCC4AggsguACCC4DgAgguAIILILgAggvAg+L24a7uP+XZea7uP+2+r7b7/H1f",
  "brgACC6A4AIILgCCCyC4AAgugOACCC4AggsguAAILoDgAgguAIIL0Ej5Ptzb9p",
  "+m6T5/+3l9X264AAgugOACCC4AggsguAAILoDgAgguAIILILgACC6A4AIILgCC",
  "C3CM8n24aftJV89zG/tze72f3c+T9j1Wz9MNF+BNBBdAcAEEFwDBBRBcAME1Ag",
  "DBBRBcAAQXQHABBBcAwQUQXAAEFyDVa8551QNX79+0n3Qvz3v286Z9j264AKEE",
  "F0BwAQQXAMEFEFwAwTUCAMEFEFwABBdAcAEEFwDBBRBcAAQXINXo/gDd98PaT/",
  "rsfLq/D7edp/v+aDdcAP9SABBcAAQXQHABEFwAwQUQXAAEF0BwARBcAMEFEFwA",
  "BBdAcAFYUr4Pt3ofZdq+TvtAneed0vbnVs/fDRcAwQUQXADBBUBwAQQXAMEFEF",
  "wAwQVAcAEEFwDBBRBcAMEFQHABWhtGsFf1PtDu+2TT9infNp+09y3t+3LDBQgl",
  "uACCCyC4AAgugOACCK4RAAgugOACILgAggsguAAILoDgAiC4AKnsw31Y2j7QVW",
  "n7Q51/7/vmPG64AC0JLoDgAgguAIILILgAgmsEAIILILgACC6A4AIILgCCCyC4",
  "AAguQKq4fbjd911W72Ndfd7qeXrevT+/+77a23rihgvgXwoAgguA4AIILoDgGg",
  "GA4AIILgCCCyC4AIILgOACCC4AggsguACXi9uHm7aP9bZ9r91VP699wWc/rxsu",
  "gH8pACC4AIILILgACC6A4AIguACCCyC4AAgugOACILgAggsguACUGkawV9q+zr",
  "R9u/ar7j1P2v7c7vug3XABBBdAcAEQXADBBRBcIwAQXADBBUBwAQQXQHABEFwA",
  "wQVAcAEEF+ByrzmnKRwsbX9r2v7c286Tdv60fcduuACHEFwAwQUQXAAEF0BwAQ",
  "TXCAAEF0BwARBcAMEFEFwABBdAcAEQXIBUo/oXpO1j7S5tv2f1/tPVn582n7Tv",
  "q/v7030+brgAbyK4AIILILgACC6A4AIIrhEACC6A4AIguACCCyC4AAgugOACIL",
  "gAqUbagewzNf+T5unvZT5uuACCCyC4AAgugOACILgAggsguAAILoDgAiC4AIIL",
  "ILgACC6A4ALw3+j+APbJ7p1n9fN2P0/39xk3XADBBUBwAQQXQHABEFwAwQVAcA",
  "EEF0BwARBcAMEFQHABBBdAcAF40DACvlrdJ1u9vzXtPNXnX5W2L7ha2vvmhgsQ",
  "SnABBBdAcAEQXADBBRBcIwAQXADBBUBwAQQXQHABEFwAwQVAcAFS2Yd7uOr9nr",
  "ftq60+f9p+27S/lxsuAIILILgAgguA4AIILgCCCyC4AIILgOACCC4AggsguACC",
  "C4DgArTWfh9u9T5Qvle9vzVtP2z397l6v233/chuuACCC4DgAggugOACILgAgg",
  "uA4AIILoDgAiC4AIILgOACCC6A4AJQKm4f7m37MbvP87b9p2nPu3qetH273ff/",
  "uuEChBJcAMEFEFwABBdAcAEE1wgABBdAcAEQXADBBRBcAAQXQHABEFyAVK85py",
  "kAuOECCC4AggsguACCC4DgAgguAIILILgAgguA4AIILgCCCyC4AIILgOACCC4A",
  "ggsguACCC4DgAgguAIILILgAgguA4AIILgCCCyC4AIILgOACnOAfrk+XUEDcDq",
  "kAAAAASUVORK5CYII="
)

################################################################################
## D: Plots ####################################################################
################################################################################
# Note: Builders only; each returns a ggplot for the server to draw. Figure
#   conventions follow Whelan figs 1.5 to 1.10 and Peia [M1 50, 66, 68, 72,
#   79]: a curve carries its name, not its equation; a marked value gets
#   dashed leaders from both axes with the symbol as an axis break; a shift
#   draws both positions solid with arrows between them. See CONVENTIONS.md 6.

#### D_01: Theme and Helpers ###################################################
# Note: Shared theme and label placement.

###### D_01_01: Plot Theme #####################################################
# Note: theme_bw with Dublin colours. grid = "h" for a series read off the
#   y axis, "none" for a diagram.

D_01_01_theme_fn <- function(base_size = B_03_06_base_size_int,
                             grid = c("h", "none"), ratio = 2 / 3) {
  grid <- match.arg(grid)
  theme_bw(base_size = base_size) +
    theme(
      # Every figure is 3:2 whatever box it is drawn in (CONVENTIONS.md 6);
      #   the faceted time paths pass ratio = NULL
      aspect.ratio     = ratio,
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = if (grid == "h") {
        element_line(colour = B_03_05_palette_vec[["rule"]], linewidth = 0.3)
      } else {
        element_blank()
      },
      panel.border     = element_blank(),
      # No border, so the two spines are drawn explicitly
      axis.line        = element_line(colour = B_03_05_palette_vec[["muted"]],
                                      linewidth = 0.4),
      axis.line.x.top  = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks       = element_line(colour = B_03_05_palette_vec[["muted"]],
                                      linewidth = 0.4),
      strip.background = element_rect(fill = B_03_05_palette_vec[["wash"]],
                                      colour = B_03_05_palette_vec[["rule"]]),
      strip.text       = element_text(colour = B_03_05_palette_vec[["navy"]],
                                      face = "bold", hjust = 0),
      # Titles sit in the card header, never inside the image
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      # Reading notes go under the figure, never inside it
      plot.caption     = element_blank(),
      axis.title       = element_text(colour = B_03_05_palette_vec[["ink"]],
                                      face = "bold", size = rel(0.85)),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

###### D_01_02: Axis Limits ####################################################
# Note: Covers the values and a minimum window, padded 10 per cent, capped.

D_01_02_limits_fn <- function(vals, centre, half_width, cap) {
  vals <- vals[is.finite(vals)]
  lo   <- min(c(vals, centre - half_width))
  hi   <- max(c(vals, centre + half_width))
  pad  <- 0.1 * (hi - lo)
  c(max(lo - pad, centre - cap), min(hi + pad, centre + cap))
}

###### D_01_03: Dashed Leaders to a Point ######################################
# Note: Dashed leaders from both axes to a point (Whelan fig. 1.10). Darker
#   than the zero cross from T_02_02_zero_fn.

D_01_03_leader_fn <- function(x, y, x_from, y_from,
                              colour = B_03_05_palette_vec[["ink"]]) {
  list(
    annotate("segment", x = x_from, xend = x, y = y, yend = y,
             linetype = "dashed", linewidth = 0.45, colour = colour),
    annotate("segment", x = x, xend = x, y = y_from, yend = y,
             linetype = "dashed", linewidth = 0.45, colour = colour)
  )
}

###### D_01_04: Where a Curve Leaves the Panel #################################
# Note: The visible point of a curve furthest in the direction asked for, or
#   NULL if the curve is off-panel. Names sit at this end.

D_01_04_endpt_fn <- function(df, x_lim, y_lim,
                             side = c("top", "right", "bottom", "left")) {
  side <- match.arg(side)
  vis  <- df[is.finite(df$x) & is.finite(df$y) &
               df$x >= x_lim[1] & df$x <= x_lim[2] &
               df$y >= y_lim[1] & df$y <= y_lim[2], ]
  if (nrow(vis) == 0) return(NULL)
  i <- switch(side,
              top    = which.max(vis$y),
              right  = which.max(vis$x),
              bottom = which.min(vis$y),
              left   = which.min(vis$x))
  vis[i, c("x", "y")]
}

###### D_01_05: How Wide a Name Is, in Data Units ##############################
# Note: Estimated width of a name in data units, so a label can clear a
#   sloped line over its whole width. No graphics device, so 0.55 of the
#   point size per glyph; panel_mm errs narrow so clearance errs generous.

D_01_05_labw_fn <- function(label, x_lim, size = B_03_24_name_size_num,
                            panel_mm = 128) {
  # Strip plotmath mark-up; each Greek name counts as one glyph
  txt <- gsub("bar(", "", label, fixed = TRUE)
  txt <- gsub("==", "=", txt, fixed = TRUE)
  for (chr in c("*", "'", "^", "[", "]")) {
    txt <- gsub(chr, "", txt, fixed = TRUE)
  }
  txt <- gsub(paste0("(epsilon|alpha|beta|gamma|delta|lambda|sigma|theta|",
                     "mu|phi|pi)"), "x", txt)
  nchar(txt) * 0.55 * size / panel_mm * diff(range(x_lim))
}

###### D_01_06: Every Line's Name at Its End ###################################
# Note: Places every name on a panel in one call. Each name goes in the
#   nearest empty band that clears every line in avoid_lst, and every name
#   already placed, across the name's own width. spec_lst holds one
#   list(pt, label, colour, anchor) per name; anchor is the end of the name
#   that sits at pt$x.

D_01_06_endlabs_fn <- function(spec_lst, x_lim, y_lim, avoid_lst = list(),
                               size = B_03_24_name_size_num, panel_mm = 52) {

  # Clearance is one text height
  h_num <- 0.75 * size / panel_mm * diff(range(y_lim))
  cand  <- seq(y_lim[1] + h_num, y_lim[2] - h_num, length.out = 401L)
  out   <- list()

  for (spec in spec_lst) {
    pt <- spec$pt
    if (is.null(pt)) next
    w_num <- D_01_05_labw_fn(spec$label, x_lim, size)
    span  <- if (spec$anchor == "right") c(pt$x - w_num, pt$x) else
      c(pt$x, pt$x + w_num)

    # Heights every line reaches across the span this name occupies
    hits <- unlist(lapply(avoid_lst, function(d) {
      d$y[is.finite(d$x) & is.finite(d$y) & d$x >= span[1] & d$x <= span[2]]
    }))
    hits <- hits[hits >= y_lim[1] & hits <= y_lim[2]]

    free <- if (length(hits) == 0) {
      rep(TRUE, length(cand))
    } else {
      vapply(cand, function(y) all(abs(hits - y) > h_num), logical(1))
    }
    y_num <- if (any(free)) {
      cand[free][which.min(abs(cand[free] - pt$y))]
    } else {
      pt$y
    }

    out <- c(out, list(annotate(
      "text", x = pt$x, y = y_num, label = spec$label, parse = TRUE,
      hjust = if (spec$anchor == "right") 1 else 0, vjust = 0.5,
      colour = spec$colour, size = size)))
    avoid_lst <- c(avoid_lst, list(data.frame(
      x = seq(span[1], span[2], length.out = 21L), y = y_num)))
  }
  out
}

###### D_01_07: Shift Arrows ###################################################
# Note: Arrows from the old position of a curve to the new one (Whelan figs
#   1.6, 1.7, 1.9; Peia [M1 50, 51, 66]). Each runs to the nearest point of
#   the new curve in panel units, so it is square-on whichever way the curve
#   moved. Trimmed so it touches neither line.

D_01_07_shift_arrows_fn <- function(from_df, to_df, x_lim, y_lim, colour,
                                    n_arrows = 4L, trim = 0.30,
                                    keep = 0.76) {
  keep_fn <- function(d) d[is.finite(d$x) & is.finite(d$y) &
                             d$x >= x_lim[1] & d$x <= x_lim[2] &
                             d$y >= y_lim[1] & d$y <= y_lim[2], ]
  from_df <- keep_fn(from_df)
  to_df   <- keep_fn(to_df)
  if (nrow(from_df) < 2 || nrow(to_df) < 2) return(NULL)

  sx  <- diff(range(x_lim))
  sy  <- diff(range(y_lim))
  # Arrows over the middle of the curve, clear of the names at its ends
  lo  <- (1 - keep) / 2
  idx <- unique(round(seq(lo, 1 - lo, length.out = n_arrows) *
                        (nrow(from_df) - 1L) + 1L))

  seg_df <- do.call(rbind, lapply(idx, function(i) {
    x0 <- from_df$x[i]
    y0 <- from_df$y[i]
    j  <- which.min(((to_df$x - x0) / sx)^2 + ((to_df$y - y0) / sy)^2)
    data.frame(x = x0, y = y0, xend = to_df$x[j], yend = to_df$y[j])
  }))
  gap_num <- abs(seg_df$xend - seg_df$x) / sx +
    abs(seg_df$yend - seg_df$y) / sy
  seg_df  <- seg_df[gap_num > 0.02, ]
  if (nrow(seg_df) == 0) return(NULL)

  dx_num <- seg_df$xend - seg_df$x
  dy_num <- seg_df$yend - seg_df$y
  cut_df <- data.frame(
    x    = seg_df$x    + trim * dx_num,
    y    = seg_df$y    + trim * dy_num,
    xend = seg_df$xend - trim * dx_num,
    yend = seg_df$yend - trim * dy_num)

  # pts is the ground the arrows occupy, for D_01_06_endlabs_fn to avoid
  list(
    layer = annotate("segment", x = cut_df$x, y = cut_df$y,
                     xend = cut_df$xend, yend = cut_df$yend,
                     colour = colour, linewidth = 0.5,
                     arrow = arrow(length = unit(0.16, "cm"),
                                   type = "closed")),
    pts = data.frame(
      x = c(cut_df$x, cut_df$xend, (cut_df$x + cut_df$xend) / 2),
      y = c(cut_df$y, cut_df$yend, (cut_df$y + cut_df$yend) / 2))
  )
}

###### D_01_08: Curve Tag ######################################################
# Note: Name plus the parameter that separates two states of one curve, as a
#   plotmath string: IS-MP (epsilon^y = 0) against IS-MP (epsilon^y > 0).
#   No t subscripts on a one-period panel, as in Whelan and Peia.

D_01_08_tag_fn <- function(name, sym, rel) {
  paste0("'", name, " ('*", sym, " ", rel, "*')'")
}

#### D_02: Lecture 1.1 Building Blocks #########################################
# Note: One curve per panel. A moved curve shows both positions solid, the
#   baseline in muted grey, with arrows between them (Whelan figs 1.6, 1.7,
#   1.9).

###### D_02_01: IS Curve Block #################################################
# Note: Real rate against the output gap. Whelan fig. 1.5, eq. 1.5.

D_02_01_is_block_fn <- function(par, eps_y, ref = NULL, ref_eps_y = 0) {

  pal    <- B_03_05_palette_vec
  r_grid <- seq(par$r_star - 5, par$r_star + 5, length.out = 101)
  x_lim  <- c(-8, 8)
  # Headroom at the top for the names
  y_lim  <- c(par$r_star - 5, par$r_star + 5.8)
  moved  <- abs(eps_y) > 1e-9

  base_df <- data.frame(x = -par$alpha * (r_grid - par$r_star), y = r_grid)
  now_df  <- data.frame(x = base_df$x + eps_y,                  y = r_grid)

  # Ghost: the same curve at the worked example's settings, drawn first
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    T_02_03a_ghost_line_fn(
      data.frame(x = -ref$alpha * (r_grid - ref$r_star) + ref_eps_y,
                 y = r_grid),
      aes(x = x, y = y), colour = pal[["navy"]])
  }

  base_lab <- D_01_08_tag_fn("IS", "epsilon^y", "== 0")
  now_lab  <- if (!moved) {
    "'IS'"
  } else {
    D_01_08_tag_fn("IS", "epsilon^y", if (eps_y > 0) "> 0" else "< 0")
  }

  arr_lst <- if (moved) {
    D_01_07_shift_arrows_fn(base_df, now_df, x_lim, y_lim, pal[["navy"]])
  }

  plt <- ggplot() +
    # No vertical zero: the leader to (y*, r*) already runs up it
    T_02_02_zero_fn(v = FALSE) +
    D_01_03_leader_fn(0, par$r_star, x_lim[1], y_lim[1]) +
    ghost_lyr
  if (moved) {
    plt <- plt +
      geom_line(data = base_df, aes(x = x, y = y),
                colour = pal[["muted"]], linewidth = 1) +
      arr_lst$layer
  }
  plt <- plt +
    geom_line(data = now_df, aes(x = x, y = y),
              colour = pal[["navy"]], linewidth = 1)

  # Names at the top end, reading leftwards, away from the falling line
  avoid_lst <- c(list(now_df),
                 if (moved) list(base_df, arr_lst$pts))
  spec_lst  <- c(
    list(list(pt = D_01_04_endpt_fn(now_df, x_lim, y_lim, "top"),
              label = now_lab, colour = pal[["navy"]], anchor = "right")),
    if (moved) {
      list(list(pt = D_01_04_endpt_fn(base_df, x_lim, y_lim, "top"),
                label = base_lab, colour = pal[["muted"]], anchor = "right"))
    })

  plt +
    D_01_06_endlabs_fn(spec_lst, x_lim, y_lim, avoid_lst) +
    scale_y_continuous(breaks = par$r_star, labels = expression(r^"*")) +
    scale_x_continuous(breaks = 0, labels = expression(y == y^"*")) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    labs(title = "Investment\u2013Saving (IS) Curve",
         x = expression(bold("Output gap (" * y[t] - y[t]^"*" * ")")),
         y = expression(bold("Real interest rate (" * i[t] - pi[t] * ")"))) +
    D_01_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

###### D_02_02: Phillips Curve Block ###########################################
# Note: Inflation against the output gap at a given pi^e. Whelan eq. 1.3,
#   figs 1.6 (supply shock) and 1.7 (expectations).

D_02_02_pc_block_fn <- function(par, pi_e, eps_pi, ref = NULL,
                                ref_pi_e = 0, ref_eps_pi = 0) {

  pal    <- B_03_05_palette_vec
  y_grid <- seq(-8, 8, length.out = 101)
  x_lim  <- c(-8, 8)

  base_df <- data.frame(x = y_grid, y = par$pi_star + par$gamma * y_grid)
  now_df  <- data.frame(x = y_grid, y = pi_e + par$gamma * y_grid + eps_pi)
  moved   <- abs(pi_e - par$pi_star) > 1e-9 || abs(eps_pi) > 1e-9

  # Target plus six either side, opened to hold both curves and a name
  y_lim  <- range(par$pi_star - 6, par$pi_star + 6, base_df$y, now_df$y)
  y_lim  <- y_lim + c(0, 0.20) * diff(y_lim)

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    T_02_03a_ghost_line_fn(
      data.frame(x = y_grid,
                 y = ref_pi_e + ref$gamma * y_grid + ref_eps_pi),
      aes(x = x, y = y), colour = pal[["blue"]])
  }

  # The bracket names whichever moved: epsilon^pi (fig. 1.6) or pi^e (1.7)
  shifted_e <- abs(pi_e - par$pi_star) > 1e-9
  tag_lst   <- if (shifted_e) {
    list(sym = "pi^e", base = "== pi^'*'",
         now = if (pi_e > par$pi_star) "> pi^'*'" else "< pi^'*'")
  } else {
    list(sym = "epsilon^pi", base = "== 0",
         now = if (eps_pi > 0) "> 0" else "< 0")
  }
  base_lab <- D_01_08_tag_fn("PC", tag_lst$sym, tag_lst$base)
  now_lab  <- if (!moved) "'PC'" else
    D_01_08_tag_fn("PC", tag_lst$sym, tag_lst$now)

  arr_lst <- if (moved) {
    D_01_07_shift_arrows_fn(base_df, now_df, x_lim, y_lim, pal[["blue"]])
  }

  plt <- ggplot() +
    T_02_02_zero_fn(v = FALSE) +
    D_01_03_leader_fn(0, par$pi_star, x_lim[1], y_lim[1]) +
    ghost_lyr
  if (moved) {
    plt <- plt +
      geom_line(data = base_df, aes(x = x, y = y),
                colour = pal[["muted"]], linewidth = 1) +
      arr_lst$layer
  }
  plt <- plt +
    geom_line(data = now_df, aes(x = x, y = y),
              colour = pal[["blue"]], linewidth = 1)

  # Names at the right-hand end, reading back into the panel
  avoid_lst <- c(list(now_df),
                 if (moved) list(base_df, arr_lst$pts))
  spec_lst  <- c(
    list(list(pt = D_01_04_endpt_fn(now_df, x_lim, y_lim, "right"),
              label = now_lab, colour = pal[["blue"]], anchor = "right")),
    if (moved) {
      list(list(pt = D_01_04_endpt_fn(base_df, x_lim, y_lim, "right"),
                label = base_lab, colour = pal[["muted"]], anchor = "right"))
    })

  plt +
    D_01_06_endlabs_fn(spec_lst, x_lim, y_lim, avoid_lst) +
    scale_y_continuous(breaks = par$pi_star, labels = expression(pi^"*")) +
    scale_x_continuous(breaks = 0, labels = expression(y == y^"*")) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    labs(title = "Phillips Curve (PC)",
         x = expression(bold("Output gap (" * y[t] - y[t]^"*" * ")")),
         y = expression(bold("Inflation (" * pi[t] * ")"))) +
    D_01_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

###### D_02_03: Policy Rule Block ##############################################
# Note: Policy rate against inflation (Whelan eq. 1.8), with the neutral line
#   i = r* + pi along which the real rate is r*. The rule is steeper than it
#   iff beta_pi > 1: the Taylor principle. With the ZLB on, the rule is flat
#   at zero below pi^ZLB and the desired rate is left to the time paths.

D_02_03_mp_block_fn <- function(par, ref = NULL) {

  pal    <- B_03_05_palette_vec
  p_grid <- seq(par$pi_star - 4, par$pi_star + 4, length.out = 101)
  at_zlb <- isTRUE(par$zlb)
  floor_fn <- function(i_vec, pars) {
    if (isTRUE(pars$zlb)) pmax(i_vec, 0) else i_vec
  }
  i_rule <- floor_fn(C_01_01_rule_rate_fn(p_grid, 0, par), par)
  # Headroom above the rule for its name
  i_lim  <- range(par$r_star + p_grid, i_rule)
  i_lim  <- i_lim + c(0, 0.14) * diff(i_lim)
  i_star <- par$r_star + par$pi_star
  pi_zlb <- C_01_06_diagnostics_fn(par)$pi_zlb
  p_lim   <- range(p_grid)
  neut_df <- data.frame(x = p_grid, y = par$r_star + p_grid)
  rule_df <- data.frame(x = p_grid, y = i_rule)

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    T_02_03a_ghost_line_fn(
      data.frame(x = p_grid,
                 y = floor_fn(C_01_01_rule_rate_fn(p_grid, 0, ref), ref)),
      aes(x = x, y = y), colour = pal[["green"]])
  }

  # Two different lines, so neither takes a bracket
  lab_lyr <- D_01_06_endlabs_fn(
    list(
      list(pt = D_01_04_endpt_fn(neut_df, p_lim, i_lim, "right"),
           label = "'Neutral rate'", colour = pal[["muted"]],
           anchor = "right"),
      list(pt = D_01_04_endpt_fn(rule_df, p_lim, i_lim, "right"),
           label = "'MP'", colour = pal[["green"]], anchor = "right")),
    p_lim, i_lim, list(neut_df, rule_df))

  ggplot() +
    T_02_02_zero_fn() +
    D_01_03_leader_fn(par$pi_star, i_star, p_lim[1], i_lim[1]) +
    (if (at_zlb) {
      D_01_03_leader_fn(pi_zlb, 0, p_lim[1], i_lim[1])
    }) +
    ghost_lyr +
    geom_line(data = neut_df, aes(x = x, y = y),
              colour = pal[["muted"]], linewidth = 1) +
    geom_line(data = rule_df, aes(x = x, y = y),
              colour = pal[["green"]], linewidth = 1) +
    lab_lyr +
    scale_x_continuous(
      breaks = if (at_zlb) c(pi_zlb, par$pi_star) else par$pi_star,
      labels = if (at_zlb) {
        c(expression(pi^"ZLB"), expression(pi^"*"))
      } else {
        expression(pi^"*")
      }) +
    scale_y_continuous(breaks = c(0, i_star),
                       labels = c(expression(0),
                                  expression(r^"*" + pi^"*"))) +
    coord_cartesian(xlim = p_lim, ylim = i_lim, expand = FALSE) +
    labs(title = "Monetary Policy (MP) Rule",
         x = expression(bold("Inflation (" * pi[t] * ")")),
         y = expression(bold("Policy rate (" * i[t] * ")"))) +
    D_01_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

#### D_03: Diagram and Time Paths ##############################################
# Note: The two main figures from lecture 1.2 on.

###### D_03_01: IS-MP / PC Diagram #############################################
# Note: Whelan fig. 1.10 and Peia [M1 68, 71, 72], drawn from the simulator
#   for the period shown: both curves, their pre-shock positions where a
#   shock has moved one, the path so far, and the region where the ZLB binds.
#   Leaders mark the resting point (pi*, y*) and, once away from it, the
#   current point (pi_t, y_t), as on [M1 72].

D_03_01_diagram_fn <- function(sim_df, par, t_show, ghost_df = NULL) {

  pal <- B_03_05_palette_vec
  cap <- B_03_10_diagram_cap_num

  # Axes fixed over the whole run so they hold still while animating
  y_vals <- c(sim_df$mod_output_gap_val, ghost_df$mod_output_gap_val)
  p_vals <- c(sim_df$mod_inflation_val, ghost_df$mod_inflation_val)
  x_lim  <- D_01_02_limits_fn(y_vals, 0, 3, cap)
  p_lim  <- D_01_02_limits_fn(p_vals, par$pi_star, 3, cap)
  y_grid <- seq(x_lim[1] - 1, x_lim[2] + 1, length.out = 400)
  p_grid <- seq(p_lim[1] - 1, p_lim[2] + 1, length.out = 400)

  now <- sim_df[sim_df$mod_period_tm == t_show, ]

  is_now  <- C_01_04_is_mp_curve_fn(p_grid, now$shk_demand_val,
                                    now$shk_markup_val, par)
  pc_now  <- C_01_05_pc_curve_fn(y_grid, now$mod_expected_inf_val,
                                 now$shk_supply_val, par)
  is_base <- C_01_04_is_mp_curve_fn(p_grid, 0, 0, par)
  pc_base <- C_01_05_pc_curve_fn(y_grid, par$pi_star, 0, par)

  trail <- sim_df[sim_df$mod_period_tm <= t_show, ]

  # One line or two, per curve
  is_moved <- abs(now$shk_demand_val) > 1e-9 || abs(now$shk_markup_val) > 1e-9
  pc_moved <- abs(now$mod_expected_inf_val - par$pi_star) > 1e-9 ||
    abs(now$shk_supply_val) > 1e-9

  # Label and arrow helpers work in plain x and y
  xy_fn <- function(df) data.frame(x = df$crv_output_gap_val,
                                   y = df$crv_inflation_val)

  # IS-MP can rise to the right (ZLB, beta_pi < 1), so read the side off it
  side_fn <- function(df) {
    vis <- df[df$x >= x_lim[1] & df$x <= x_lim[2] &
                df$y >= p_lim[1] & df$y <= p_lim[2], ]
    if (nrow(vis) < 2) return("left")
    if (vis$x[which.max(vis$y)] <= vis$x[which.min(vis$y)]) "right" else "left"
  }

  plt <- ggplot()

  # ZLB region: rule rate below zero, boundary
  # pi = pi* + (-r* - pi* - beta_y y) / beta_pi
  if (isTRUE(par$zlb) && par$beta_pi > 0) {
    zlb_df <- data.frame(
      x    = y_grid,
      ymax = par$pi_star + (-par$r_star - par$pi_star - par$beta_y * y_grid) /
        par$beta_pi
    )
    zlb_df$ymax <- pmax(zlb_df$ymax, p_lim[1] - 1)
    plt <- plt +
      geom_ribbon(data = zlb_df, aes(x = x, ymin = p_lim[1] - 1, ymax = ymax),
                  fill = pal[["light"]], alpha = 0.35) +
      geom_line(data = zlb_df, aes(x = x, y = ymax), linetype = "dotted",
                colour = pal[["blue"]])
    in_x <- zlb_df$x >= x_lim[1] & zlb_df$x <= x_lim[2]
    if (any(zlb_df$ymax[in_x] > p_lim[1])) {
      plt <- plt +
        annotate("text", x = x_lim[1] + 0.03 * diff(x_lim),
                 y = p_lim[1] + 0.04 * diff(p_lim), hjust = 0, vjust = 0,
                 label = "Zero lower bound binds", size = 3.8,
                 colour = pal[["blue"]])
    }
  }

  # Resting point: the crossing of the two baseline curves
  plt <- plt +
    D_01_03_leader_fn(0, par$pi_star, x_lim[1], p_lim[1])

  is_arr <- if (is_moved) {
    D_01_07_shift_arrows_fn(xy_fn(is_base), xy_fn(is_now),
                            x_lim, p_lim, pal[["navy"]])
  }
  pc_arr <- if (pc_moved) {
    D_01_07_shift_arrows_fn(xy_fn(pc_base), xy_fn(pc_now),
                            x_lim, p_lim, pal[["blue"]])
  }
  if (is_moved) {
    plt <- plt +
      geom_path(data = is_base, aes(crv_output_gap_val, crv_inflation_val),
                colour = pal[["muted"]], linewidth = 1.2) +
      is_arr$layer
  }
  if (pc_moved) {
    plt <- plt +
      geom_path(data = pc_base, aes(crv_output_gap_val, crv_inflation_val),
                colour = pal[["muted"]], linewidth = 1.2) +
      pc_arr$layer
  }

  plt <- plt +
    geom_path(data = is_now, aes(crv_output_gap_val, crv_inflation_val),
              colour = pal[["navy"]], linewidth = 1.2) +
    geom_path(data = pc_now, aes(crv_output_gap_val, crv_inflation_val),
              colour = pal[["blue"]], linewidth = 1.2)

  if (!is.null(ghost_df)) {
    ghost_trail <- ghost_df[ghost_df$mod_period_tm <= t_show, ]
    if (nrow(ghost_trail) > 1) {
      plt <- plt +
        geom_path(data = ghost_trail,
                  aes(mod_output_gap_val, mod_inflation_val),
                  colour = "grey60", linetype = "22", linewidth = 0.6)
    }
    plt <- plt +
      geom_point(data = ghost_trail[nrow(ghost_trail), ],
                 aes(mod_output_gap_val, mod_inflation_val),
                 colour = "grey60", size = 3)
  }

  if (nrow(trail) > 1) {
    plt <- plt +
      geom_path(data = trail, aes(mod_output_gap_val, mod_inflation_val),
                colour = pal[["muted"]], linewidth = 0.5)
  }
  plt <- plt +
    geom_point(data = trail, aes(mod_output_gap_val, mod_inflation_val),
               colour = pal[["muted"]], size = 1.8) +
    geom_point(data = now, aes(mod_output_gap_val, mod_inflation_val),
               colour = "white", fill = pal[["green"]], shape = 21,
               size = 5.5, stroke = 1.2) +
    annotate("text", x = now$mod_output_gap_val, y = now$mod_inflation_val,
             label = paste0("t = ", t_show), hjust = 1.75, vjust = -1.2,
             colour = pal[["ink"]], size = 4.2)

  # IS-MP is named at its top end, PC at its right-hand end (Whelan 1.10)
  is_sym  <- if (abs(now$shk_markup_val) > 1e-9) {
    "bar(mu)^B"
  } else {
    "epsilon^y"
  }
  is_val  <- if (abs(now$shk_markup_val) > 1e-9) {
    now$shk_markup_val
  } else {
    now$shk_demand_val
  }
  is_now_lab <- if (!is_moved) {
    D_01_08_tag_fn("IS-MP", "pi^'*'", "== pi^e")
  } else {
    D_01_08_tag_fn("IS-MP", is_sym, if (is_val > 0) "> 0" else "< 0")
  }
  pc_sym <- if (abs(now$mod_expected_inf_val - par$pi_star) > 1e-9) {
    "pi^e"
  } else {
    "epsilon^pi"
  }
  pc_rel <- if (pc_sym == "pi^e") {
    if (now$mod_expected_inf_val > par$pi_star) "> pi^'*'" else "< pi^'*'"
  } else {
    if (now$shk_supply_val > 0) "> 0" else "< 0"
  }
  pc_now_lab <- if (!pc_moved) {
    D_01_08_tag_fn("PC", "pi^e", "== pi^'*'")
  } else {
    D_01_08_tag_fn("PC", pc_sym, pc_rel)
  }

  # The period marker is in the way too
  avoid_lst <- c(list(xy_fn(is_now), xy_fn(pc_now),
                      data.frame(x = now$mod_output_gap_val,
                                 y = now$mod_inflation_val)),
                 if (is_moved) list(xy_fn(is_base), is_arr$pts),
                 if (pc_moved) list(xy_fn(pc_base), pc_arr$pts))

  spec_lst <- c(
    list(list(pt = D_01_04_endpt_fn(xy_fn(is_now), x_lim, p_lim, "top"),
              label = is_now_lab, colour = pal[["navy"]],
              anchor = side_fn(xy_fn(is_now))),
         list(pt = D_01_04_endpt_fn(xy_fn(pc_now), x_lim, p_lim, "right"),
              label = pc_now_lab, colour = pal[["blue"]], anchor = "right")),
    if (is_moved) {
      list(list(pt = D_01_04_endpt_fn(xy_fn(is_base), x_lim, p_lim, "top"),
                label = D_01_08_tag_fn("IS-MP", is_sym, "== 0"),
                colour = pal[["muted"]], anchor = side_fn(xy_fn(is_base))))
    },
    if (pc_moved) {
      list(list(pt = D_01_04_endpt_fn(xy_fn(pc_base), x_lim, p_lim, "right"),
                label = D_01_08_tag_fn(
                  "PC", pc_sym, if (pc_sym == "pi^e") "== pi^'*'" else "== 0"),
                colour = pal[["muted"]], anchor = "right"))
    })

  plt <- plt + D_01_06_endlabs_fn(spec_lst, x_lim, p_lim, avoid_lst)

  # pi^ZLB is a single rate only when beta_y = 0
  mark_zlb <- isTRUE(par$zlb) && par$beta_pi > 0 && par$beta_y == 0
  pi_zlb   <- C_01_06_diagnostics_fn(par)$pi_zlb
  mark_zlb <- mark_zlb && pi_zlb > p_lim[1] && pi_zlb < p_lim[2] &&
    abs(pi_zlb - par$pi_star) > 0.06 * diff(p_lim)

  # Second pair of leaders to the current point, once clear of the first
  away_lgl <- abs(now$mod_inflation_val - par$pi_star) > 0.06 * diff(p_lim) ||
    abs(now$mod_output_gap_val) > 0.06 * diff(x_lim)
  if (away_lgl) {
    plt <- plt + D_01_03_leader_fn(now$mod_output_gap_val,
                                   now$mod_inflation_val,
                                   x_lim[1], p_lim[1])
  }

  y_at  <- c(par$pi_star, if (mark_zlb) pi_zlb,
             if (away_lgl) now$mod_inflation_val)
  y_lab <- c(expression(pi^"*"), if (mark_zlb) expression(pi^"ZLB"),
             if (away_lgl) expression(pi[t]))
  x_at  <- c(0, if (away_lgl) now$mod_output_gap_val)
  x_lab <- c(expression(y == y^"*"), if (away_lgl) expression(y[t]))

  plt +
    scale_y_continuous(breaks = y_at, labels = y_lab) +
    scale_x_continuous(breaks = x_at, labels = x_lab) +
    coord_cartesian(xlim = x_lim, ylim = p_lim, expand = FALSE) +
    labs(x = expression(bold("Output gap (" * y[t] - y[t]^"*" * ")")),
         y = expression(bold("Inflation (" * pi[t] * ")")),
         caption = paste("Grey: the curves before the shock.",
                         "Grey dots: the path so far.")) +
    D_01_01_theme_fn(grid = "none")
}

###### D_03_02: Time Paths #####################################################
# Note: Four panels. Shaded band marks the shock window; the vertical line
#   marks the period shown in the diagram.

D_03_02_paths_fn <- function(sim_df, par, stage_num, t_show, shock_window,
                             ghost_df = NULL) {

  pal       <- B_03_05_palette_vec
  show_bank <- stage_num >= 1.4
  panels    <- c("Output Gap", "Inflation", "Nominal Interest Rates",
                 "Real Interest Rate")
  real_col  <- if (show_bank) "mod_real_lending_val" else "mod_real_policy_val"

  make_rows <- function(df, panel, series, col) {
    data.frame(plt_period_tm  = df$mod_period_tm,
               plt_value_val  = df[[col]],
               plt_panel_cat  = panel,
               plt_series_cat = series)
  }

  rows_lst <- list(
    make_rows(sim_df, panels[1], "Model", "mod_output_gap_val"),
    make_rows(sim_df, panels[2], "Model", "mod_inflation_val"),
    make_rows(sim_df, panels[2], "Expected Inflation",
              "mod_expected_inf_val"),
    make_rows(sim_df, panels[3], "Model", "mod_policy_rate_val"),
    make_rows(sim_df, panels[4], if (show_bank) "Lending Rate" else "Model",
              real_col)
  )
  if (any(sim_df$mod_zlb_is)) {
    rows_lst <- c(rows_lst, list(
      make_rows(sim_df, panels[3], "Rule Rate (Desired)", "mod_rule_rate_val")
    ))
  }
  if (show_bank) {
    rows_lst <- c(rows_lst, list(
      make_rows(sim_df, panels[3], "Lending Rate", "mod_lending_rate_val"),
      make_rows(sim_df, panels[4], "Model", "mod_real_policy_val")
    ))
  }
  if (!is.null(ghost_df)) {
    rows_lst <- c(rows_lst, list(
      make_rows(ghost_df, panels[1], "Comparison Run", "mod_output_gap_val"),
      make_rows(ghost_df, panels[2], "Comparison Run", "mod_inflation_val"),
      make_rows(ghost_df, panels[3], "Comparison Run", "mod_policy_rate_val"),
      make_rows(ghost_df, panels[4], "Comparison Run", real_col)
    ))
  }

  long_df <- do.call(rbind, rows_lst)
  long_df$plt_panel_cat <- factor(long_df$plt_panel_cat, levels = panels)
  clipped <- any(abs(long_df$plt_value_val) > B_03_11_path_cap_num)
  long_df$plt_value_val[abs(long_df$plt_value_val) >
                          B_03_11_path_cap_num] <- NA

  ref_df <- data.frame(
    plt_panel_cat = factor(panels, levels = panels),
    ref_val       = c(0, par$pi_star, NA, par$r_star)
  )
  if (isTRUE(par$zlb)) ref_df$ref_val[3] <- 0

  series_cols <- c("Model" = pal[["navy"]],
                   "Expected Inflation" = pal[["blue"]],
                   "Lending Rate" = pal[["green"]],
                   "Rule Rate (Desired)" = pal[["muted"]],
                   "Comparison Run" = "grey60")
  series_ltys <- c("Model" = "solid",
                   "Expected Inflation" = "42",
                   "Lending Rate" = "solid",
                   "Rule Rate (Desired)" = "11",
                   "Comparison Run" = "22")
  present <- intersect(names(series_cols), unique(long_df$plt_series_cat))

  ggplot(long_df, aes(x = plt_period_tm, y = plt_value_val,
                      colour = plt_series_cat, linetype = plt_series_cat)) +
    annotate("rect", xmin = shock_window[1] - 0.5,
             xmax = shock_window[2] + 0.5, ymin = -Inf, ymax = Inf,
             fill = pal[["green"]], alpha = 0.15) +
    geom_hline(data = ref_df[!is.na(ref_df$ref_val), ],
               aes(yintercept = ref_val), linetype = "dotted",
               colour = "grey50") +
    (if (!is.null(t_show)) {
      geom_vline(xintercept = t_show, colour = pal[["muted"]],
                 linewidth = 0.3)
    }) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.3, na.rm = TRUE) +
    facet_wrap(~plt_panel_cat, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = series_cols[present], breaks = present) +
    scale_linetype_manual(values = series_ltys[present], breaks = present) +
    labs(x = "Period", y = "Per cent",
         caption = if (clipped) {
           paste0("Values beyond +/-", B_03_11_path_cap_num,
                  " not drawn: the path explodes.")
         }) +
    D_01_01_theme_fn(ratio = NULL)
}

###### D_03_03: Stability and the Taylor Principle #############################
# Note: Two IS-MP curves at two values of beta_pi and one Phillips curve on
#   one panel, as Peia [M1 79]; the PC does not depend on beta_pi. Slide
#   figure only.

D_03_03_stability_fn <- function(par, beta_hi, beta_lo) {

  pal    <- B_03_05_palette_vec
  x_lim  <- c(-4, 4)
  p_lim  <- c(par$pi_star - 3, par$pi_star + 3)
  y_grid <- seq(x_lim[1], x_lim[2], length.out = 400)
  p_grid <- seq(p_lim[1], p_lim[2], length.out = 400)

  is_fn <- function(b) {
    par_b <- par
    par_b$beta_pi <- b
    par_b$zlb     <- FALSE
    df <- C_01_04_is_mp_curve_fn(p_grid, 0, 0, par_b)
    data.frame(x = df$crv_output_gap_val, y = df$crv_inflation_val)
  }
  is_hi <- is_fn(beta_hi)
  is_lo <- is_fn(beta_lo)
  pc_df <- C_01_05_pc_curve_fn(y_grid, par$pi_star, 0, par)
  pc_df <- data.frame(x = pc_df$crv_output_gap_val,
                      y = pc_df$crv_inflation_val)

  tag_fn <- function(b) {
    D_01_08_tag_fn("IS-MP", "beta[pi]", paste0("== ", format(b)))
  }

  avoid_lst <- list(is_hi, is_lo, pc_df)

  ggplot() +
    D_01_03_leader_fn(0, par$pi_star, x_lim[1], p_lim[1]) +
    geom_path(data = is_hi, aes(x = x, y = y),
              colour = pal[["navy"]], linewidth = 1.2) +
    geom_path(data = is_lo, aes(x = x, y = y),
              colour = pal[["muted"]], linewidth = 1.2) +
    geom_path(data = pc_df, aes(x = x, y = y),
              colour = pal[["blue"]], linewidth = 1.2) +
    D_01_06_endlabs_fn(
      list(
        list(pt = D_01_04_endpt_fn(is_hi, x_lim, p_lim, "top"),
             label = tag_fn(beta_hi), colour = pal[["navy"]],
             anchor = "right"),
        list(pt = D_01_04_endpt_fn(is_lo, x_lim, p_lim, "top"),
             label = tag_fn(beta_lo), colour = pal[["muted"]],
             anchor = "left"),
        list(pt = D_01_04_endpt_fn(pc_df, x_lim, p_lim, "right"),
             label = "'PC'", colour = pal[["blue"]], anchor = "right")),
      x_lim, p_lim, avoid_lst) +
    scale_y_continuous(breaks = par$pi_star, labels = expression(pi^"*")) +
    scale_x_continuous(breaks = 0, labels = expression(y == y^"*")) +
    coord_cartesian(xlim = x_lim, ylim = p_lim, expand = FALSE) +
    labs(title = paste0("The Taylor Principle: IS-MP Slopes Down Only When ",
                        "the Rule Raises the Rate More Than One for One"),
         x = expression(bold("Output gap (" * y[t] - y[t]^"*" * ")")),
         y = expression(bold("Inflation (" * pi[t] * ")"))) +
    D_01_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

#### D_04: Narration ###########################################################
# Note: Commentary on the period shown.

###### D_04_01: Narrate One Period #############################################
# Note: A few sentences for period t: the shock, expectations, output and
#   inflation, the central bank, and the mechanism when one stands out.

D_04_01_narrate_fn <- function(sim_df, par, t_show, stage_num, shock_window,
                               diag) {

  f1  <- function(x) formatC(x, format = "f", digits = 1)
  pc  <- function(x) paste0(f1(x), "%")
  sg  <- function(x) paste0(if (x > 0) "+" else "−", f1(abs(x)))
  lam <- format(round(par$lambda, 2))
  now <- sim_df[sim_df$mod_period_tm == t_show, ]

  any_shock <- any(sim_df$shk_demand_val != 0 | sim_df$shk_supply_val != 0 |
                     sim_df$shk_expect_val != 0 | sim_df$shk_markup_val != 0)

  # Steady state and no-shock cases
  if (t_show == 0) {
    return(HTML(paste0(
      "The economy starts in its steady state: output at potential, ",
      "inflation at its ", pc(par$pi_star), " target and the policy rate ",
      "at ", pc(par$r_star + par$pi_star), " (r* + π*).",
      if (any_shock) " Press ▶ above the diagram to run the shock." else
        " Move a shock slider or choose a scenario, then press ▶."
    )))
  }
  if (!any_shock) {
    return(HTML(paste(
      "No shock is switched on, so nothing moves: every period repeats",
      "the steady state."
    )))
  }
  if (t_show < shock_window[1]) {
    return(HTML(paste0(
      "No shock yet: it arrives in period ", shock_window[1], ". The ",
      "economy is still in its steady state."
    )))
  }

  # Off the figures
  cap <- B_03_11_path_cap_num
  if (abs(now$mod_inflation_val) > cap || abs(now$mod_output_gap_val) > cap) {
    return(HTML(paste0(
      "Inflation is now ", pc(now$mod_inflation_val), " and the output gap ",
      pc(now$mod_output_gap_val), ", outside the range of the figures. ",
      "Each period's inflation feeds next period's ",
      "expectations, and nothing in the model pulls it back: the path ",
      "keeps diverging."
    )))
  }

  prev     <- sim_df[sim_df$mod_period_tm == t_show - 1, ]
  in_shock <- t_show <= shock_window[2]
  out      <- character(0)

  # 1. What hit the economy this period
  hits <- character(0)
  if (now$shk_demand_val != 0) {
    neg  <- now$shk_demand_val < 0
    hits <- c(hits, paste0(
      "a ", if (neg) "negative" else "positive", " demand shock ",
      "(ε<sup>y</sup> = ", sg(now$shk_demand_val), ") shifts the IS-MP ",
      "curve ", if (neg) "left" else "right"
    ))
  }
  if (now$shk_supply_val != 0) {
    hits <- c(hits, paste0(
      "an inflation shock (ε<sup>π</sup> = ", sg(now$shk_supply_val),
      ") shifts the Phillips curve ",
      if (now$shk_supply_val > 0) "up" else "down"
    ))
  }
  if (now$shk_expect_val != 0) {
    hits <- c(hits, paste0(
      "expected inflation jumps by ", sg(now$shk_expect_val), " points, ",
      "shifting the Phillips curve ",
      if (now$shk_expect_val > 0) "up" else "down"
    ))
  }
  if (now$shk_markup_val != 0) {
    up   <- now$shk_markup_val > 0
    hits <- c(hits, paste0(
      "banks ", if (up) "raise" else "cut", " their lending mark-up by ",
      f1(abs(now$shk_markup_val)), " points, shifting the IS-MP curve ",
      if (up) "left" else "right"
    ))
  }
  if (length(hits) > 0) {
    lead <- if (t_show == shock_window[1]) "The shock hits: " else
      "The shock continues: "
    out <- c(out, paste0(lead, paste(hits, collapse = "; "), "."))
  } else {
    out <- c(out, paste0(
      "The shock ended in period ", shock_window[2], ", so the curve it ",
      "moved is back in place. The Phillips curve still moves because ",
      "expectations are adjusting."
    ))
  }

  # 2. Expectations
  gap_e <- now$mod_expected_inf_val - par$pi_star
  if (abs(gap_e) >= 0.05) {
    src <- if (par$lambda == 0) {
      paste0("last period's inflation of ", pc(prev$mod_inflation_val))
    } else {
      paste0("a mix of the target and last period's inflation of ",
             pc(prev$mod_inflation_val), " (λ = ", lam, ")")
    }
    if (now$shk_expect_val != 0) src <- paste0(src, ", plus the jump")
    out <- c(out, paste0(
      "People expect inflation of ", pc(now$mod_expected_inf_val),
      ", based on ", src, ", so the Phillips curve sits ",
      if (gap_e > 0) "above" else "below", " its starting position."
    ))
  }

  # 3. Where output and inflation end up
  y        <- now$mod_output_gap_val
  p        <- now$mod_inflation_val
  gap_now  <- abs(p - par$pi_star)
  gap_prev <- abs(prev$mod_inflation_val - par$pi_star)
  y_txt <- if (abs(y) < 0.05) "Output is at potential" else
    paste0("Output is ", f1(abs(y)), "% ", if (y < 0) "below" else "above",
           " potential")
  p_dir <- if (gap_now < 0.05) {
    ", on target"
  } else if (gap_now < gap_prev - 0.02) {
    ", heading back towards target"
  } else if (gap_now > gap_prev + 0.02) {
    ", moving away from target"
  } else {
    ""
  }
  out <- c(out, paste0(y_txt, " and inflation is ", pc(p), p_dir, "."))

  # 4. The central bank and the real rate that matters for spending
  if (now$mod_zlb_is) {
    pol <- paste0(
      "The policy rule calls for ", pc(now$mod_rule_rate_val), ", but ",
      "rates cannot go below zero, so the policy rate is stuck at 0%."
    )
  } else {
    di  <- now$mod_policy_rate_val - prev$mod_policy_rate_val
    pol <- if (abs(di) < 0.05) {
      paste0("The policy rate stays at ", pc(now$mod_policy_rate_val), ".")
    } else {
      paste0("The central bank ", if (di > 0) "raises" else "cuts",
             " the policy rate from ", pc(prev$mod_policy_rate_val), " to ",
             pc(now$mod_policy_rate_val), ".")
    }
  }
  bank   <- stage_num >= 1.4
  real   <- if (bank) now$mod_real_lending_val else now$mod_real_policy_val
  stance <- if (real > par$r_star + 0.05) {
    "above r*, which holds spending back"
  } else if (real < par$r_star - 0.05) {
    "below r*, which supports spending"
  } else {
    "at r*, so policy is neutral"
  }
  pol <- paste0(pol, " The real ", if (bank) "lending " else "",
                "interest rate is ", pc(real), ", ", stance, ".")
  if (bank && abs(now$mod_markup_val) >= 0.05) {
    pol <- paste0(pol, " Banks lend at ", pc(now$mod_lending_rate_val), ", ",
                  f1(abs(now$mod_markup_val)), " points ",
                  if (now$mod_markup_val > 0) "above" else "below",
                  " the policy rate.")
  }
  out <- c(out, pol)

  if (stage_num < 1.3 && now$mod_policy_rate_val < 0) {
    out <- c(out, paste("Negative rates are allowed here; stage 3 adds",
                        "the zero lower bound."))
  }

  # 5. The mechanism at work, when one stands out
  spiral <- utils::tail(sim_df$mod_zlb_is, 1) && diag$persistence_zlb >= 1
  mech <- if (now$mod_zlb_is && p < prev$mod_inflation_val && spiral) {
    paste("At the floor, lower inflation means a higher real rate: output",
          "falls, expectations fall with inflation, and the spiral feeds",
          "itself.")
  } else if (now$mod_zlb_is && p < prev$mod_inflation_val) {
    paste("At the floor, falling inflation raises the real rate and pushes",
          "output down further, and the central bank cannot cut any more.")
  } else if (now$mod_zlb_is) {
    paste("The central bank cannot cut any further. Only higher inflation,",
          "which lowers the real rate, can lift the economy off the floor.")
  } else if (par$beta_pi < 1 && gap_now > gap_prev + 0.02) {
    paste("With β<sub>π</sub> below one, the real rate falls as inflation",
          "rises, so demand adds fuel and inflation drifts further from",
          "target.")
  } else if (in_shock && now$shk_supply_val > 0 && y < 0) {
    paste("This is the central bank's dilemma: raising rates to fight",
          "inflation pushes output below potential.")
  } else if (!in_shock && gap_now < gap_prev) {
    if (par$lambda == 0) {
      paste("With no new shocks the economy converges, but slowly, because",
            "expectations only catch up with last period's inflation.")
    } else {
      paste("With no new shocks the economy converges, and anchoring pulls",
            "expectations towards the target each period.")
    }
  } else {
    NULL
  }
  if (!is.null(mech)) out <- c(out, mech)

  HTML(paste(out, collapse = " "))
}

################################################################################
## E: User Interface ###########################################################
################################################################################
# Note: bslib page: controls in a sidebar, figures in cards.

#### E_01: Theme and Styles ####################################################
# Note: Dublin colours and IBM Plex Sans, as on the slides.

###### E_01_01: Bootstrap Theme ################################################
# Note: Plex from Google Fonts when online, else the system font.

E_01_01_theme_lst <- bs_theme(
  version   = 5,
  primary   = B_03_05_palette_vec[["blue"]],
  secondary = B_03_05_palette_vec[["muted"]],
  success   = B_03_05_palette_vec[["green"]],
  info      = B_03_05_palette_vec[["light"]],
  bg        = "#FFFFFF",
  fg        = B_03_05_palette_vec[["ink"]],
  base_font = font_collection(font_google("IBM Plex Sans", local = FALSE),
                              "Segoe UI", "Helvetica", "Arial", "sans-serif"),
  heading_font = font_collection(font_google("IBM Plex Sans", local = FALSE),
                                 "Segoe UI", "Helvetica", "Arial", "sans-serif")
)

###### E_01_02: Extra CSS ######################################################
# Note: Tiles, equation tables, prompt, story, narration, controls, tabs, nav.

E_01_02_css_chr <- "
  /* Cards and panels are square: they organise the page, not decorate it */
  .card, .card-header, .card-body, .card-footer, .bslib-card,
  .bslib-sidebar-layout, .navset-card-tab, .nav-tabs .nav-link,
  .accordion-item, .accordion-button, .story, .prompt, .problem,
  .stat-tile, .stat-input input, .btn, .form-control, .form-select,
  .badge { border-radius: 0 !important; }
  .card, .bslib-card { border: none; box-shadow: none; }
  .card-header { border-bottom: none; background: transparent;
    color: #0056A4; font-weight: 700; }
  .bslib-navs-card-title { display: flex; align-items: center; gap: 1rem;
    flex-wrap: wrap; }
  .bslib-navs-card-title .nav-tabs { border-bottom: none; margin: 0; }
  .nav-tabs .nav-link { margin-bottom: 0; }
  /* The stage name is the first tab: selected while the card is folded */
  .eq-stage { padding: 0.5rem 1rem; cursor: pointer; font-weight: 600;
    color: #0056A4; }
  .eq-stage:hover { background: #F2F6F9; }
  .eq-folded > .bslib-navs-card-title > .eq-stage { background: #0056A4;
    color: #FFFFFF; font-weight: 700; }
  .eq-folded > .tab-content { display: none; }
  .eq-folded .nav-tabs .nav-link.active { background: transparent;
    color: #6C757D; border-color: transparent; }
  .eq-folded .nav-tabs .nav-link.active:hover { color: #0056A4;
    background: #F2F6F9; }
  .fig-r32 { width: 100%; }
  @supports (aspect-ratio: 3 / 2) {
    .fig-r32 > .shiny-plot-output { height: auto !important;
      aspect-ratio: 3 / 2; min-height: 0; overflow: hidden; }
  }
  .card-footer { border-top: none; background: transparent; }
  .stat-row { display: flex; flex-wrap: wrap; gap: 0.75rem; }
  .stat-caption { font-size: 0.8rem; color: #6C757D; margin: 0.2rem 0; }
  .stat-slot { flex: 1 1 11rem; display: flex; }
  .stat-slot > * { flex: 1 1 auto; }
  .stat-input .form-group { margin-bottom: 0; }
  .stat-input input { font-size: 1.35rem; font-weight: 600; color: #04204C;
    padding: 0.05rem 0.4rem; border: 1px solid #D8E0E6; background: #FFFFFF; }
  .stat-hint { font-size: 0.72rem; color: #6C757D; font-style: italic; }
  .side-qr { flex: 0 0 auto; }
  .side-qr img { width: 56px; height: 56px; display: block; }
  .sidebar-qr { text-align: center; margin-top: 1rem; font-size: 0.8rem; }
  .sidebar-qr img { width: 110px; height: 110px; }
  .sidebar-qr-name { font-weight: 700; color: #04204C; margin-top: 0.3rem; }
  .bslib-page-title { display: flex; align-items: center; gap: 0.3rem;
    width: 100%; }
  .title-qr { margin-left: auto; }
  .title-qr img { height: 40px; width: 40px; }
  .stat-tile { flex: 1 1 11rem; border: none;
    border-left: 5px solid #0056A4; border-radius: 4px;
    padding: 0.45rem 0.8rem; background: #F2F6F9; }
  .stat-tile.good { border-left-color: #61B77C; }
  .stat-tile.bad  { border-left-color: #04204C; }
  .stat-label { font-size: 0.8rem; color: #6C757D; }
  .stat-value { font-size: 1.5rem; font-weight: 600; color: #04204C; }
  .stat-note  { font-size: 0.78rem; color: #212529; }
  .eq-table td { padding: 0.15rem 0.9rem 0.15rem 0; vertical-align: middle; }
  .eq-label { color: #6C757D; font-size: 0.85rem; white-space: nowrap; }
  .eq-group { overflow-x: auto; }
  .eq-group-title { font-weight: 700; color: #04204C; font-size: 0.9rem;
    border-bottom: 2px solid #D8E0E6; margin-bottom: 0.3rem; }
  .eq-table td { border-bottom: 1px solid #F2F6F9; }
  .eq-flag { display: inline-block; font-size: 0.65rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.04em; color: #FFFFFF;
    padding: 0.05rem 0.35rem; border-radius: 3px; margin-left: 0.35rem; }
  .eq-new     { background: #61B77C; }
  .eq-changed { background: #0056A4; }
  .eq-legend  { font-size: 0.78rem; color: #6C757D; margin-top: 0.3rem; }
  .eq-explain { width: 100%; table-layout: fixed; }
  .eq-explain td.eq-label { width: 22%; white-space: normal; }
  .eq-explain td.eq-math { width: 42%; }
  .eq-explain td.chg-note { width: 36%; }
  .eq-explain td.eq-group-title { padding-top: 0.6rem; }
  .nota-table { width: 100%; font-size: 0.88rem; }
  .nota-table td { padding: 0.25rem 0.6rem 0.25rem 0; vertical-align: top;
    border-bottom: 1px solid #F2F6F9; }
  .nota-table td:first-child { white-space: nowrap; width: 6.5rem; }
  .chg-was  { color: #6C757D; font-size: 0.85rem; }
  .chg-note { font-size: 0.85rem; }
  .prompt { background: #F2F6F9; border-left: 4px solid #61B77C;
    padding: 0.6rem 0.9rem; border-radius: 4px; font-size: 0.95rem; }
  .problem { background: #F2F6F9; border-left: 4px solid #04204C;
    padding: 0.6rem 0.9rem; border-radius: 4px; }
  .story { background: #F2F6F9; border-radius: 4px; font-size: 0.86rem;
    padding: 0.55rem 0.75rem; margin: -0.4rem 0 0.7rem 0; }
  .story-key { font-weight: 700; color: #04204C; margin-top: 0.45rem; }
  .story dl { margin: 0.2rem 0 0 0; }
  .story dt { font-weight: 600; color: #0056A4; }
  .story dd { margin: 0 0 0.3rem 0; }
  .narrative { border: 1px solid #D8E0E6; border-left: 4px solid #61B77C;
    border-radius: 4px; padding: 0.55rem 0.75rem; font-size: 0.88rem;
    min-height: 9rem; margin-bottom: 0.7rem; background: #FFFFFF; }
  .nar-head { font-weight: 700; color: #04204C; margin-bottom: 0.2rem; }
  .ctl { margin-bottom: 0.4rem; }
  .ctl-label { font-size: 0.88rem; font-weight: 600; color: #0056A4;
    margin-bottom: -0.25rem; }
  .ctl-help { color: #0056A4; cursor: help; font-size: 0.85rem; }
  .ctl-row { display: flex; gap: 0.5rem; align-items: center; }
  .ctl-slider { flex: 1 1 auto; min-width: 0; }
  .ctl-box { flex: 0 0 4.9rem; }
  .ctl .form-group { margin-bottom: 0; }
  .ctl-box input { padding: 0.15rem 0.35rem; font-size: 0.85rem;
    text-align: right; }
  #stage-label, #scenario-label { font-weight: 700; color: #04204C; }
  .sidebar .accordion-button { font-weight: 700; color: #04204C; }
  .sidebar h6 { color: #04204C; font-weight: 700; margin-top: 0.5rem; }
  .title-credit { font-size: 0.8rem; font-weight: 400; margin-left: 0.8rem;
    opacity: 0.8; }
  .title-credit a { color: inherit; }
  .credit { font-size: 0.8rem; color: #6C757D; text-align: center;
    padding: 1rem 0 0.5rem 0; }
  .nav-tabs .nav-link { color: #6C757D; font-weight: 600;
    border-color: transparent; }
  .nav-tabs .nav-link:hover { color: #0056A4; background: #F2F6F9; }
  .nav-tabs .nav-link.active { color: #FFFFFF; font-weight: 700;
    background: #0056A4; border-color: #0056A4; }
  .site-nav { background: #FFFFFF; border-bottom: 1px solid #D8E0E6;
    padding: 1rem 2rem; margin: -0.5rem -0.5rem 0.75rem -0.5rem; }
  .site-nav-row { display: flex; align-items: center; gap: 2rem;
    max-width: 1200px; margin: 0 auto; }
  .site-nav ul { list-style: none; display: flex; justify-content: center;
    gap: 2rem; max-width: 1200px; margin: 0 auto; padding: 0;
    flex-wrap: wrap; }
  .site-nav a { color: #04204C; text-decoration: none; font-weight: 500;
    font-size: 0.95rem; padding-bottom: 0.25rem; position: relative; }
  .site-nav a:hover, .site-nav a.active { color: #0056A4; }
  .site-nav a.active::after { content: ''; position: absolute;
    bottom: -0.5rem; left: 0; right: 0; height: 2px; background: #0056A4; }
  .page-title-wrap { display: flex; width: 100%; align-items: center;
    justify-content: space-between; gap: 1rem; }
  .page-title-text { min-width: 0; }
  .page-byline { color: #6C757D; font-size: 0.95rem; font-weight: 500;
    margin: -0.35rem 0 0.6rem 0; }
  .page-byline a { color: #6C757D; text-decoration: none;
    display: inline-flex; align-items: center; gap: 0.45rem; }
  .page-byline a:hover { color: #0056A4; }
  .page-byline img { width: 24px; height: 24px; border-radius: 50%; }
  @media (max-width: 700px) {
    .site-nav { padding: 0.6rem 0.75rem; }
    .site-nav ul { gap: 1rem; font-size: 0.85rem; }
  }
"

###### E_01_03: MathJax on Tab Switch ##########################################
# Note: Formulas laid out in a hidden tab are the wrong size; re-typeset on
#   show.

E_01_03_mathjax_js_chr <- paste(
  "document.addEventListener('shown.bs.tab', function() {",
  "  if (window.MathJax && MathJax.Hub) {",
  "    MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
  "  }",
  "});"
)

###### E_01_04: Fold the Equations Card #######################################
# Note: The equations card opens folded, so the figures sit high on the
#   page, with the stage name drawn as the selected tab. Clicking a tab
#   opens it; clicking the open tab again, or the stage name, folds it.

E_01_04_eqfold_js_chr <- paste(
  "document.addEventListener('DOMContentLoaded', function () {",
  "  document.querySelectorAll('.card > .bslib-navs-card-title')",
  "    .forEach(function (hdr) {",
  "      var card = hdr.parentElement;",
  "      card.classList.add('eq-folded');",
  "      var name = hdr.querySelector(':scope > :not(.nav)');",
  "      if (name) {",
  "        name.classList.add('eq-stage');",
  "        name.addEventListener('click', function () {",
  "          card.classList.add('eq-folded');",
  "        });",
  "      }",
  "      hdr.querySelectorAll('.nav-link').forEach(function (a) {",
  "        a.addEventListener('click', function () {",
  "          var open = !card.classList.contains('eq-folded');",
  "          if (open && a.classList.contains('active')) {",
  "            card.classList.add('eq-folded');",
  "          } else {",
  "            card.classList.remove('eq-folded');",
  "          }",
  "        }, true);",
  "      });",
  "    });",
  "});",
  sep = "\n"
)

###### E_01_04: Site Navigation ################################################
# Note: The website's nav bar. Absolute links, target _top to leave the
#   shinylive iframe.

E_01_04_nav_fn <- function(active = "Resources") {
  pages <- c(Bio = "index.html", Papers = "papers.html",
             Teaching = "teaching.html", Experience = "experience.html",
             Presentations = "talks.html", Resources = "resources.html",
             Contact = "contact.html")
  tags$nav(
    class = "site-nav",
    tags$div(
      class = "site-nav-row",
      tags$ul(
        lapply(names(pages), function(nm) {
          tags$li(tags$a(href = paste0(B_03_21_site_chr, "/", pages[[nm]]),
                         target = "_top",
                         class = if (identical(nm, active)) "active" else NULL,
                         nm))
        })
      )
    )
  )
}

#### E_02: Sidebar #############################################################
# Note: Lecture selector, narration, controls. The sidebar chooses the model;
#   the main window chooses what to run in it (CONVENTIONS.md 1).

###### E_02_01: Worked-Example Presets #########################################
# Note: Preset card for the main window, from the toolkit (T_05_04 to
#   T_05_06). Only the current lecture's presets show; 1.1 has none.

E_02_01_presets_lst <- T_05_04_presets_fn(
  B_03_04_scenarios_lst, B_03_03_stages_vec, stage_word = ""
)

###### E_02_02: Control Builder ################################################
# Note: Label with tooltip, slider, and a box for an exact value.

E_02_02_control_fn <- function(id, min = NULL) {
  spec  <- B_03_13_controls_lst[[id]]
  value <- B_03_01_defaults_lst[[id]]
  tags$div(
    class = "ctl",
    tags$div(
      class = "ctl-label",
      HTML(spec$label), " ",
      tooltip(tags$span(class = "ctl-help", tabindex = "0", "ⓘ"),
              HTML(B_03_15_help_lst[[id]]), placement = "right")
    ),
    tags$div(
      class = "ctl-row",
      tags$div(class = "ctl-slider",
               sliderInput(id, NULL,
                           min = if (is.null(min)) spec$min else min,
                           max = spec$max, value = value, step = spec$step,
                           width = "100%")),
      tags$div(class = "ctl-box",
               numericInput(paste0(id, "_box"), NULL, value = value,
                            step = spec$step, width = "100%"))
    )
  )
}

###### E_02_03: Sidebar Controls ###############################################
# Note: conditionalPanel reveals controls as the lectures add them.

E_02_03_sidebar_lst <- sidebar(
  width = 380,
  radioButtons("stage", "Stage", choices = B_03_03_stages_vec,
               selected = "1.1"),
  conditionalPanel("input.stage != '1.1'", uiOutput("narrative")),
  accordion(
    open = c("Shocks", "Parameters"),
    accordion_panel(
      "Shocks",
      E_02_02_control_fn("shk_demand"),
      E_02_02_control_fn("shk_supply"),
      E_02_02_control_fn("shk_expect"),
      conditionalPanel("parseFloat(input.stage) >= 1.4",
                       E_02_02_control_fn("shk_markup")),
      conditionalPanel("parseFloat(input.stage) >= 1.2",
                       E_02_02_control_fn("shk_start"),
                       E_02_02_control_fn("shk_length"))
    ),
    accordion_panel(
      "Parameters",
      tags$h6("Demand and Supply"),
      E_02_02_control_fn("alpha"),
      E_02_02_control_fn("gamma"),
      tags$h6("Central Bank"),
      E_02_02_control_fn("beta_pi", min = 1),
      conditionalPanel("parseFloat(input.stage) >= 1.3",
                       E_02_02_control_fn("beta_y"),
                       checkboxInput("zlb", "Impose the Zero Lower Bound",
                                     value = TRUE)),
      E_02_02_control_fn("pi_star"),
      E_02_02_control_fn("r_star"),
      conditionalPanel("parseFloat(input.stage) >= 1.2",
                       tags$h6("Expectations"),
                       E_02_02_control_fn("lambda")),
      conditionalPanel("parseFloat(input.stage) >= 1.5",
                       tags$h6("Banks"),
                       E_02_02_control_fn("phi"))
    )
  ),
  actionButton("reset", "Reset Everything",
               class = "btn-outline-secondary btn-sm w-100"),
  tags$div(
    class = "sidebar-qr",
    tags$a(href = B_03_21_site_chr, target = "_blank",
           tags$img(src = B_04_01_qr_src_chr,
                    alt = paste("QR code for", B_03_21_site_chr))),
    tags$div(class = "sidebar-qr-name", B_03_20_author_chr),
    tags$div(tags$a(href = B_03_21_site_chr, target = "_blank",
                    sub("^https?://", "", B_03_21_site_chr)))
  )
)

#### E_03: Main Panel ##########################################################
# Note: Equations card, presets, prompt, readouts, then the figures.

###### E_03_01: Readout Builder ################################################
# Note: A tile whose number can be typed over; the server backs out the
#   parameter named in hint.

E_03_01_readout_fn <- function(id, label, value, hint) {
  tags$div(
    class = "stat-tile",
    tags$div(class = "stat-label", HTML(label)),
    tags$div(class = "stat-input",
             numericInput(id, NULL, value = round(value, 3), step = 0.05,
                          width = "100%")),
    tags$div(class = "stat-note", uiOutput(paste0(id, "_note"),
                                           inline = TRUE)),
    tags$div(class = "stat-hint", HTML(hint))
  )
}

###### E_03_02: Default Readouts ###############################################
# Note: Tile values at the defaults.

E_03_02_default_diag_lst <- C_01_06_diagnostics_fn(B_03_01_defaults_lst)

###### E_03_03: Page ###########################################################
# Note: The UI passed to shinyApp().

E_03_03_app_ui_lst <- tagList(
  E_01_04_nav_fn(),
  page_sidebar(
  title    = tags$div(
    class = "page-title-wrap",
    tags$div(
      class = "page-title-text",
      tags$h1(class = "bslib-page-title", "The IS-MP-PC Model"),
      tags$div(class = "page-byline",
               tags$a(href = B_03_21_site_chr, target = "_blank",
                      tags$img(src = "sd-logo.png", alt = ""),
                      B_03_20_author_chr))),
    tags$div(
      class = "side-qr",
      tags$a(href = B_03_21_site_chr, target = "_blank",
             tags$img(src = B_04_01_qr_src_chr,
                      alt = paste("QR code for", B_03_21_site_chr))))
  ),
  window_title = paste("The IS-MP-PC Model ·", B_03_20_author_chr),
  fillable = FALSE,
  theme    = E_01_01_theme_lst,
  sidebar  = E_02_03_sidebar_lst,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=IBM+Plex+Sans:wght@400;500;600;700",
                            "&display=swap")),
    tags$style(HTML(E_01_02_css_chr)),
    tags$style(HTML(T_05_07_preset_css_chr)),
    tags$script(src = paste0(
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js",
      "?config=TeX-AMS-MML_HTMLorMML")),
    tags$script(HTML(E_01_03_mathjax_js_chr)),
    tags$script(HTML(T_05_05_preset_js_chr)),
    tags$script(HTML(E_01_04_eqfold_js_chr))
  ),
  navset_card_tab(
    title = textOutput("eq_title", inline = TRUE),
    nav_panel("Equations", uiOutput("eq_model")),
    nav_panel("Notation", uiOutput("eq_notation")),
    nav_panel("In Words", uiOutput("eq_explain"))
  ),
  E_02_01_presets_lst,
  uiOutput("prompt"),
  uiOutput("problems"),
  conditionalPanel(
    "input.stage == '1.1'",
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(6, 6, 12), lg = c(4, 4, 4)),
      card(card_header("Investment\u2013Saving (IS) Curve"),
           tags$div(class = "fig-r32",
                    plotOutput("block_is", height = B_03_07_block_height_chr))),
      card(card_header("Phillips Curve (PC)"),
           tags$div(class = "fig-r32",
                    plotOutput("block_pc", height = B_03_07_block_height_chr))),
      card(card_header("Monetary Policy (MP) Rule"),
           tags$div(class = "fig-r32",
                    plotOutput("block_mp", height = B_03_07_block_height_chr)))
    ),
    tags$div(
      class = "stat-caption",
      HTML(paste(
        "How to read them. Each panel carries its own equation beside the",
        "line it describes. The faint <b>dashed</b> cross marks zero on both",
        "axes \u2014 on the policy rule the horizontal one is the lower bound.",
        "<b>Dotted</b> lines mark the resting points the model is written",
        "around: the natural real rate r*, the inflation target",
        "\u03c0*, and a zero output gap, which is the vertical zero itself.",
        "Each is named on the opposite axis, so the symbol is tied to a value",
        "rather than floating in the panel: read a dynamic off the panel by",
        "saying which side of r*, \u03c0* or y* the economy is on.",
        "On the policy rule the long-dashed line holds the <i>real</i> rate",
        "at r*, so it has slope one; the rule has slope",
        "\u03b2<sub>\u03c0</sub>, and is steeper than it exactly when the",
        "Taylor principle holds."
      ))
    )
  ),
  conditionalPanel(
    "input.stage != '1.1'",
    tags$div(
      class = "stat-caption",
      HTML(paste(
        "Readouts. They follow the parameters, and you can also type over",
        "them: θ sets β<sub>π</sub>, persistence sets λ, and the ZLB",
        "trigger sets r*."
      ))
    ),
    tags$div(
      class = "stat-row mb-2",
      E_03_01_readout_fn(
        "ro_theta",
        "θ: response of π<sub>t</sub> to π<sup>e</sup><sub>t</sub>",
        E_03_02_default_diag_lst$theta, "Type a value to set β<sub>π</sub>"
      ),
      conditionalPanel(
        "parseFloat(input.stage) >= 1.2", class = "stat-slot",
        E_03_01_readout_fn(
          "ro_persist", "Inflation persistence, (1 − λ)θ",
          E_03_02_default_diag_lst$persistence, "Type a value to set λ"
        )
      ),
      conditionalPanel(
        "parseFloat(input.stage) >= 1.3 && input.zlb", class = "stat-slot",
        E_03_01_readout_fn(
          "ro_pizlb", "ZLB trigger, π<sup>ZLB</sup>",
          E_03_02_default_diag_lst$pi_zlb, "Type a value to set r*"
        )
      ),
      conditionalPanel(
        "parseFloat(input.stage) >= 1.3 && input.zlb", class = "stat-slot",
        uiOutput("tile_zlb")
      ),
      conditionalPanel(
        "parseFloat(input.stage) >= 1.4", class = "stat-slot",
        uiOutput("tile_lend")
      )
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(5, 7)),
      card(
        card_header("IS-MP and Phillips Curves"),
        sliderInput("t_show", "Period Shown (Press ▶ to Animate)",
                    min = 0, max = B_03_02_n_periods_int, value = 0,
                    step = 1, width = "100%",
                    animate = animationOptions(
                      interval = B_03_19_play_interval_int)),
        tags$div(class = "fig-r32",
                 plotOutput("diagram", height = B_03_08_diagram_height_chr))
      ),
      card(
        card_header("Time Paths"),
        tags$div(class = "stat-caption",
                 "Green band: shock active. Dotted lines: y*, π*, r* and",
                 "(with the ZLB) zero."),
        plotOutput("paths", height = B_03_09_paths_height_chr)
      )
    )
  ),
  tags$footer(
    class = "credit",
    "Built by ", tags$a(href = B_03_21_site_chr, target = "_blank",
                        B_03_20_author_chr),
    " for ", B_03_22_course_chr, ". Notation follows Whelan's lecture notes.",
    " Version ", B_03_25_version_chr, ".",
    " ", tags$a(href = B_03_26_repo_chr, target = "_blank",
               "Source and download on GitHub"), "."
  )
))

################################################################################
## F: Server ###################################################################
################################################################################
# Note: Assembles the lecture's parameters, simulates, draws.

#### F_01: Server Function #####################################################
# Note: Everything reactive lives here.

###### F_01_01: Server #########################################################
# Note: Local objects are plain snake_case.

F_01_01_app_server_fn <- function(input, output, session) {

  # --- Stage as a number ------------------------------------------------------
  stage_num <- reactive(as.numeric(input$stage))

  # --- Exact value of a control: the box if typed, else the slider ------------
  val <- function(id) {
    b <- input[[paste0(id, "_box")]]
    if (is.null(b) || is.na(b)) input[[id]] else b
  }

  # --- Keep each slider and its box in step -----------------------------------
  # The box holds the exact value. Slider -> box copies; box -> slider snaps
  # the slider without copying back. pushed remembers server-sent values so
  # their echo is not read as typing.
  pushed <- new.env()

  near <- function(slider_value, box_value, spec) {
    abs(slider_value - min(max(box_value, spec$min), spec$max)) <=
      spec$step / 2 + 1e-8
  }

  lapply(names(B_03_13_controls_lst), function(id) {
    box  <- paste0(id, "_box")
    spec <- B_03_13_controls_lst[[id]]

    observeEvent(input[[id]], {
      b <- input[[box]]
      if (!is.null(b) && !is.na(b) && near(input[[id]], b, spec)) return()
      pushed[[box]] <- utils::tail(c(pushed[[box]], input[[id]]), 3)
      updateNumericInput(session, box, value = input[[id]])
    }, ignoreInit = TRUE)

    # Wait for typing to pause
    box_typed <- debounce(reactive(input[[box]]), 500)

    observeEvent(box_typed(), {
      b <- box_typed()
      if (is.null(b) || is.na(b)) return()
      hit <- which(abs(pushed[[box]] - b) < 1e-9)
      if (length(hit) > 0) {
        pushed[[box]] <- pushed[[box]][-seq_len(hit[1])]
        return()
      }
      # Correct values the model would not use as typed
      fixed <- b
      if (spec$step == 1) fixed <- round(fixed)
      if (id == "lambda") fixed <- min(max(fixed, 0), 1)
      if (id == "beta_pi" && stage_num() < 1.4) fixed <- max(fixed, 1)
      if (fixed != b) {
        updateNumericInput(session, box, value = fixed)
        return()
      }
      if (!near(input[[id]], b, spec)) {
        updateSliderInput(session, id, value = b)
      }
    }, ignoreInit = TRUE)
  })

  # --- Set controls to given values (slider and box together) ----------------
  apply_values <- function(vals) {
    for (nm in names(vals)) {
      if (is.logical(vals[[nm]])) {
        updateCheckboxInput(session, nm, value = vals[[nm]])
      } else {
        updateSliderInput(session, nm, value = vals[[nm]])
        updateNumericInput(session, paste0(nm, "_box"), value = vals[[nm]])
      }
    }
  }

  # --- Switching lecture ------------------------------------------------------
  # Every lecture opens on its first preset. A lecture with no preset keeps
  # the sliders and resets only the controls it does not have.
  scenario <- reactiveVal("custom")

  first_preset_fn <- function(stage) {
    hits <- names(B_03_04_scenarios_lst)[vapply(
      B_03_04_scenarios_lst, function(x) identical(x$stage, stage), TRUE)]
    if (length(hits) == 0L) NULL else hits[[1L]]
  }

  observeEvent(input$stage, {
    s <- stage_num()
    updateSliderInput(session, "beta_pi", min = if (s < 1.4) 1 else 0.2)
    first <- first_preset_fn(input$stage)
    if (!is.null(first)) {
      load_preset_fn(first)
      return()
    }
    hidden <- names(Filter(function(x) x$from > s, B_03_13_controls_lst))
    if (s < B_03_14_zlb_from_num) hidden <- c(hidden, "zlb")
    apply_values(B_03_01_defaults_lst[hidden])
    if (s < 1.4 && isTRUE(val("beta_pi") < 1)) {
      apply_values(list(beta_pi = 1))
    }
    set_scenario_fn(if (is.null(first)) NULL else first)
  })

  # --- Worked-example presets -------------------------------------------------
  # One observer per preset; set_scenario_fn is the only place that sets the
  # loaded preset, so the button marker and card header cannot drift apart.
  scn_now <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_04_scenarios_lst)) NULL
    else B_03_04_scenarios_lst[[k]]
  })

  set_scenario_fn <- function(key) {
    scenario(if (is.null(key)) "custom" else key)
    session$sendCustomMessage("dgPreset", if (is.null(key)) "" else key)
    invisible(NULL)
  }

  load_preset_fn <- function(key) {
    if (is.null(key) || !key %in% names(B_03_04_scenarios_lst)) {
      return(invisible(NULL))
    }
    scn <- B_03_04_scenarios_lst[[key]]
    set_scenario_fn(key)
    updateSliderInput(session, "beta_pi",
                      min = if (as.numeric(scn$stage) < 1.4) 1 else 0.2)
    apply_values(utils::modifyList(B_03_01_defaults_lst, scn$values))
    updateSliderInput(session, "t_show", value = 0)
    invisible(NULL)
  }

  lapply(names(B_03_04_scenarios_lst), function(key) {
    observeEvent(input[[paste0("preset_", key)]],
                 load_preset_fn(key), ignoreInit = TRUE)
  })

  output$preset_title <- renderUI({
    T_05_06_preset_title_fn(scn_now(),
                            input$stage, B_03_03_stages_vec,
                            stage_word = "")
  })

  # --- Reset ------------------------------------------------------------------
  observeEvent(input$reset, {
    apply_values(B_03_01_defaults_lst)
    set_scenario_fn(NULL)
    updateSliderInput(session, "t_show", value = 0)
  })

  # --- Parameters and shocks in force for this lecture -----------------------
  # Controls hidden at earlier lectures are switched off whatever their value,
  # so each stage is exactly that lecture's model: lambda = 1 (pi^e = pi*)
  # before 1.2, beta_y = 0 before 1.3, no ZLB before 1.3, beta_pi >= 1 before
  # 1.4, no mark-up before 1.4, phi = 0 before 1.5. A pure function of the
  # control values and the stage, so the ghost can be assembled the same way.
  assemble_fn <- function(v, s) {
    par <- list(
      alpha   = v$alpha,
      gamma   = v$gamma,
      beta_pi = if (s < 1.4) max(v$beta_pi, 1) else v$beta_pi,
      beta_y  = if (s >= 1.3) v$beta_y else 0,
      pi_star = v$pi_star,
      r_star  = v$r_star,
      lambda  = if (s >= 1.2) min(max(v$lambda, 0), 1) else 1,
      phi     = if (s >= 1.5) v$phi else 0,
      zlb     = s >= B_03_14_zlb_from_num && isTRUE(v$zlb)
    )
    n      <- B_03_02_n_periods_int
    start  <- min(max(round(v$shk_start), 1), n)
    len    <- max(round(v$shk_length), 1)
    window <- c(start, min(start + len - 1, n))
    on     <- seq_len(n) >= window[1] & seq_len(n) <= window[2]
    markup <- if (s >= 1.4) v$shk_markup else 0
    shocks <- data.frame(
      shk_demand_val = ifelse(on, v$shk_demand, 0),
      shk_supply_val = ifelse(on, v$shk_supply, 0),
      shk_expect_val = ifelse(on, v$shk_expect, 0),
      shk_markup_val = ifelse(on, markup, 0)
    )
    list(par = par, window = window, shocks = shocks)
  }

  inputs_raw <- reactive({
    req(!is.null(input$alpha))
    vals <- stats::setNames(lapply(names(B_03_01_defaults_lst), val),
                            names(B_03_01_defaults_lst))
    vals$zlb <- isTRUE(input$zlb)
    assemble_fn(vals, stage_num())
  })

  inputs_now   <- debounce(inputs_raw, B_03_18_debounce_ms_int)
  par_now      <- reactive(inputs_now()$par)
  shock_window <- reactive(inputs_now()$window)
  shocks_df    <- reactive(inputs_now()$shocks)

  diag_now <- reactive(C_01_06_diagnostics_fn(par_now()))

  sim_df <- reactive({
    req(length(diag_now()$problems) == 0)
    C_01_03_simulate_fn(par_now(), shocks_df())
  })

  # --- The ghost: the figure at the worked example's own settings -------------
  # Reference values are the loaded preset's, or the defaults when none is
  # loaded (lecture 1.1). Ghost layers are skipped while live and reference
  # agree.
  ref_vals <- reactive({
    key <- scenario()
    if (is.null(key) || !key %in% names(B_03_04_scenarios_lst)) {
      return(B_03_01_defaults_lst)
    }
    utils::modifyList(B_03_01_defaults_lst,
                      B_03_04_scenarios_lst[[key]]$values)
  })

  ref_inputs <- reactive(assemble_fn(ref_vals(), stage_num()))

  ghost <- reactive({
    ref <- ref_inputs()
    if (is.null(ref)) return(NULL)
    if (T_02_03b_ghost_off_fn(inputs_now()$par, ref$par) &&
        isTRUE(all.equal(inputs_now()$shocks, ref$shocks))) return(NULL)
    if (length(C_01_06_diagnostics_fn(ref$par)$problems) > 0) return(NULL)
    C_01_03_simulate_fn(ref$par, ref$shocks)
  })

  ghost_par <- reactive({
    ref <- ref_inputs()
    if (is.null(ref) || T_02_03b_ghost_off_fn(inputs_now()$par, ref$par)) {
      return(NULL)
    }
    ref$par
  })

  # --- Scenario story and narration -------------------------------------------
  output$scenario_story <- renderUI({
    T_05_02_story_fn(scn_now(),
                     B_03_13_controls_lst, B_03_15_help_lst)
  })


  output$narrative <- renderUI({
    req(length(diag_now()$problems) == 0)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head",
               paste0("What Is Happening · Period ", input$t_show)),
      D_04_01_narrate_fn(sim_df(), par_now(), input$t_show, stage_num(),
                         shock_window(), diag_now())
    )
  })

  # --- Equations, notation and explanation tabs -------------------------------
  output$eq_title <- renderText({
    names(B_03_03_stages_vec)[match(input$stage, B_03_03_stages_vec)]
  })

  mj <- function(tex) HTML(paste0("\\(", tex, "\\)"))

  # Items in force at this lecture: current version, status, previous form
  eq_items <- reactive({
    s <- stage_num()
    items <- Filter(function(it) min(as.numeric(names(it$versions))) <= s,
                    B_03_16_equations_lst)
    lapply(items, function(it) {
      keys   <- as.numeric(names(it$versions))
      cur    <- max(keys[keys <= s])
      cur_nm <- names(it$versions)[keys == cur]
      status <- if (cur != s) "" else if (cur == min(keys)) "new" else
        "changed"
      was <- if (status == "changed") {
        it$versions[[names(it$versions)[keys == max(keys[keys < cur])]]]
      }
      list(group = it$group, label = it$label, tex = it$versions[[cur_nm]],
           status = status, was = was, note = it$notes[[cur_nm]])
    })
  })

  flag <- function(status) {
    if (status != "") tags$span(class = paste0("eq-flag eq-", status), status)
  }

  empty_note <- tags$div(
    class = "chg-note text-muted",
    "These appear from stage 2, once the model is solved."
  )

  # Tab 1: equations in four groups
  output$eq_model <- renderUI({
    items <- eq_items()
    group_col <- function(grp) {
      rows <- lapply(Filter(function(x) x$group == grp, items), function(x) {
        tags$tr(tags$td(class = "eq-label", HTML(x$label), flag(x$status)),
                tags$td(mj(x$tex)))
      })
      tags$div(
        class = "eq-group",
        tags$div(class = "eq-group-title", B_03_17_groups_vec[[grp]]),
        if (length(rows) == 0) empty_note else
          tags$table(class = "eq-table", do.call(tagList, rows))
      )
    }
    withMathJax(tagList(
      layout_columns(
        col_widths = breakpoints(sm = 12, md = c(6, 6, 6, 6),
                                 xl = c(7, 5, 7, 5)),
        group_col("model"), group_col("assumption"),
        group_col("solved"), group_col("descriptor")
      ),
      tags$div(class = "eq-legend",
               tags$span(class = "eq-flag eq-new", "new"), " and ",
               tags$span(class = "eq-flag eq-changed", "changed"),
               " mark what this stage adds to the one before. The",
               " Explanations tab says what each one does.")
    ))
  })

  # Tab 2: notation
  output$eq_notation <- renderUI({
    s <- stage_num()
    items <- Filter(function(x) x$from <= s, B_03_23_notation_lst)
    col <- function(grps, title) {
      its <- Filter(function(x) x$grp %in% grps, items)
      tags$div(
        tags$div(class = "eq-group-title", title),
        tags$table(class = "nota-table", lapply(its, function(x) {
          tags$tr(tags$td(mj(x$sym)),
                  tags$td(paste0(toupper(substr(x$txt, 1, 1)),
                                 substring(x$txt, 2)),
                          if (x$from == s && s > 1.1) {
                            tags$span(class = "eq-flag eq-new", "new")
                          }))
        }))
      )
    }
    withMathJax(layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 4, 4)),
      col("var", "Variables"),
      col("par", "Parameters"),
      col(c("tgt", "shk"), "Targets, Thresholds and Shocks")
    ))
  })

  # Tab 3: each equation with its explanation and what changed
  output$eq_explain <- renderUI({
    items <- eq_items()
    blocks <- lapply(names(B_03_17_groups_vec), function(grp) {
      its <- Filter(function(x) x$group == grp, items)
      if (length(its) == 0) return(NULL)
      tagList(
        tags$tr(tags$td(colspan = "3", class = "eq-group-title",
                        B_03_17_groups_vec[[grp]])),
        lapply(its, function(x) {
          tags$tr(
            tags$td(class = "eq-label", HTML(x$label), flag(x$status)),
            tags$td(class = "eq-math",
                    tags$div(mj(x$tex)),
                    if (!is.null(x$was)) {
                      tags$div(class = "chg-was", "was ", mj(x$was))
                    }),
            tags$td(class = "chg-note", HTML(x$note))
          )
        })
      )
    })
    withMathJax(tags$table(class = "eq-table eq-explain", blocks))
  })

  # --- Prompt and problems ----------------------------------------------------
  output$prompt <- renderUI({
    scn <- scn_now()
    on_stage <- !is.null(scn) && scn$stage == input$stage
    txt <- if (on_stage) scn$prompt else B_03_12_prompts_lst[[input$stage]]
    tags$div(class = "prompt",
             tags$strong(if (on_stage) "Example. " else "Note. "), txt)
  })

  output$problems <- renderUI({
    probs <- diag_now()$problems
    if (length(probs) == 0) return(NULL)
    tags$div(class = "problem", lapply(probs, tags$p))
  })

  # --- Readouts: follow the parameters, and can be typed over -----------------
  # Typing a readout inverts the algebra for one parameter: theta sets
  # beta_pi, persistence sets lambda, pi^ZLB sets r*. ro_pushed works as
  # pushed does above.
  ro_pushed <- new.env()

  push_ro <- function(id, value) {
    if (!is.finite(value)) return()
    value <- round(value, 3)
    cur   <- isolate(input[[id]])
    if (!is.null(cur) && !is.na(cur) && abs(cur - value) < 5e-4) return()
    ro_pushed[[id]] <- utils::tail(c(ro_pushed[[id]], value), 3)
    updateNumericInput(session, id, value = value)
  }

  observe({
    req(length(diag_now()$problems) == 0)
    d <- diag_now()
    push_ro("ro_theta", d$theta)
    push_ro("ro_persist", d$persistence)
    push_ro("ro_pizlb", d$pi_zlb)
  })

  ro_edit <- function(id, handler) {
    typed <- debounce(reactive(input[[id]]), 600)
    observeEvent(typed(), {
      v <- typed()
      if (is.null(v) || is.na(v)) return()
      hit <- which(abs(ro_pushed[[id]] - v) < 1e-9)
      if (length(hit) > 0) {
        ro_pushed[[id]] <- ro_pushed[[id]][-seq_len(hit[1])]
        return()
      }
      handler(v)
    }, ignoreInit = TRUE)
  }

  # theta = D / (D + alpha gamma (beta_pi - 1)), D = 1 + alpha (beta_y - phi);
  # Whelan eq. 2.6 when beta_y = phi = 0
  ro_edit("ro_theta", function(v) {
    p      <- par_now()
    d_coef <- 1 + p$alpha * (p$beta_y - p$phi)
    if (v <= 0) {
      showNotification("θ must be above zero.", type = "warning")
      push_ro("ro_theta", diag_now()$theta)
      return()
    }
    bp <- 1 + d_coef * (1 / v - 1) / (p$alpha * p$gamma)
    if (!is.finite(bp)) return()
    if (stage_num() < 1.4 && bp < 1) {
      bp <- 1
      showNotification(paste("Until stage 4 β_π stays at or above one,",
                             "so θ cannot go above one."), type = "message")
    }
    if (abs(bp - p$beta_pi) < 1e-9) push_ro("ro_theta", diag_now()$theta)
    apply_values(list(beta_pi = round(bp, 4)))
  })

  # persistence = (1 - lambda) theta
  ro_edit("ro_persist", function(v) {
    th  <- diag_now()$theta
    lam <- 1 - v / th
    if (!is.finite(lam)) return()
    if (lam < 0 || lam > 1) {
      showNotification(paste0("With θ = ", round(th, 2), ", persistence can ",
                              "only lie between 0 and ", round(th, 2),
                              ". Change θ (via β_π) to go beyond that."),
                       type = "message")
      lam <- min(max(lam, 0), 1)
    }
    if (abs(lam - par_now()$lambda) < 1e-9) {
      push_ro("ro_persist", diag_now()$persistence)
    }
    apply_values(list(lambda = round(lam, 4)))
  })

  # pi_zlb = ((beta_pi - 1) / beta_pi) pi* - r* / beta_pi
  ro_edit("ro_pizlb", function(v) {
    p  <- par_now()
    rs <- (p$beta_pi - 1) * p$pi_star - p$beta_pi * v
    if (!is.finite(rs)) return()
    apply_values(list(r_star = round(rs, 4)))
  })

  # --- Readout notes and the two output-only tiles ----------------------------
  fmt <- function(x) formatC(x, format = "f", digits = 2)

  output$ro_theta_note <- renderUI({
    th <- diag_now()$theta
    if (th < 1) {
      "Below one: policy leans against inflation"
    } else if (th == 1) {
      "Equal to one: policy does not lean against inflation"
    } else {
      "Above one: Taylor principle violated"
    }
  })

  output$ro_persist_note <- renderUI({
    if (diag_now()$persistence < 1) "Below one: shocks die out" else
      "One or more: inflation never settles"
  })

  output$ro_pizlb_note <- renderUI({
    if (par_now()$beta_y == 0) "The rule asks for i < 0 below this" else
      HTML(paste("With β<sub>y</sub> > 0 this is the trigger at a zero",
                 "output gap"))
  })

  stat_tile <- function(label, value, note, status = "") {
    tags$div(class = paste("stat-tile", status),
             tags$div(class = "stat-label", label),
             tags$div(class = "stat-value", value),
             tags$div(class = "stat-note", note))
  }

  output$tile_zlb <- renderUI({
    req(length(diag_now()$problems) == 0)
    sim    <- sim_df()
    n_zlb  <- sum(sim$mod_zlb_is)
    spiral <- utils::tail(sim$mod_zlb_is, 1) &&
      diag_now()$persistence_zlb >= 1
    stat_tile(
      "Periods at the ZLB", n_zlb,
      if (spiral) {
        paste0("Deflationary spiral: at the ZLB each fall in expected ",
               "inflation is amplified ×", fmt(diag_now()$persistence_zlb))
      } else if (n_zlb > 0) {
        "The economy escapes the lower bound"
      } else {
        "Policy rate stays positive"
      },
      if (spiral) "bad" else if (n_zlb > 0) "good" else ""
    )
  })

  output$tile_lend <- renderUI({
    req(length(diag_now()$problems) == 0)
    sim <- sim_df()
    stat_tile("Lowest lending rate, ℓ", fmt(min(sim$mod_lending_rate_val)),
              paste0("Lowest policy rate: ",
                     fmt(min(sim$mod_policy_rate_val))))
  })

  # --- Lecture 1.1 blocks -----------------------------------------------------
  output$block_is <- renderPlot({
    D_02_01_is_block_fn(par_now(), val("shk_demand"),
                        ref = ghost_par(),
                        ref_eps_y = ref_vals()$shk_demand)
  })
  output$block_pc <- renderPlot({
    rp <- ghost_par()
    D_02_02_pc_block_fn(par_now(), par_now()$pi_star + val("shk_expect"),
                        val("shk_supply"),
                        ref = rp,
                        ref_pi_e = ref_inputs()$par$pi_star +
                          ref_vals()$shk_expect,
                        ref_eps_pi = ref_vals()$shk_supply)
  })
  output$block_mp <- renderPlot({
    D_02_03_mp_block_fn(par_now(), ref = ghost_par())
  })

  # --- Diagram and paths ------------------------------------------------------
  output$diagram <- renderPlot({
    D_03_01_diagram_fn(sim_df(), par_now(), input$t_show, ghost())
  })

  output$paths <- renderPlot({
    D_03_02_paths_fn(sim_df(), par_now(), stage_num(), NULL,
                     shock_window(), ghost())
  })
}

################################################################################
## G: Run ######################################################################
################################################################################
# Note: Launch.

#### G_01: Launch ##############################################################
# Note: Returns the app object.

###### G_01_01: Build App ######################################################
# Note: UI from E, server from F.

shinyApp(ui = E_03_03_app_ui_lst, server = F_01_01_app_server_fn)

#--------------------------------- Script End ---------------------------------#
