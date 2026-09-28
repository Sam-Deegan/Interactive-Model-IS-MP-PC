################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## IS-MP-PC Model: Solver and Simulator                                       ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced automatically by app.R. Can be sourced alone from a lecture
##   .qmd so slide figures come from the same model:
##     source("R/model.R")
##
## Inputs:
##   None. Every function is a pure function of a parameter list and a
##   shock data frame.
##
## Outputs:
##   C_01_* functions: one-period solution, simulation, curves, diagnostics.
##
## The model (Whelan 2023, ch. 1 notation):
##   IS:     y_t = y*_t - alpha (l_t - pi_t - r*) + eps^y_t        (eq. 1.5)
##   PC:     pi_t = pi^e_t + gamma (y_t - y*_t) + eps^pi_t          (eq. 1.3)
##   MP:     i_t = max[r* + pi* + beta_pi (pi_t - pi*)              (eq. 1.8)
##                     + beta_y (y_t - y*_t), 0]
##   Banks:  l_t = i_t + mu^B_t
##           mu^B_t = mubar^B_t - phi (y_t - y*_t)
##   Expect: pi^e_t = lambda pi* + (1 - lambda) pi_{t-1} + (one-off shock)
##
##   Switching features off recovers each lecture's model: mubar = phi = 0
##   gives l_t = i_t; beta_y = 0 gives Whelan's simple rule; lambda = 0 gives
##   his adaptive expectations pi^e_t = pi_{t-1} (ch. 2); zlb = FALSE removes
##   the max[., 0].
##
## Extensions beyond Whelan, and why:
##   Banks. The lending rate l_t = i_t + mu^B_t puts a wedge between the
##   policy rate and the rate borrowers face, so a mark-up shock is a
##   demand shock policy cannot offset one for one. The accelerator phi is a
##   reduced form of the credit channel (Bernanke and Gertler 1995); it is
##   not the BGG net-worth mechanism, which needs capital and an asset
##   price.
##   Anchoring. lambda in [0, 1] spans Whelan's adaptive case (0) and a
##   fully anchored target (1), so one slider shows why anchoring stops the
##   deflationary spiral at the ZLB.
##
## Parameter list (par) elements:
##   alpha, gamma, beta_pi, beta_y, pi_star, r_star, lambda, phi, zlb

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C_01 holds the model; app.R holds sections B, D, E, F and G.
#
#   C: Model
#     C_01_01  Policy rule (desired rate, before the lower bound)
#     C_01_02  Solve one period
#     C_01_03  Simulate a path
#     C_01_04  IS-MP curve for the diagram
#     C_01_05  Phillips curve for the diagram
#     C_01_06  Diagnostics: theta, pi^ZLB, stability

################################################################################
## C: Model ####################################################################
################################################################################
# Note: Closed-form solutions period by period; no numerical solver.

#### C_01: Model Functions #####################################################
# Note: All gaps are percentage points; rates and inflation are in per cent.

###### C_01_01: Policy Rule ####################################################
# Note: The rate the rule asks for, before the lower bound. Whelan eq. 1.8
#   with the output-gap term of pp. 35-36.

C_01_01_rule_rate_fn <- function(pi_t, y_gap, par) {
  par$r_star + par$pi_star +
    par$beta_pi * (pi_t - par$pi_star) +
    par$beta_y * y_gap
}

###### C_01_02: Solve One Period ###############################################
# Note: Solves IS, PC, MP and the mark-up jointly for period t.
#   Normal regime. MP and the mark-up into IS give the IS-MP curve
#     y_gap = [-alpha (beta_pi - 1)(pi - pi*) - alpha mubar + eps_y] / D,
#     D = 1 + alpha (beta_y - phi)                   (Whelan eq. 1.18 at D = 1)
#   and with PC
#     pi - pi* = [D (pi_e - pi* + eps_pi) + gamma (eps_y - alpha mubar)]
#                / [D + alpha gamma (beta_pi - 1)],
#   which is Whelan's pi_t = theta pi^e_t + (1 - theta) pi* + theta(...)
#   (eqs 2.5-2.10) when beta_y = phi = mubar = 0.
#   ZLB regime (rule rate < 0). i_t = 0, so
#     y_gap = [alpha (pi + r* - mubar) + eps_y] / E,  E = 1 - alpha phi,
#     pi    = [E (pi_e + eps_pi) + gamma (alpha (r* - mubar) + eps_y)]
#             / (E - alpha gamma),
#   Whelan's liquidity-trap solution (ch. 4) when phi = mubar = 0.

C_01_02_solve_period_fn <- function(pi_e, eps_y, eps_pi, mubar, par) {

  # Normal regime: the policy rule holds
  d_coef   <- 1 + par$alpha * (par$beta_y - par$phi)
  denom_n  <- d_coef + par$alpha * par$gamma * (par$beta_pi - 1)
  pi_gap   <- (d_coef * (pi_e - par$pi_star + eps_pi) +
                 par$gamma * (eps_y - par$alpha * mubar)) / denom_n
  pi_t     <- par$pi_star + pi_gap
  y_gap    <- (-par$alpha * (par$beta_pi - 1) * pi_gap -
                 par$alpha * mubar + eps_y) / d_coef
  i_rule   <- C_01_01_rule_rate_fn(pi_t, y_gap, par)
  i_t      <- i_rule
  at_zlb   <- FALSE

  # ZLB regime: the rule asks for a negative rate, so i_t = 0
  if (isTRUE(par$zlb) && i_rule < 0) {
    e_coef  <- 1 - par$alpha * par$phi
    denom_z <- e_coef - par$alpha * par$gamma
    pi_t    <- (e_coef * (pi_e + eps_pi) +
                  par$gamma * (par$alpha * (par$r_star - mubar) + eps_y)) /
               denom_z
    y_gap   <- (par$alpha * (pi_t + par$r_star - mubar) + eps_y) / e_coef
    i_rule  <- C_01_01_rule_rate_fn(pi_t, y_gap, par)
    i_t     <- 0
    at_zlb  <- TRUE
  }

  mu_t <- mubar - par$phi * y_gap
  l_t  <- i_t + mu_t

  list(
    pi_t   = pi_t,
    pi_e   = pi_e,
    y_gap  = y_gap,
    i_t    = i_t,
    i_rule = i_rule,
    mu_t   = mu_t,
    l_t    = l_t,
    at_zlb = at_zlb
  )
}

###### C_01_03: Simulate a Path ################################################
# Note: Period 0 is the steady state; shocks enter from period 1. shocks_df
#   has one row per period: shk_demand_val (eps^y), shk_supply_val (eps^pi),
#   shk_expect_val (one-off jump in pi^e), shk_markup_val (mubar^B).

C_01_03_simulate_fn <- function(par, shocks_df) {

  n_periods <- nrow(shocks_df)
  out_lst   <- vector("list", n_periods + 1)

  out_lst[[1]] <- data.frame(
    mod_period_tm        = 0L,
    mod_output_gap_val   = 0,
    mod_inflation_val    = par$pi_star,
    mod_expected_inf_val = par$pi_star,
    mod_policy_rate_val  = par$r_star + par$pi_star,
    mod_rule_rate_val    = par$r_star + par$pi_star,
    mod_markup_val       = 0,
    mod_lending_rate_val = par$r_star + par$pi_star,
    mod_zlb_is           = FALSE,
    shk_demand_val       = 0,
    shk_supply_val       = 0,
    shk_expect_val       = 0,
    shk_markup_val       = 0
  )

  pi_lag <- par$pi_star

  for (t in seq_len(n_periods)) {
    shk  <- shocks_df[t, ]
    pi_e <- par$lambda * par$pi_star + (1 - par$lambda) * pi_lag +
      shk$shk_expect_val
    sol  <- C_01_02_solve_period_fn(
      pi_e   = pi_e,
      eps_y  = shk$shk_demand_val,
      eps_pi = shk$shk_supply_val,
      mubar  = shk$shk_markup_val,
      par    = par
    )
    out_lst[[t + 1]] <- data.frame(
      mod_period_tm        = t,
      mod_output_gap_val   = sol$y_gap,
      mod_inflation_val    = sol$pi_t,
      mod_expected_inf_val = sol$pi_e,
      mod_policy_rate_val  = sol$i_t,
      mod_rule_rate_val    = sol$i_rule,
      mod_markup_val       = sol$mu_t,
      mod_lending_rate_val = sol$l_t,
      mod_zlb_is           = sol$at_zlb,
      shk_demand_val       = shk$shk_demand_val,
      shk_supply_val       = shk$shk_supply_val,
      shk_expect_val       = shk$shk_expect_val,
      shk_markup_val       = shk$shk_markup_val
    )
    pi_lag <- sol$pi_t
  }

  sim_df <- do.call(rbind, out_lst)
  sim_df$mod_real_policy_val  <- sim_df$mod_policy_rate_val -
    sim_df$mod_inflation_val
  sim_df$mod_real_lending_val <- sim_df$mod_lending_rate_val -
    sim_df$mod_inflation_val
  sim_df
}

###### C_01_04: IS-MP Curve ####################################################
# Note: Output gap implied by IS + MP (+ mark-up) at each inflation rate,
#   for one period's shocks. With zlb = TRUE it kinks at pi^ZLB: downward
#   sloping above, upward sloping below (Whelan ch. 4).

C_01_04_is_mp_curve_fn <- function(pi_grid, eps_y, mubar, par) {
  d_coef <- 1 + par$alpha * (par$beta_y - par$phi)
  e_coef <- 1 - par$alpha * par$phi
  y_rule <- (-par$alpha * (par$beta_pi - 1) * (pi_grid - par$pi_star) -
               par$alpha * mubar + eps_y) / d_coef
  y_gap  <- y_rule
  if (isTRUE(par$zlb)) {
    i_rule <- C_01_01_rule_rate_fn(pi_grid, y_rule, par)
    y_zlb  <- (par$alpha * (pi_grid + par$r_star - mubar) + eps_y) / e_coef
    y_gap  <- ifelse(i_rule < 0, y_zlb, y_rule)
  }
  data.frame(crv_inflation_val = pi_grid, crv_output_gap_val = y_gap)
}

###### C_01_05: Phillips Curve #################################################
# Note: Inflation implied by the PC at each output gap.

C_01_05_pc_curve_fn <- function(y_grid, pi_e, eps_pi, par) {
  data.frame(
    crv_output_gap_val = y_grid,
    crv_inflation_val  = pi_e + par$gamma * y_grid + eps_pi
  )
}

###### C_01_06: Diagnostics ####################################################
# Note: theta is the coefficient on pi^e_t in the solution for pi_t (Whelan
#   eq. 2.6). Inflation persistence is (1 - lambda) theta: below one shocks
#   die out, above one they explode. theta_zlb is the same coefficient at
#   the lower bound, Whelan's 1 / (1 - alpha gamma). pi_zlb is the inflation
#   rate at which the rule reaches zero when beta_y = 0.

C_01_06_diagnostics_fn <- function(par) {
  d_coef    <- 1 + par$alpha * (par$beta_y - par$phi)
  e_coef    <- 1 - par$alpha * par$phi
  denom_n   <- d_coef + par$alpha * par$gamma * (par$beta_pi - 1)
  denom_z   <- e_coef - par$alpha * par$gamma
  theta     <- d_coef / denom_n
  theta_zlb <- e_coef / denom_z
  pi_zlb    <- ((par$beta_pi - 1) / par$beta_pi) * par$pi_star -
    par$r_star / par$beta_pi

  # Parameter combinations with no equilibrium
  problems_vec <- character(0)
  if (d_coef <= 0 || denom_n <= 0) {
    problems_vec <- c(problems_vec, paste0(
      "No sensible equilibrium: 1 + alpha gamma (beta_pi - 1) must be ",
      "positive. Raise beta_pi or lower alpha or gamma."
    ))
  }
  if (isTRUE(par$zlb) && (e_coef <= 0 || denom_z <= 0)) {
    problems_vec <- c(problems_vec, paste0(
      "At the lower bound the Phillips curve is steeper than the IS-MP ",
      "curve (alpha gamma >= 1), so there is no equilibrium there. ",
      "Lower alpha or gamma."
    ))
  }

  if (isTRUE(par$zlb) && par$r_star + par$pi_star < 0) {
    problems_vec <- c(problems_vec, paste0(
      "r* + pi* is below zero, so even the steady state needs a negative ",
      "policy rate. Raise pi* or r*, or switch off the lower bound."
    ))
  }

  list(
    theta           = theta,
    theta_zlb       = theta_zlb,
    persistence     = (1 - par$lambda) * theta,
    persistence_zlb = (1 - par$lambda) * theta_zlb,
    pi_zlb          = pi_zlb,
    problems        = problems_vec
  )
}

#--------------------------------- Script End ---------------------------------#
