# Interactive Model: IS-MP-PC

A Shiny app for teaching the IS-MP-PC model of short-run
fluctuations. Built by [Sam Deegan](https://sam-deegan.com) for ECON42550
Macroeconomics, University College Dublin.

**Try it in the browser (nothing to install):**
https://sam-deegan.com/toy-models/is-mp-pc/

Current version: **1.0.0** (see [CHANGELOG.md](CHANGELOG.md)). The version
is shown in the app footer; releases are tagged `vX.Y.Z`.

## What it does

The lecture selector adds one layer of the model at a time:

| Lecture | What is added |
|---|---|
| 1.1 | The IS curve, the Phillips curve and the policy rule as three separate diagrams |
| 1.2 | The solved model: the IS-MP / PC diagram, adaptive expectations, time paths |
| 1.3 | The zero lower bound, and an output-gap term in the policy rule |
| 1.4 | The Taylor principle relaxed (β<sub>π</sub> below one); a bank lending rate with a mark-up |
| 1.5 | A financial accelerator: mark-ups that widen in downturns |

Each lecture opens on a worked example (a demand shock, an energy shock, a
deep recession at the lower bound, the Great Inflation, a banking crisis).
Every slider has a box beside it for an exact value, and the sidebar
narrates each period when you press play. The Equations, Notation and In
Words tabs show the model as it stands at the chosen lecture and flag what
that lecture changed.

## Run it locally

1. Install [R](https://cran.r-project.org/) (4.1 or later) and, ideally,
   [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the three packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or from R in this folder:

   ```r
   shiny::runApp()
   ```

Equations are typeset with MathJax from a CDN, so they need an internet
connection; everything else runs offline.

## Files

```
app.R          the app: settings and text (section B), figures (D),
               interface (E), server (F)
R/model.R      the model: closed-form solution, simulator, curves,
               diagnostics. Sources on its own, so slides can reuse it.
R/toolkit.R    layout and helpers shared with the other toy-model apps
www/           logo and QR code
README.md      this file
CHANGELOG.md   version history
CONVENTIONS.md how the figures and worked examples are laid out
LICENSE        CC BY-NC-ND 4.0
```

All text on screen (worked examples, prompts, equations, notation) is in
section `B_03` of `app.R`, so it can be edited without touching the rest.

## The model

The IS-MP-PC model is the workhorse three-equation model of short-run
fluctuations: an IS curve for demand, a Phillips curve for inflation, and a
monetary-policy rule in place of the old LM curve. It is the model taught in
Whelan's *Lecture Notes on Macroeconomics* (2023, chapters 1, 2 and 4) and in
Romer's *Advanced Macroeconomics* (chapters 6 and 12). Output `y` is in logs,
so `y − y*` is the percentage output gap; rates and inflation are in per cent.

```
IS:      y_t = y*_t − α (ℓ_t − π_t − r*) + ε^y_t
PC:      π_t = π^e_t + γ (y_t − y*_t) + ε^π_t
MP:      i_t = max[ r* + π* + β_π (π_t − π*) + β_y (y_t − y*_t), 0 ]
Banks:   ℓ_t = i_t + μ^B_t,   μ^B_t = μ̄^B_t − φ (y_t − y*_t)
Expect:  π^e_t = λ π* + (1 − λ) π_{t−1}
```

**The IS curve** says spending falls when the real interest rate borrowers
face (`ℓ − π`) is above the natural rate `r*`, the rate that keeps output at
potential. `α` is how much output falls per point of real rate; `ε^y` is a
demand shock (confidence, fiscal policy, a lockdown).

**The Phillips curve** says inflation moves one-for-one with what people
expect and rises with the output gap. `γ` is its slope; `ε^π` is a cost
shock (energy prices, supply chains). `y*` is potential output, the level at
which inflation is stable. It is a resting point, not an optimum.

**The policy rule** says the central bank raises its rate more than
one-for-one when inflation is above target (`β_π > 1`, the Taylor
principle), so that the *real* rate rises and demand cools. Substituting the
rule into the IS curve gives the downward-sloping **IS-MP curve** in
(output gap, inflation) space; where it crosses the Phillips curve is the
period's equilibrium. Solving the two together gives

```
π_t = θ π^e_t + (1 − θ) π* + θ (γ ε^y_t + ε^π_t),   θ = 1 / [1 + αγ(β_π − 1)]
```

so `θ` measures how much of a change in expectations passes into inflation.
With adaptive expectations (`π^e_t = π_{t−1}`), `θ` is also how much of a
shock survives into next period: below one, shocks die out.

**What the five lectures show with it**

- *1.1* The three curves on their own, and what each parameter does to each.
- *1.2* The solved model. Demand shocks move output and inflation together;
  supply shocks move them apart, so the central bank faces a trade-off.
  Shocks persist because expectations carry last period's inflation forward.
- *1.3* The zero lower bound. Once `i` hits zero, lower inflation *raises*
  the real rate, the IS-MP curve slopes upwards, and a deep recession can
  turn into a deflationary spiral. Anchored expectations (`λ > 0`) stop it.
- *1.4* The Taylor principle relaxed. With `β_π < 1` the real rate falls as
  inflation rises, the IS-MP curve slopes the wrong way, and inflation drifts
  away from target: the Great Inflation. A bank lending rate `ℓ = i + μ^B`
  puts a wedge between the policy rate and the rate borrowers face, so a
  banking crisis is a demand shock that policy cannot offset one-for-one.
- *1.5* A financial accelerator. Mark-ups widen as output falls (`φ > 0`),
  so each round of falling output tightens credit and lowers output again.

**Where it departs from the textbook.** With `μ̄ = φ = 0` the lending rate
is the policy rate, with `β_y = 0` the rule is Whelan's simple rule, and
with `λ = 0` expectations are his adaptive case, so every textbook result is
a special case of the app. The bank block is a reduced form of the credit
channel (Bernanke and Gertler 1995), not a full model of bank net worth,
and `λ` is a pedagogical device for showing why anchoring matters. The model
has no capital, no exchange rate and no fiscal block; it is a model of one
period's equilibrium repeated, not of optimising agents.

## References

- Whelan, K. (2023). *Lecture Notes on Macroeconomics*. Chapters 1, 2, 4.
- Romer, D. (2019). *Advanced Macroeconomics*, 5th ed. Chapters 6 and 12.
- Clarida, R., Galí, J. and Gertler, M. (2000). Monetary policy rules and
  macroeconomic stability. *Quarterly Journal of Economics* 115(1).
- Bernanke, B. and Gertler, M. (1995). Inside the black box: the credit
  channel of monetary policy transmission. *Journal of Economic
  Perspectives* 9(4).

## Licence

© Sam Deegan. Released under
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/):
free to use and share for teaching with attribution; not for commercial use
or redistribution in modified form.
