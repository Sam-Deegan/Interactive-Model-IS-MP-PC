# Changelog

All notable changes to this app. Versions follow [Semantic Versioning](https://semver.org/):
MAJOR for a change to the model or its notation, MINOR for new features
(a stage, a worked example, a figure), PATCH for fixes and wording.
Each release is tagged in git as `vX.Y.Z` and shown in the app footer.

## [1.0.2] - 2026-09-28

### App
- No figure carries a title or subtitle inside the image; the card header
  and the caption under it name and explain the figure (CONVENTIONS.md 6).

## [1.0.1] - 2026-09-28

### App
- The In Words tab lays out its three columns at fixed widths, so an
  equation no longer collapses to one term per line beside its note.
- The preset card no longer doubles the word "Stage" in front of a stage
  name that already carries it.
- The stage selector reads "Stage 1" to "Stage 5" (the ECON42550 lecture
  numbers 1.1 to 1.5 remain the values behind them), matching the other
  apps; on-screen text says "stage" throughout.

## [1.0.0] - 2026-09-28

First public release as a standalone repository.

### Model
- IS, PC and MP rule following Whelan (2023) ch. 1, with the closed-form
  solution of ch. 2 and the zero-lower-bound regime of ch. 4.
- Extensions beyond Whelan: a bank lending rate with a mark-up shock, a
  financial-accelerator term in the mark-up, and a slider for anchored
  expectations.

### App
- Five lectures (1.1 to 1.5) that add one layer of the model at a time.
- Nine worked examples with a narrated animation.
- Equations, Notation and In Words tabs that track the model at each lecture.
- Readout tiles that can be typed over (theta sets beta_pi, persistence sets
  lambda, the ZLB trigger sets r*).
- Ghost curves showing the loaded worked example alongside the live sliders.
