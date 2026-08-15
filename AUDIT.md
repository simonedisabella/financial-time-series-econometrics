# Portfolio audit

This repository is a curated public version of an individual **Topics in Econometrics** assignment. The audit keeps the empirical work intact while removing machine-specific dependencies and making known limitations explicit.

## Verified inventory

- 20 R exercise scripts
- 41 original PNG outputs
- 38 CSV outputs in the submitted archive
- 15 Excel input workbooks
- 48-page submitted report

The 41 PNGs in the coursework archive were compared with the 41 raster images embedded in the submitted PDF. All 41 matched at the decoded-pixel level, and those original PNGs are the ones published here.

## Changes made for the public version

1. **Portable paths.** Machine-specific `setwd("C:/Users/...")` calls were removed. Scripts now read from `data/raw/` and write to chapter-specific `figures/` and `results/` folders when run from the repository root.
2. **Dependency handling.** Package installation was moved out of the exercise scripts and centralized in `requirements.R`.
3. **Exercise 1.5.** The exercise asks for standard deviation. The original R code computes it, while the submitted report relabelled/squared it as variance. The public code follows the exercise and reports standard deviation.
4. **Exercise 2.3.** MA(1) has a narrow AIC/BIC advantage, while the coursework retained AR(1) from the PACF interpretation. The public script presents AR(1) as that modelling choice rather than as the information-criterion winner.
5. **Exercise 2.7.** The public script prints the ARIMA `(p,d,q)` order selected by `auto.arima`, so the model-selection step is explicit.
6. **Exercise 3.3 data endpoint.** The exercise text names January 1973-December 2008, but the supplied `m-intc7308.xlsx` workbook ends in December 2003. The public script uses the supplied endpoint and does not invent 2004-2008 observations.
7. **Exercises 4.1-4.2: Student-t scaling.** `fGarch::garchFit(..., cond.dist = "std")` uses a standardized Student-t innovation with unit variance. The submitted VaR/ES code then used base-R `qt()`/`dt()`, which correspond to the ordinary Student-t. The public scripts use `fGarch::qstd()`/`dstd()` and the matching standardized-t ES multiplier. The two legacy mixed Gaussian/Student-t summary CSVs are therefore not presented as validated results; they should be regenerated from the audited scripts with the original data.
8. **Exercise 4.1 persistence.** A fitted `alpha + beta` slightly above one is described as a near-integrated/non-stationary variance estimate, not as a formally constrained IGARCH fit.
9. **15-day GE tail risk.** Summing conditional means and variances and then applying a one-period tail multiplier is retained as the coursework multi-step approximation and labelled as such; it is not presented as the exact distribution of the 15-day GARCH return.
10. **Exercises 3.1-3.2: higher-order GARCH convergence.** The submitted report informally interprets total persistence of 0.95 as if the forecast gap decayed by a single 0.95 factor each step and quotes an AR(1)-style half-life. For GARCH(1,2) and GARCH(2,1), the multi-step variance forecast follows a second-order recursion, so convergence is governed by the roots of that recursion. The public code keeps the correct recursions and figures but does not rely on the one-root half-life interpretation.

## What was not changed

Apart from the path/dependency cleanup and the corrections documented above, the exercise specifications, model choices, estimation calls, diagnostics and plotting code are preserved. Historical figures remain the submitted outputs rather than regenerated substitutes.
