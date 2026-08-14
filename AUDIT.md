# Portfolio audit

This repository is a curated public version of an individual **Topics in Econometrics** assignment. The aim of the audit is to preserve the empirical work while removing machine-specific dependencies and avoiding the publication of known inconsistencies as validated results.

## Verified inventory

- 20 R exercise scripts
- 41 original PNG outputs
- 38 CSV outputs in the submitted archive
- 15 Excel input workbooks
- 48-page submitted report

The 41 PNGs in the coursework archive were compared to the 41 raster images embedded in the submitted PDF: all 41 matched exactly at the decoded-pixel level. The repository publishes those original PNGs unchanged.

## Portfolio changes

1. **Portable paths.** Removed machine-specific `setwd("C:/Users/...")` calls. Scripts now read from `data/raw/` and write to chapter-specific `figures/` and `results/` directories when run from the repository root.
2. **Dependency handling.** Removed runtime `install.packages()` calls from exercise scripts; package installation is centralized in `requirements.R`.
3. **Exercise 1.5.** The exercise asks for standard deviation. The R code already computes `Stdev`; the report table relabelled/squared it as variance. The public code follows the exercise and retains standard deviation.
4. **Exercise 2.3.** MA(1) has the narrow AIC/BIC lead, while the coursework deliberately retains AR(1) from the PACF interpretation. The public script states this as a modelling choice rather than implying AR(1) wins the information criteria.
5. **Exercise 2.7.** The public script explicitly prints the ARIMA `(p,d,q)` order selected by `auto.arima`, improving reproducibility of the model-selection step.
6. **Exercise 3.3 data endpoint.** The exercise text names January 1973-December 2008, but the supplied `m-intc7308.xlsx` workbook ends in December 2003. The public script preserves the supplied-data endpoint and does not fabricate 2004-2008 observations.
7. **Exercises 4.1-4.2: Student-t tail scaling.** `fGarch::garchFit(..., cond.dist = "std")` estimates a standardized Student-t innovation with unit variance. The submitted VaR/ES code subsequently used base-R `qt()`/`dt()` for the ordinary Student-t. The public scripts use `fGarch::qstd()`/`dstd()` and the matching standardized-t ES multiplier. The two legacy mixed Gaussian/Student-t VaR/ES summary CSVs are therefore not republished as validated results; they should be regenerated from the audited scripts using the original data.
8. **Exercise 4.1 persistence.** A fitted `alpha + beta` slightly above one is described as a near-integrated/non-stationary variance estimate, not as a formally constrained IGARCH model.
9. **15-day GE tail risk.** The sum-of-conditional-means / square-root-of-summed-variances calculation is retained as the coursework multi-step approximation and is labelled as such.

## What was not changed

Apart from path/dependency cleanup and the explicitly documented corrections above, the exercise specifications, model choices, diagnostics, parameter-estimation calls and plotting code are preserved. The historical figures remain the exact submitted outputs rather than regenerated substitutes.
