# Financial Time-Series Econometrics in R

Individual university coursework for **Topics in Econometrics**, organized as a compact empirical research portfolio in R. The project spans return diagnostics, linear time-series models, conditional volatility, volatility forecasting and market tail risk across **20 exercises**.

## Scope

| Block | Exercises | Main methods |
|---|---:|---|
| Returns & distributions | 5 | simple/log returns, descriptive moments, t-tests, skewness/kurtosis tests, empirical densities |
| ARMA / ARIMA | 7 | ACF/PACF, Ljung-Box, MA/AR/ARMA forecasts, long-memory diagnostics, unit-root tests, ARIMA |
| ARCH / GARCH | 6 | ARCH-LM, ARCH, ARCH-M, GARCH, GARCH-M, EGARCH, Gaussian/Student-t innovations, multi-step volatility forecasts |
| VaR / Expected Shortfall | 2 | Gaussian and standardized Student-t ARMA-GARCH/GARCH, 1-day and multi-day VaR/ES, residual diagnostics |

The empirical datasets include US equities, CRSP market-cap portfolios and indices, FX rates, Moody's corporate bond yields and the US GDP implicit price deflator. Raw third-party workbooks are not redistributed.

## Selected empirical findings

- Daily and monthly equity returns show asymmetric, heavy-tailed distributions; Gaussian benchmarks miss material tail mass.
- Small-cap portfolio returns display stronger serial dependence than large-cap returns in the sample.
- IBM absolute daily returns retain positive autocorrelation over 100 lags even when raw-return autocorrelation is weak, illustrating persistent volatility clustering.
- The GDP deflator is treated as an integrated series and forecast through a low-order ARIMA specification on the log level.
- ARCH/GARCH-family models capture strong conditional heteroskedasticity in Intel, Merck, 3M and GM returns; Student-t innovations improve heavy-tail modelling where applicable.
- GE and Cisco tail-risk exercises show how distributional assumptions materially affect VaR/ES. The public Chapter 7 scripts include an audit correction so Student-t tail calculations are consistent with `fGarch`'s **standardized** Student-t convention.

## Original output gallery

All **41 figures** in [`figures/`](figures/) are the original R output files from the coursework archive. They were checked against the submitted PDF at the decoded-pixel level: **41/41 are exact matches**. No chart has been reconstructed, smoothed or restyled.

### Return distributions and forecasting

<table><tr>
<td width="50%" valign="top"><strong>Daily equity return distributions</strong><br><img src="figures/chapter_01/exercise1_1_log_returns_distributions.png" width="100%"></td>
<td width="50%" valign="top"><strong>Decile 2 AR(1) forecast</strong><br><img src="figures/chapter_02/exercise2_3_forecast.png" width="100%"></td>
</tr></table>

### Persistence and macro time series

<table><tr>
<td width="50%" valign="top"><strong>IBM absolute-return persistence</strong><br><img src="figures/chapter_02/exercise2_4_acf_absolute.png" width="100%"></td>
<td width="50%" valign="top"><strong>GDP-deflator quarterly inflation</strong><br><img src="figures/chapter_02/exercise2_7_inflation.png" width="100%"></td>
</tr></table>

### Conditional volatility

<table><tr>
<td width="50%" valign="top"><strong>Intel GARCH volatility forecast</strong><br><img src="figures/chapter_03/exercise3_3_volatility_forecast.png" width="100%"></td>
<td width="50%" valign="top"><strong>GM volatility forecast comparison</strong><br><img src="figures/chapter_03/exercise3_6_forecast_comparison.png" width="100%"></td>
</tr></table>

### Tail-risk modelling

<table><tr>
<td width="50%" valign="top"><strong>GE return ACF for AR mean identification</strong><br><img src="figures/chapter_07/exercise4_1_acf_returns.png" width="100%"></td>
<td width="50%" valign="top"><strong>Cisco Gaussian-GARCH conditional volatility</strong><br><img src="figures/chapter_07/exercise4_2_conditional_volatility.png" width="100%"></td>
</tr></table>

See [`figures/README.md`](figures/README.md) for the complete 41-figure archive.

## Repository structure

```text
.
├── R/
│   ├── utils.R
│   ├── chapter_01_returns/      # 5 scripts
│   ├── chapter_02_arma_arima/   # 7 scripts
│   ├── chapter_03_arch_garch/   # 6 scripts
│   └── chapter_07_var/          # 2 scripts
├── figures/                     # 41 exact submitted PNG outputs
├── results/                     # audited machine-readable CSV outputs
├── data/README.md               # required input filenames / data policy
├── requirements.R
├── AUDIT.md
└── README.md
```

## Reproducibility

1. Obtain the original course/textbook workbooks and place them in `data/raw/` using the filenames listed in [`data/README.md`](data/README.md).
2. From the repository root, install missing packages with:

```r
source("requirements.R")
```

3. Run any exercise script from the repository root, for example:

```r
source("R/chapter_03_arch_garch/exercise3_6.R")
```

Outputs are written to the corresponding chapter folders under `figures/` and `results/`.

## Audit notes

The public version is not a blind copy of the submission. A line-by-line portfolio audit removed machine-specific paths and documented/corrected several issues without silently rewriting the empirical project. Most importantly:

- Exercise 1.5 follows the exercise request and reports **standard deviation** rather than reproducing the report's variance relabelling.
- Exercise 2.3 makes clear that AR(1) is a deliberate PACF-driven choice even though MA(1) has a narrow AIC/BIC advantage.
- Exercise 3.3 documents that the supplied Intel workbook ends in December 2003 even though the exercise text names December 2008; the code uses the data actually supplied rather than inventing missing observations.
- Chapter 7 Student-t VaR/ES now uses the standardized Student-t distribution corresponding to `fGarch::garchFit(..., cond.dist = "std")`; the two legacy mixed Gaussian/Student-t VaR/ES summary CSVs are not presented as audited results.
- GE's slightly-above-one GARCH persistence is not mislabeled as a formally constrained IGARCH fit.

The full change log and provenance policy are in [`AUDIT.md`](AUDIT.md).

## Software

Core packages: `forecast`, `fGarch`, `rugarch`, `tseries`, `fBasics`, `openxlsx`, `ggplot2`, `dplyr` and `kableExtra`.

## Authorship

This was an **individual** Topics in Econometrics assignment completed by **Simone D'Isabella** during the 2025/2026 academic year. The public repository is a portfolio-oriented curation of that work.

## Scope disclaimer

This repository documents academic econometric analysis on historical data. It is not investment advice and is not a production trading or risk-management system.
