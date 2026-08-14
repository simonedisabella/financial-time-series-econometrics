# R source

The repository contains **20 exercise scripts** grouped by topic. The submitted analytical logic is preserved, with a small portfolio audit layer for portability and correctness.

- `chapter_01_returns/` - returns, descriptive statistics, hypothesis tests, FX distributions
- `chapter_02_arma_arima/` - MA/AR/ARMA, long-memory diagnostics, bond yields, ARIMA
- `chapter_03_arch_garch/` - ARCH, ARCH-M, GARCH, GARCH-M, EGARCH, volatility forecasts
- `chapter_07_var/` - Gaussian/Student-t GARCH VaR and Expected Shortfall

Run scripts from the repository root after placing the required original Excel files in `data/raw/`. Install dependencies with `source("requirements.R")`.

See [`AUDIT.md`](../AUDIT.md) for the exact changes made relative to the submitted archive.
