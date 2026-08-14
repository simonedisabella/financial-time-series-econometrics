# Machine-readable results

This directory preserves the submitted CSV outputs that passed the portfolio audit. Files are grouped by chapter.

**Chapter 7 note.** The two submitted VaR/ES summary CSVs each mix Gaussian and Student-t rows. They are intentionally omitted as whole files because the submitted Student-t rows combined `fGarch::garchFit(..., cond.dist = "std")` with ordinary `qt`/`dt` tail multipliers. The audited public scripts use `qstd`/`dstd`, consistent with fGarch's standardized Student-t convention. Re-run `exercise4_1.R` and `exercise4_2.R` with the original data to regenerate complete corrected summary files. The Gaussian calculations and the Cisco diagnostic CSV are unchanged.

## Chapter 1

- [`exercise1_1_log_summary.csv`](chapter_01/exercise1_1_log_summary.csv)
- [`exercise1_1_simple_summary.csv`](chapter_01/exercise1_1_simple_summary.csv)
- [`exercise1_1_ttests.csv`](chapter_01/exercise1_1_ttests.csv)
- [`exercise1_2_log_summary.csv`](chapter_01/exercise1_2_log_summary.csv)
- [`exercise1_2_simple_summary.csv`](chapter_01/exercise1_2_simple_summary.csv)
- [`exercise1_2_ttests.csv`](chapter_01/exercise1_2_ttests.csv)
- [`exercise1_3_summary.csv`](chapter_01/exercise1_3_summary.csv)
- [`exercise1_4_tests.csv`](chapter_01/exercise1_4_tests.csv)
- [`exercise1_5_summary.csv`](chapter_01/exercise1_5_summary.csv)

## Chapter 2

- [`exercise2_1_summary.csv`](chapter_02/exercise2_1_summary.csv)
- [`exercise2_2_summary.csv`](chapter_02/exercise2_2_summary.csv)
- [`exercise2_3_forecasts.csv`](chapter_02/exercise2_3_forecasts.csv)
- [`exercise2_3_ljungbox.csv`](chapter_02/exercise2_3_ljungbox.csv)
- [`exercise2_3_model_comparison.csv`](chapter_02/exercise2_3_model_comparison.csv)
- [`exercise2_4_acf_full.csv`](chapter_02/exercise2_4_acf_full.csv)
- [`exercise2_4_acf_summary.csv`](chapter_02/exercise2_4_acf_summary.csv)
- [`exercise2_5_summary.csv`](chapter_02/exercise2_5_summary.csv)
- [`exercise2_5_tests.csv`](chapter_02/exercise2_5_tests.csv)
- [`exercise2_6_forecasts.csv`](chapter_02/exercise2_6_forecasts.csv)
- [`exercise2_6_model_comparison.csv`](chapter_02/exercise2_6_model_comparison.csv)
- [`exercise2_7_forecasts.csv`](chapter_02/exercise2_7_forecasts.csv)

## Chapter 3

- [`exercise3_1_forecasts.csv`](chapter_03/exercise3_1_forecasts.csv)
- [`exercise3_2_forecasts.csv`](chapter_03/exercise3_2_forecasts.csv)
- [`exercise3_3_forecasts.csv`](chapter_03/exercise3_3_forecasts.csv)
- [`exercise3_3_model_comparison.csv`](chapter_03/exercise3_3_model_comparison.csv)
- [`exercise3_3_preliminary_tests.csv`](chapter_03/exercise3_3_preliminary_tests.csv)
- [`exercise3_4_arch_tests.csv`](chapter_03/exercise3_4_arch_tests.csv)
- [`exercise3_4_model_comparison.csv`](chapter_03/exercise3_4_model_comparison.csv)
- [`exercise3_5_arch2_forecasts.csv`](chapter_03/exercise3_5_arch2_forecasts.csv)
- [`exercise3_5_arch_order_compare.csv`](chapter_03/exercise3_5_arch_order_compare.csv)
- [`exercise3_5_arch_tests.csv`](chapter_03/exercise3_5_arch_tests.csv)
- [`exercise3_5_archm_test.csv`](chapter_03/exercise3_5_archm_test.csv)
- [`exercise3_5_egarch_forecasts.csv`](chapter_03/exercise3_5_egarch_forecasts.csv)
- [`exercise3_6_dof_test.csv`](chapter_03/exercise3_6_dof_test.csv)
- [`exercise3_6_forecast_comparison.csv`](chapter_03/exercise3_6_forecast_comparison.csv)

## Chapter 7

- [`exercise4_2_diagnostics.csv`](chapter_07/exercise4_2_diagnostics.csv)
