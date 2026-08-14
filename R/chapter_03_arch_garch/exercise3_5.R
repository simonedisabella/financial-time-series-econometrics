# Exercise 3.5
# Monthly 3M stock simple returns from m-3m4608.xlsx (Date, rtn). n = 755.
# (a) ARCH test; (b) PACF -> ARCH order, fit; (c) refit on first 750 and forecast 5 ahead;
# (d) ARCH-M with test on gamma; (e) EGARCH on first 750 with 5-step forecast.
# fGarch does not support ARCH-M / EGARCH, so I use rugarch for (d) and (e).

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)
library(rugarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_03")



engle_arch_lm_test <- function(x, lags = 12) {
  x <- as.vector(x)
  m <- lags + 1
  mat <- embed(x^2, m)
  fit <- summary(lm(mat[, 1] ~ mat[, -1]))
  lm_statistic <- fit$r.squared * length(resid(fit))
  p_value <- 1 - pchisq(lm_statistic, df = lags)
  list(statistic = c("Chi-squared" = lm_statistic),
       parameter = c("df" = lags),
       p.value = p_value,
       method = "ARCH LM-test;  Null hypothesis: no ARCH effects")
}


mmm_data <- read.xlsx(paths$data("m-3m4608.xlsx"), sheet = 1, detectDates = TRUE)
mmm_data$Date <- as.Date(mmm_data$Date)

dim(mmm_data)
head(mmm_data)
tail(mmm_data)

mmm_data$lrtn <- log(1 + mmm_data$rtn)
mmm_log <- mmm_data$lrtn
n_obs <- length(mmm_log)


plot_mmm_timeseries <- ggplot(
  data.frame(Date = mmm_data$Date, lrtn = mmm_log),
  aes(x = Date, y = lrtn)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "3M Monthly Log Returns",
       subtitle = "February 1946 - December 2008",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_mmm_timeseries)


# (a) Test for ARCH effects on squared demeaned log returns

demeaned_log <- mmm_log - mean(mmm_log)

lb_sq_6 <- Box.test(demeaned_log^2, lag = 6, type = "Ljung-Box")
lb_sq_12 <- Box.test(demeaned_log^2, lag = 12, type = "Ljung-Box")
arch_LM <- engle_arch_lm_test(demeaned_log, lags = 12)

print(lb_sq_6)
print(lb_sq_12)
cat("\n\tEngle ARCH LM test (lag 12)\n",
    "Chi-squared =", round(arch_LM$statistic, 4),
    ", df = ", arch_LM$parameter,
    ", p-value = ", format(arch_LM$p.value, digits = 4, scientific = TRUE), "\n\n")

arch_tests_table <- data.frame(
  Test = c("Ljung-Box on squared demeaned, lag 6",
           "Ljung-Box on squared demeaned, lag 12",
           "Engle LM ARCH test, lag 12"),
  Statistic = c(lb_sq_6$statistic, lb_sq_12$statistic, arch_LM$statistic),
  p_value = c(lb_sq_6$p.value, lb_sq_12$p.value, arch_LM$p.value),
  reject_H0_5pc = c(lb_sq_6$p.value < 0.05,
                    lb_sq_12$p.value < 0.05,
                    arch_LM$p.value < 0.05),
  row.names = NULL
)
kable(arch_tests_table, digits = 4,
      caption = "Exercise 3.5(a) - Tests for ARCH effects on 3M log returns")


# (b) PACF of squared returns -> ARCH order, fit and compare

plot_pacf_squared <- ggPacf(demeaned_log^2, lag.max = 15) +
  labs(title = "Sample PACF of squared demeaned 3M log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))
print(plot_pacf_squared)

# Spikes at lags 1 and 2, cut-off after lag 2: ARCH(2). I fit ARCH(1), (2), (3).
arch1_mmm <- garchFit(lrtn ~ garch(1, 0), data = mmm_data,
                      cond.dist = "norm", trace = FALSE)
arch2_mmm <- garchFit(lrtn ~ garch(2, 0), data = mmm_data,
                      cond.dist = "norm", trace = FALSE)
arch3_mmm <- garchFit(lrtn ~ garch(3, 0), data = mmm_data,
                      cond.dist = "norm", trace = FALSE)

ics_list <- list(arch1_mmm@fit$ics, arch2_mmm@fit$ics, arch3_mmm@fit$ics)
arch_order_compare <- data.frame(
  Model = c("ARCH(1)", "ARCH(2)", "ARCH(3)"),
  LogLik = c(-arch1_mmm@fit$llh, -arch2_mmm@fit$llh, -arch3_mmm@fit$llh),
  AIC = sapply(ics_list, function(x) x["AIC"]),
  BIC = sapply(ics_list, function(x) x["BIC"]),
  row.names = NULL
)
kable(arch_order_compare, digits = 4,
      caption = "Exercise 3.5(b) - ARCH order selection on 3M")

# ARCH(2) wins on BIC, coherent with the PACF cut-off
cat("\n========== Final ARCH(2) on full sample ==========\n")
summary(arch2_mmm)

coef_arch2 <- coef(arch2_mmm)
cat("\nFitted ARCH(2) model (full sample, n =", n_obs, "):\n")
print(round(coef_arch2, 6))


# (c) Refit on first 750 obs, predict 5 ahead

train_data <- mmm_data[1:750, ]
arch2_train <- garchFit(lrtn ~ garch(2, 0), data = train_data,
                        cond.dist = "norm", trace = FALSE)
cat("\n========== ARCH(2) on first 750 observations ==========\n")
summary(arch2_train)

forecast_arch2 <- predict(arch2_train, n.ahead = 5)
print(forecast_arch2)

actual_returns <- mmm_log[751:755]

forecast_table_arch2 <- data.frame(
  Step = 1:5,
  Date = mmm_data$Date[751:755],
  Volatility_forecast = round(forecast_arch2$standardDeviation, 6),
  Actual_log_return = round(actual_returns, 6),
  Actual_absolute = round(abs(actual_returns), 6)
)
kable(forecast_table_arch2, digits = 6,
      caption = paste("Exercise 3.5(c) - 5-step volatility forecasts from",
                      "ARCH(2) fitted on first 750 obs (origin = 750)"))


# (d) ARCH-M model and Wald test on the risk premium gamma
# rugarch parameterises GARCH-in-mean as r_t = mu + gamma * sigma_t + a_t
# (archm = TRUE, archpow = 1). H0: gamma = 0 means no risk-volatility link.

spec_archm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                    archm = TRUE, archpow = 1),
  distribution.model = "norm"
)
archm_fit <- ugarchfit(spec = spec_archm, data = mmm_log, solver = "hybrid")

cat("\n========== ARCH-M(2) model on 3M (full sample) ==========\n")
show(archm_fit)

archm_pars <- archm_fit@fit$matcoef
print(round(archm_pars, 6))

gamma_estimate <- archm_pars["archm", " Estimate"]
gamma_se <- archm_pars["archm", " Std. Error"]
gamma_t <- gamma_estimate / gamma_se
gamma_p <- 2 * (1 - pnorm(abs(gamma_t)))

cat("\nHypothesis test on the risk premium gamma:\n")
cat("gamma_hat =", round(gamma_estimate, 6), "\n")
cat("SE(gamma) =", round(gamma_se, 6), "\n")
cat("t statistic =", round(gamma_t, 4), "\n")
cat("p-value =", format(gamma_p, digits = 4, scientific = TRUE), "\n")
cat("Reject H0 at 5%? ", gamma_p < 0.05, "\n")

archm_test_table <- data.frame(
  Parameter = "gamma (risk premium)",
  Estimate = round(gamma_estimate, 6),
  Std_Error = round(gamma_se, 6),
  t_stat = round(gamma_t, 4),
  p_value = gamma_p,
  reject_H0_5pc = gamma_p < 0.05
)
kable(archm_test_table, digits = 4,
      caption = "Exercise 3.5(d) - Test of risk premium gamma = 0 in ARCH-M(2)")


# (e) EGARCH on first 750 obs - 5-step volatility forecast at h = 750

spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)
egarch_train <- ugarchfit(spec = spec_egarch, data = mmm_log[1:750],
                          solver = "hybrid")
cat("\n========== EGARCH(1,1) on first 750 observations ==========\n")
show(egarch_train)

egarch_forecast <- ugarchforecast(egarch_train, n.ahead = 5)
egarch_sigma <- as.numeric(sigma(egarch_forecast))
egarch_mean <- as.numeric(fitted(egarch_forecast))

forecast_table_egarch <- data.frame(
  Step = 1:5,
  Date = mmm_data$Date[751:755],
  Mean_forecast = round(egarch_mean, 6),
  Volatility = round(egarch_sigma, 6),
  Variance = round(egarch_sigma^2, 8),
  Actual_log_return = round(actual_returns, 6)
)
kable(forecast_table_egarch, digits = 6,
      caption = paste("Exercise 3.5(e) - 5-step volatility forecasts from",
                      "EGARCH(1,1) fitted on first 750 obs (origin = 750)"))

egarch_coef <- coef(egarch_train)
cat("\nFitted EGARCH(1,1) (first 750 obs) for 3M log returns:\n")
print(round(egarch_coef, 6))


write.csv(arch_tests_table, paths$result("exercise3_5_arch_tests.csv"), row.names = FALSE)
write.csv(arch_order_compare, paths$result("exercise3_5_arch_order_compare.csv"), row.names = FALSE)
write.csv(forecast_table_arch2, paths$result("exercise3_5_arch2_forecasts.csv"), row.names = FALSE)
write.csv(archm_test_table, paths$result("exercise3_5_archm_test.csv"), row.names = FALSE)
write.csv(forecast_table_egarch, paths$result("exercise3_5_egarch_forecasts.csv"), row.names = FALSE)

ggsave(paths$figure("exercise3_5_timeseries.png"),
       plot = plot_mmm_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(paths$figure("exercise3_5_pacf_squared.png"),
       plot = plot_pacf_squared,
       width = 9, height = 5, units = "in", dpi = 300)
