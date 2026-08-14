# Exercise 4.1
# Daily GE simple returns Jan 1998 - Dec 2008 from d-ge9808.xlsx (Dates, ge).
# Position: $1 million long; tail probability 0.01 (99% confidence).
# VaR and ES for 1-day and 15-day horizons under (a) Gaussian and (b) Student-t innovations.
# 15-day cumulative forecast via mu_cum = sum_h mu_{h+i}, sigma_cum = sqrt(sum sigma^2_{h+i}).

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_07")



ge_data <- read.xlsx(paths$data("d-ge9808.xlsx"), sheet = 1, detectDates = TRUE)
ge_data$Dates <- as.Date(ge_data$Dates)

dim(ge_data)
head(ge_data)
tail(ge_data)

ge_data$lrtn <- log(1 + ge_data$ge)
ge_log <- ge_data$lrtn
n_obs <- length(ge_log)


position <- 1e6
tail_prob <- 0.01
alpha_level <- 1 - tail_prob


plot_ge_timeseries <- ggplot(
  data.frame(Date = ge_data$Dates, lrtn = ge_log),
  aes(x = Date, y = lrtn)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  labs(title = "GE Daily Log Returns",
       subtitle = "January 1998 - December 2008",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_ge_timeseries)


# Mean equation identification

for (L in c(10, 15, 20)) {
  out <- Box.test(ge_log, lag = L, type = "Ljung-Box")
  cat(sprintf("Ljung-Box(lag=%2d) on GE log returns: Q=%.3f, p-value=%.4e\n",
              L, out$statistic, out$p.value))
}

demeaned_ge <- ge_log - mean(ge_log)
for (L in c(10, 20)) {
  out <- Box.test(demeaned_ge^2, lag = L, type = "Ljung-Box")
  cat(sprintf("Ljung-Box(lag=%2d) on squared demeaned: Q=%.2f, p-value=%.4e\n",
              L, out$statistic, out$p.value))
}

plot_acf_ge <- ggAcf(ge_log, lag.max = 20) +
  labs(title = "Sample ACF of GE daily log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))
plot_pacf_ge <- ggPacf(ge_log, lag.max = 20) +
  labs(title = "Sample PACF of GE daily log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))
print(plot_acf_ge)
print(plot_pacf_ge)

# Lags 1-2 are the dominant spikes (Q(10) rejects strongly), so I pick AR(2)
# for the conditional mean.


# (a) Gaussian AR(2)-GARCH(1,1)

m1 <- garchFit(lrtn ~ arma(2, 0) + garch(1, 1), data = ge_data,
               cond.dist = "norm", trace = FALSE)
cat("\n========== (a) Gaussian AR(2)-GARCH(1,1) on GE log returns ==========\n")
summary(m1)

coef_m1 <- coef(m1)
cat("\nFitted Gaussian AR(2)-GARCH(1,1):\n")
print(round(coef_m1, 6))

# On GE 1998-2008 the fitted alpha + beta is slightly above one.
# This is a near-integrated/non-stationary variance estimate, not a constrained IGARCH fit.
# Short-horizon forecasts can still be reported, but a finite unconditional variance is not implied.
persistence_n <- coef_m1["alpha1"] + coef_m1["beta1"]
cat("\nPersistence (alpha + beta) =", round(persistence_n, 6), "\n")


# 1-day forecast

forecast_1d_n <- predict(m1, n.ahead = 1)
mu_1d_n <- forecast_1d_n$meanForecast[1]
sigma_1d_n <- forecast_1d_n$standardDeviation[1]
cat("\n1-day Gaussian forecast at t =", n_obs, ":\n")
cat("mu_hat =", round(mu_1d_n, 6), "\n")
cat("sigma_hat =", round(sigma_1d_n, 6), "\n")


z_alpha <- qnorm(alpha_level)
phi_z_alpha <- dnorm(z_alpha)

VaR_1d_n <- -mu_1d_n + z_alpha * sigma_1d_n
ES_1d_n <- -mu_1d_n + sigma_1d_n * phi_z_alpha / (1 - alpha_level)

PL_VaR_1d_n <- position * VaR_1d_n
PL_ES_1d_n <- position * ES_1d_n

cat("\n1-day Gaussian VaR (99%): ", sprintf("%.6f (=> $%.2f)", VaR_1d_n, PL_VaR_1d_n), "\n")
cat("1-day Gaussian ES  (99%): ", sprintf("%.6f (=> $%.2f)", ES_1d_n, PL_ES_1d_n), "\n")


# 15-day cumulative forecast

# fGarch prints harmless warnings on meanError under near-IGARCH persistence;
# I suppress them because I only use the standardDeviation column.
forecast_15d_n <- suppressWarnings(predict(m1, n.ahead = 15))
print(forecast_15d_n)

mu_15d_n <- sum(forecast_15d_n$meanForecast)
var_15d_n <- sum(forecast_15d_n$standardDeviation^2)
sigma_15d_n <- sqrt(var_15d_n)

cat("\n15-day cumulative forecast (Gaussian):\n")
cat("mu_cum =", round(mu_15d_n, 6), "\n")
cat("sigma_cum =", round(sigma_15d_n, 6), "\n")

VaR_15d_n <- -mu_15d_n + z_alpha * sigma_15d_n
ES_15d_n <- -mu_15d_n + sigma_15d_n * phi_z_alpha / (1 - alpha_level)
PL_VaR_15d_n <- position * VaR_15d_n
PL_ES_15d_n <- position * ES_15d_n

cat("\n15-day Gaussian VaR (99%): ",
    sprintf("%.6f (=> $%.2f)", VaR_15d_n, PL_VaR_15d_n), "\n")
cat("15-day Gaussian ES  (99%): ",
    sprintf("%.6f (=> $%.2f)", ES_15d_n, PL_ES_15d_n), "\n")


# (b) Student-t AR(2)-GARCH(1,1)

m2 <- garchFit(lrtn ~ arma(2, 0) + garch(1, 1), data = ge_data,
               cond.dist = "std", trace = FALSE)
cat("\n========== (b) Student-t AR(2)-GARCH(1,1) on GE log returns ==========\n")
summary(m2)

coef_m2 <- coef(m2)
nu_hat <- coef_m2["shape"]
cat("\nFitted Student-t AR(2)-GARCH(1,1):\n")
print(round(coef_m2, 6))
cat("\nEstimated degrees of freedom nu =", round(nu_hat, 4), "\n")


# 1-day forecast

forecast_1d_t <- predict(m2, n.ahead = 1)
mu_1d_t <- forecast_1d_t$meanForecast[1]
sigma_1d_t <- forecast_1d_t$standardDeviation[1]
cat("\n1-day Student-t forecast at t =", n_obs, ":\n")
cat("mu_hat =", round(mu_1d_t, 6), "\n")
cat("sigma_hat =", round(sigma_1d_t, 6), "\n")

# fGarch cond.dist = "std" uses the standardized Student-t distribution
# (unit variance for any nu > 2), so use qstd/dstd rather than base-R qt/dt.
qstd_alpha <- qstd(alpha_level, mean = 0, sd = 1, nu = nu_hat)
dstd_q_alpha <- dstd(qstd_alpha, mean = 0, sd = 1, nu = nu_hat)
es_std_multiplier <- ((nu_hat - 2 + qstd_alpha^2) /
                      ((nu_hat - 1) * (1 - alpha_level))) * dstd_q_alpha

VaR_1d_t <- -mu_1d_t + qstd_alpha * sigma_1d_t
ES_1d_t <- -mu_1d_t + sigma_1d_t * es_std_multiplier

PL_VaR_1d_t <- position * VaR_1d_t
PL_ES_1d_t <- position * ES_1d_t

cat("\n1-day Student-t VaR (99%): ",
    sprintf("%.6f (=> $%.2f)", VaR_1d_t, PL_VaR_1d_t), "\n")
cat("1-day Student-t ES  (99%): ",
    sprintf("%.6f (=> $%.2f)", ES_1d_t, PL_ES_1d_t), "\n")


# 15-day cumulative forecast

forecast_15d_t <- suppressWarnings(predict(m2, n.ahead = 15))
print(forecast_15d_t)

mu_15d_t <- sum(forecast_15d_t$meanForecast)
var_15d_t <- sum(forecast_15d_t$standardDeviation^2)
sigma_15d_t <- sqrt(var_15d_t)

cat("\n15-day cumulative forecast (Student-t):\n")
cat("mu_cum =", round(mu_15d_t, 6), "\n")
cat("sigma_cum =", round(sigma_15d_t, 6), "\n")

# Multi-day aggregation remains the coursework approximation: aggregate
# conditional means/variances, then apply the fitted standardized-t tail multiplier.
VaR_15d_t <- -mu_15d_t + qstd_alpha * sigma_15d_t
ES_15d_t <- -mu_15d_t + sigma_15d_t * es_std_multiplier
PL_VaR_15d_t <- position * VaR_15d_t
PL_ES_15d_t <- position * ES_15d_t

cat("\n15-day Student-t VaR (99%): ",
    sprintf("%.6f (=> $%.2f)", VaR_15d_t, PL_VaR_15d_t), "\n")
cat("15-day Student-t ES  (99%): ",
    sprintf("%.6f (=> $%.2f)", ES_15d_t, PL_ES_15d_t), "\n")


# Summary table

VaR_ES_summary <- data.frame(
  Distribution = rep(c("Gaussian", "Student-t"), each = 2),
  Horizon = rep(c("1-day", "15-day"), times = 2),
  VaR_return = c(VaR_1d_n, VaR_15d_n, VaR_1d_t, VaR_15d_t),
  ES_return = c(ES_1d_n, ES_15d_n, ES_1d_t, ES_15d_t),
  VaR_USD = c(PL_VaR_1d_n, PL_VaR_15d_n, PL_VaR_1d_t, PL_VaR_15d_t),
  ES_USD = c(PL_ES_1d_n, PL_ES_15d_n, PL_ES_1d_t, PL_ES_15d_t),
  row.names = NULL
)
kable(VaR_ES_summary, digits = 6,
      caption = paste("Exercise 4.1 - VaR and Expected Shortfall (tail probability",
                      "0.01) for a $1,000,000 long position in GE stock"))


write.csv(VaR_ES_summary, paths$result("exercise4_1_VaR_ES_summary.csv"), row.names = FALSE)

ggsave(paths$figure("exercise4_1_timeseries.png"),
       plot = plot_ge_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(paths$figure("exercise4_1_acf_returns.png"),
       plot = plot_acf_ge,
       width = 9, height = 5, units = "in", dpi = 300)

ggsave(paths$figure("exercise4_1_pacf_returns.png"),
       plot = plot_pacf_ge,
       width = 9, height = 5, units = "in", dpi = 300)
