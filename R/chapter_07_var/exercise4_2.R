# Exercise 4.2
# Daily Cisco simple returns 1998-2008 from d-csco9808.xlsx (Dates, rtn). n = 2767.
# Position: $1 million long; tail probability 0.01 (99% confidence).
# 1-day VaR/ES under (a) Gaussian and (b) Student-t GARCH(1,1).
# (c) Diagnostic check on the Gaussian fit.

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_07")



# Engle's ARCH-LM written by hand so I don't depend on FinTS
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


csco_data <- read.xlsx(paths$data("d-csco9808.xlsx"), sheet = 1, detectDates = TRUE)
csco_data$Dates <- as.Date(csco_data$Dates)

dim(csco_data)
head(csco_data)
tail(csco_data)

csco_data$lrtn <- log(1 + csco_data$rtn)
csco_log <- csco_data$lrtn
n_obs <- length(csco_log)


position <- 1e6
tail_prob <- 0.01
alpha_level <- 1 - tail_prob


plot_csco_timeseries <- ggplot(
  data.frame(Date = csco_data$Dates, lrtn = csco_log),
  aes(x = Date, y = lrtn)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  labs(title = "Cisco Daily Log Returns",
       subtitle = "January 1998 - December 2008",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_csco_timeseries)

demeaned_csco <- csco_log - mean(csco_log)
lb_sq_10 <- Box.test(demeaned_csco^2, lag = 10, type = "Ljung-Box")
arch_LM_csco <- engle_arch_lm_test(demeaned_csco, lags = 12)
cat("Preliminary ARCH check:\n")
print(lb_sq_10)
cat("Engle ARCH LM (lag 12): Chi^2 =",
    round(arch_LM_csco$statistic, 2),
    "  p-value =", format(arch_LM_csco$p.value, digits = 3, scientific = TRUE), "\n")


# (a) Gaussian GARCH(1,1)

m1 <- garchFit(lrtn ~ garch(1, 1), data = csco_data,
               cond.dist = "norm", trace = FALSE)
cat("\n========== (a) Gaussian GARCH(1,1) on Cisco log returns ==========\n")
summary(m1)

coef_m1 <- coef(m1)
cat("\nFitted Gaussian GARCH(1,1):\n")
print(round(coef_m1, 6))

persistence_n <- coef_m1["alpha1"] + coef_m1["beta1"]
uncond_var_n <- coef_m1["omega"] / (1 - persistence_n)
cat("\nPersistence (alpha + beta) =", round(persistence_n, 6), "\n")
cat("Unconditional variance     =", round(uncond_var_n, 8), "\n")


# 1-day forecast

forecast_n <- predict(m1, n.ahead = 1)
mu_n <- forecast_n$meanForecast[1]
sigma_n <- forecast_n$standardDeviation[1]
cat("\n1-day Gaussian forecast (origin = last sample day):\n")
cat("mu_hat =", round(mu_n, 6), "\n")
cat("sigma_hat =", round(sigma_n, 6), "\n")


z_alpha <- qnorm(alpha_level)
phi_z_alpha <- dnorm(z_alpha)

VaR_n <- -mu_n + z_alpha * sigma_n
ES_n <- -mu_n + sigma_n * phi_z_alpha / (1 - alpha_level)
PL_VaR_n <- position * VaR_n
PL_ES_n <- position * ES_n

cat("\n1-day Gaussian VaR (99%): ",
    sprintf("%.6f (=> $%.2f)", VaR_n, PL_VaR_n), "\n")
cat("1-day Gaussian ES  (99%): ",
    sprintf("%.6f (=> $%.2f)", ES_n, PL_ES_n), "\n")


# (b) Student-t GARCH(1,1)

m2 <- garchFit(lrtn ~ garch(1, 1), data = csco_data,
               cond.dist = "std", trace = FALSE)
cat("\n========== (b) Student-t GARCH(1,1) on Cisco log returns ==========\n")
summary(m2)

coef_m2 <- coef(m2)
nu_hat <- coef_m2["shape"]
cat("\nFitted Student-t GARCH(1,1):\n")
print(round(coef_m2, 6))
cat("\nEstimated degrees of freedom nu =", round(nu_hat, 4), "\n")


forecast_t <- predict(m2, n.ahead = 1)
mu_t <- forecast_t$meanForecast[1]
sigma_t <- forecast_t$standardDeviation[1]
cat("\n1-day Student-t forecast:\n")
cat("mu_hat =", round(mu_t, 6), "\n")
cat("sigma_hat =", round(sigma_t, 6), "\n")

# fGarch cond.dist = "std" uses the standardized Student-t distribution
# (unit variance for any nu > 2), so use qstd/dstd rather than base-R qt/dt.
qstd_alpha <- qstd(alpha_level, mean = 0, sd = 1, nu = nu_hat)
dstd_q_alpha <- dstd(qstd_alpha, mean = 0, sd = 1, nu = nu_hat)
es_std_multiplier <- ((nu_hat - 2 + qstd_alpha^2) /
                      ((nu_hat - 1) * (1 - alpha_level))) * dstd_q_alpha

VaR_t <- -mu_t + qstd_alpha * sigma_t
ES_t <- -mu_t + sigma_t * es_std_multiplier
PL_VaR_t <- position * VaR_t
PL_ES_t <- position * ES_t

cat("\n1-day Student-t VaR (99%): ",
    sprintf("%.6f (=> $%.2f)", VaR_t, PL_VaR_t), "\n")
cat("1-day Student-t ES  (99%): ",
    sprintf("%.6f (=> $%.2f)", ES_t, PL_ES_t), "\n")


# (c) Diagnostic check on the Gaussian GARCH(1,1)

cat("\n========== (c) Diagnostic check on Gaussian GARCH(1,1) ==========\n")

sigma_t_fit <- volatility(m1, type = "sigma")
resid_raw <- residuals(m1, standardize = FALSE)
resid_std <- residuals(m1, standardize = TRUE)

# Ljung-Box on z_t (mean equation adequacy)
diag_LB_5 <- Box.test(resid_std, lag = 5, type = "Ljung-Box")
diag_LB_10 <- Box.test(resid_std, lag = 10, type = "Ljung-Box")
diag_LB_20 <- Box.test(resid_std, lag = 20, type = "Ljung-Box")

# Ljung-Box on z_t^2 (variance equation adequacy)
diag_LB2_5 <- Box.test(resid_std^2, lag = 5, type = "Ljung-Box")
diag_LB2_10 <- Box.test(resid_std^2, lag = 10, type = "Ljung-Box")
diag_LB2_20 <- Box.test(resid_std^2, lag = 20, type = "Ljung-Box")

diag_archLM <- engle_arch_lm_test(resid_std, lags = 10)

# Jarque-Bera by hand
jb_skew <- mean(((resid_std - mean(resid_std)) / sd(resid_std))^3)
jb_kurt <- mean(((resid_std - mean(resid_std)) / sd(resid_std))^4) - 3
jb_stat <- n_obs * (jb_skew^2 / 6 + jb_kurt^2 / 24)
jb_pvalue <- 1 - pchisq(jb_stat, df = 2)

cat("\nDiagnostics on standardised residuals z_t = a_t / sigma_t:\n")
cat("\n  Ljung-Box on z_t (mean adequacy):\n")
cat(sprintf("    lag  5: Q = %7.3f,  p-value = %.4f\n", diag_LB_5$statistic, diag_LB_5$p.value))
cat(sprintf("    lag 10: Q = %7.3f,  p-value = %.4f\n", diag_LB_10$statistic, diag_LB_10$p.value))
cat(sprintf("    lag 20: Q = %7.3f,  p-value = %.4f\n", diag_LB_20$statistic, diag_LB_20$p.value))

cat("\n  Ljung-Box on z_t^2 (variance adequacy):\n")
cat(sprintf("    lag  5: Q = %7.3f,  p-value = %.4f\n", diag_LB2_5$statistic, diag_LB2_5$p.value))
cat(sprintf("    lag 10: Q = %7.3f,  p-value = %.4f\n", diag_LB2_10$statistic, diag_LB2_10$p.value))
cat(sprintf("    lag 20: Q = %7.3f,  p-value = %.4f\n", diag_LB2_20$statistic, diag_LB2_20$p.value))

cat("\n  Engle LM ARCH test on z_t (lag 10): Chi^2 =",
    round(diag_archLM$statistic, 3),
    ", p-value =", round(diag_archLM$p.value, 4), "\n")

cat("\n  Jarque-Bera normality test on z_t:\n")
cat("    Sample skewness         =", round(jb_skew, 4), "\n")
cat("    Sample excess kurtosis  =", round(jb_kurt, 4), "\n")
cat("    JB statistic            =", round(jb_stat, 3), "\n")
cat("    p-value                 =", format(jb_pvalue, digits = 3, scientific = TRUE), "\n")

diag_summary <- data.frame(
  Test = c("Ljung-Box on z_t, lag 5",
           "Ljung-Box on z_t, lag 10",
           "Ljung-Box on z_t, lag 20",
           "Ljung-Box on z_t^2, lag 5",
           "Ljung-Box on z_t^2, lag 10",
           "Ljung-Box on z_t^2, lag 20",
           "Engle LM ARCH test, lag 10",
           "Jarque-Bera normality test"),
  Statistic = c(diag_LB_5$statistic, diag_LB_10$statistic, diag_LB_20$statistic,
                diag_LB2_5$statistic, diag_LB2_10$statistic, diag_LB2_20$statistic,
                diag_archLM$statistic, jb_stat),
  p_value = c(diag_LB_5$p.value, diag_LB_10$p.value, diag_LB_20$p.value,
              diag_LB2_5$p.value, diag_LB2_10$p.value, diag_LB2_20$p.value,
              diag_archLM$p.value, jb_pvalue),
  reject_H0_5pc = c(diag_LB_5$p.value < 0.05, diag_LB_10$p.value < 0.05, diag_LB_20$p.value < 0.05,
                    diag_LB2_5$p.value < 0.05, diag_LB2_10$p.value < 0.05, diag_LB2_20$p.value < 0.05,
                    diag_archLM$p.value < 0.05, jb_pvalue < 0.05),
  row.names = NULL
)
kable(diag_summary, digits = 4,
      caption = "Exercise 4.2(c) - Diagnostic check on the Gaussian GARCH(1,1) for Cisco")


plot_sigma_csco <- ggplot(
  data.frame(Date = csco_data$Dates, sigma = sigma_t_fit),
  aes(x = Date, y = sigma)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  labs(title = "Cisco Conditional Volatility from Gaussian GARCH(1,1)",
       subtitle = "Estimated standard deviation sigma_t",
       x = "Date", y = expression(sigma[t])) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_sigma_csco)


# Summary table (Gaussian vs Student-t, 1-day horizon)

VaR_ES_summary <- data.frame(
  Distribution = c("Gaussian", "Student-t"),
  Confidence = c("99% (p=0.01)", "99% (p=0.01)"),
  VaR_return = c(VaR_n, VaR_t),
  ES_return = c(ES_n, ES_t),
  VaR_USD = c(PL_VaR_n, PL_VaR_t),
  ES_USD = c(PL_ES_n, PL_ES_t),
  row.names = NULL
)
kable(VaR_ES_summary, digits = 6,
      caption = paste("Exercise 4.2 - 1-day VaR and Expected Shortfall",
                      "(tail probability 0.01) on a $1,000,000 long",
                      "position in Cisco stock"))


write.csv(VaR_ES_summary, paths$result("exercise4_2_VaR_ES_summary.csv"), row.names = FALSE)
write.csv(diag_summary, paths$result("exercise4_2_diagnostics.csv"), row.names = FALSE)

ggsave(paths$figure("exercise4_2_timeseries.png"),
       plot = plot_csco_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(paths$figure("exercise4_2_conditional_volatility.png"),
       plot = plot_sigma_csco,
       width = 11, height = 6, units = "in", dpi = 300)
