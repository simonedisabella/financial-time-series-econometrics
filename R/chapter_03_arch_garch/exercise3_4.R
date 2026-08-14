# Exercise 3.4
# Monthly Merck stock returns from m-mrk4608.xlsx (Date, mrk = simple return).
# I transform to log returns and answer: (a) serial correlation, (b) ARCH effects, (c) ARCH model.

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_03")



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


merck_data <- read.xlsx(paths$data("m-mrk4608.xlsx"), sheet = 1, detectDates = TRUE)
merck_data$Date <- as.Date(merck_data$Date)

dim(merck_data)
head(merck_data)
tail(merck_data)

merck_data$lrtn <- log(1 + merck_data$mrk)
merck_log <- merck_data$lrtn
n_obs <- length(merck_log)


plot_merck_timeseries <- ggplot(
  data.frame(Date = merck_data$Date, lrtn = merck_log),
  aes(x = Date, y = lrtn)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "Merck Monthly Log Returns",
       subtitle = "June 1946 - December 2008",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_merck_timeseries)


# (a) Serial correlation in the log returns

ljungbox_returns_6 <- Box.test(merck_log, lag = 6, type = "Ljung-Box")
ljungbox_returns_12 <- Box.test(merck_log, lag = 12, type = "Ljung-Box")
print(ljungbox_returns_6)
print(ljungbox_returns_12)

plot_acf_returns <- ggAcf(merck_log, lag.max = 20) +
  labs(title = "Sample ACF of Merck log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

plot_pacf_returns <- ggPacf(merck_log, lag.max = 20) +
  labs(title = "Sample PACF of Merck log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(plot_acf_returns)
print(plot_pacf_returns)

# Q(12) rejects, lag 1 is the main spike: AR(1) is the parsimonious mean fit
mean_model_AR1 <- arima(merck_log, order = c(1, 0, 0))
print(mean_model_AR1)

phi_1_hat <- coef(mean_model_AR1)["ar1"]
mu_hat <- coef(mean_model_AR1)["intercept"]
phi_0_hat <- (1 - phi_1_hat) * mu_hat
sigma_a <- sqrt(mean_model_AR1$sigma2)

cat("\nFitted AR(1) mean equation for Merck log returns:\n")
cat("phi_0 =", round(phi_0_hat, 6), "\n")
cat("phi_1 =", round(phi_1_hat, 6), "\n")
cat("sigma_a =", round(sigma_a, 6), "\n\n")

residuals_AR1 <- residuals(mean_model_AR1)
diag_LB_resid <- Box.test(residuals_AR1, lag = 12, type = "Ljung-Box", fitdf = 1)
print(diag_LB_resid)


# (b) ARCH effects on the AR(1) residuals

ljungbox_sq_6 <- Box.test(residuals_AR1^2, lag = 6, type = "Ljung-Box")
ljungbox_sq_12 <- Box.test(residuals_AR1^2, lag = 12, type = "Ljung-Box")
arch_LM_test <- engle_arch_lm_test(residuals_AR1, lags = 12)

print(ljungbox_sq_6)
print(ljungbox_sq_12)

cat("\n\tEngle LM Test for ARCH effects on AR(1) residuals\n")
cat("Chi-squared =", round(arch_LM_test$statistic, 4),
    ", df =", arch_LM_test$parameter,
    ", p-value =", format(arch_LM_test$p.value, digits = 4, scientific = TRUE), "\n\n")

arch_tests_summary <- data.frame(
  Test = c("Ljung-Box on AR(1) squared residuals, lag 6",
           "Ljung-Box on AR(1) squared residuals, lag 12",
           "Engle LM ARCH test, lag 12"),
  Statistic = c(ljungbox_sq_6$statistic,
                ljungbox_sq_12$statistic,
                arch_LM_test$statistic),
  p_value = c(ljungbox_sq_6$p.value,
              ljungbox_sq_12$p.value,
              arch_LM_test$p.value),
  reject_H0_5pc = c(ljungbox_sq_6$p.value < 0.05,
                    ljungbox_sq_12$p.value < 0.05,
                    arch_LM_test$p.value < 0.05),
  row.names = NULL
)
kable(arch_tests_summary, digits = 4,
      caption = "Exercise 3.4(b) - Tests for ARCH effects (5% level)")


# (c) Identify and fit an ARCH model

# PACF cut-off on the squared residuals tells me the ARCH order
plot_pacf_squared <- ggPacf(residuals_AR1^2, lag.max = 15) +
  labs(title = "Sample PACF of squared AR(1) residuals - Merck") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))
print(plot_pacf_squared)

# Largest spike at lag 3, so ARCH(3) is the natural pick. I also fit ARCH(2) for comparison.
arch2_merck <- garchFit(lrtn ~ arma(1,0) + garch(2, 0), data = merck_data,
                        cond.dist = "norm", trace = FALSE)
arch3_merck <- garchFit(lrtn ~ arma(1,0) + garch(3, 0), data = merck_data,
                        cond.dist = "norm", trace = FALSE)

cat("\n========== ARMA(1,0)-ARCH(2) on Merck log returns ==========\n")
summary(arch2_merck)

cat("\n========== ARMA(1,0)-ARCH(3) on Merck log returns ==========\n")
summary(arch3_merck)


ics2 <- arch2_merck@fit$ics
ics3 <- arch3_merck@fit$ics

arch_model_comparison <- data.frame(
  Model = c("ARMA(1,0)-ARCH(2)", "ARMA(1,0)-ARCH(3)"),
  LogLik = c(-arch2_merck@fit$llh, -arch3_merck@fit$llh),
  AIC = c(ics2["AIC"], ics3["AIC"]),
  BIC = c(ics2["BIC"], ics3["BIC"]),
  row.names = NULL
)
kable(arch_model_comparison, digits = 4,
      caption = "Exercise 3.4(c) - ARCH order selection on Merck")

# ARCH(3) wins on both AIC and BIC
best_arch_merck <- arch3_merck

coefficients_final <- coef(best_arch_merck)
cat("\nFinal fitted model (Merck monthly log returns):\n\n")
print(round(coefficients_final, 6))

alphas <- coefficients_final[c("alpha1", "alpha2", "alpha3")]
cat("\nSum of alpha coefficients =", round(sum(alphas), 4), "\n")
cat("Implied unconditional variance =",
    round(coefficients_final["omega"] / (1 - sum(alphas)), 6), "\n")


write.csv(arch_tests_summary, paths$result("exercise3_4_arch_tests.csv"), row.names = FALSE)
write.csv(arch_model_comparison, paths$result("exercise3_4_model_comparison.csv"), row.names = FALSE)

ggsave(paths$figure("exercise3_4_timeseries.png"),
       plot = plot_merck_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(paths$figure("exercise3_4_acf_returns.png"),
       plot = plot_acf_returns,
       width = 9, height = 5, units = "in", dpi = 300)

ggsave(paths$figure("exercise3_4_pacf_returns.png"),
       plot = plot_pacf_returns,
       width = 9, height = 5, units = "in", dpi = 300)

ggsave(paths$figure("exercise3_4_pacf_squared_residuals.png"),
       plot = plot_pacf_squared,
       width = 9, height = 5, units = "in", dpi = 300)
