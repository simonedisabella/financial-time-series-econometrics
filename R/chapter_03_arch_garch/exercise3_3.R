# Exercise 3.3
# Monthly Intel stock returns from m-intc7308.xlsx (columns Date, rtn, lrtn).
# Note: the assignment text says "Jan 1973 - Dec 2008" but the file actually ends
# in December 2003, so I take December 2003 as the forecast origin.
# Task: build a GARCH model on lrtn and produce 1- to 5-step ahead volatility forecasts.

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_03")



# Engle's ARCH-LM test written by hand so I don't have to install FinTS just
# for one function. Same output as FinTS::ArchTest().
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


intel_data <- read.xlsx(paths$data("m-intc7308.xlsx"), sheet = 1, detectDates = TRUE)
intel_data$Date <- as.Date(intel_data$Date)

dim(intel_data)
head(intel_data)
tail(intel_data)

intel_log_returns <- intel_data$lrtn
number_of_obs <- length(intel_log_returns)


plot_intel_timeseries <- ggplot(
  data.frame(Date = intel_data$Date, lrtn = intel_log_returns),
  aes(x = Date, y = lrtn)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "Intel Monthly Log Returns",
       subtitle = "January 1973 - December 2003",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_intel_timeseries)


# Preliminary tests: serial correlation in the mean and ARCH effects in the variance

ljungbox_returns <- Box.test(intel_log_returns, lag = 12, type = "Ljung-Box")
print(ljungbox_returns)

demeaned_returns <- intel_log_returns - mean(intel_log_returns)

ljungbox_sq6 <- Box.test(demeaned_returns^2, lag = 6, type = "Ljung-Box")
ljungbox_sq12 <- Box.test(demeaned_returns^2, lag = 12, type = "Ljung-Box")
print(ljungbox_sq6)
print(ljungbox_sq12)

arch_test_intel <- engle_arch_lm_test(demeaned_returns, lags = 12)
cat("\n\tEngle LM Test for ARCH effects\n")
cat("Chi-squared =", round(arch_test_intel$statistic, 4),
    ", df =", arch_test_intel$parameter,
    ", p-value =", format(arch_test_intel$p.value, digits = 4, scientific = TRUE), "\n\n")

preliminary_summary <- data.frame(
  Test = c("Ljung-Box on log returns (lag 12)",
           "Ljung-Box on squared demeaned returns (lag 6)",
           "Ljung-Box on squared demeaned returns (lag 12)",
           "Engle ARCH-LM test (lag 12)"),
  Statistic = c(ljungbox_returns$statistic,
                ljungbox_sq6$statistic,
                ljungbox_sq12$statistic,
                arch_test_intel$statistic),
  p_value = c(ljungbox_returns$p.value,
              ljungbox_sq6$p.value,
              ljungbox_sq12$p.value,
              arch_test_intel$p.value),
  reject_H0 = c(ljungbox_returns$p.value < 0.05,
                ljungbox_sq6$p.value < 0.05,
                ljungbox_sq12$p.value < 0.05,
                arch_test_intel$p.value < 0.05),
  row.names = NULL
)
kable(preliminary_summary, digits = 4,
      caption = "Exercise 3.3 - Preliminary tests on Intel log returns")


# Candidate GARCH(1,1) specs with three innovation distributions

garch_normal <- garchFit(lrtn ~ garch(1, 1), data = intel_data,
                         cond.dist = "norm", trace = FALSE)
garch_std <- garchFit(lrtn ~ garch(1, 1), data = intel_data,
                      cond.dist = "std", trace = FALSE)
garch_sstd <- garchFit(lrtn ~ garch(1, 1), data = intel_data,
                       cond.dist = "sstd", trace = FALSE)

cat("\n========== GARCH(1,1) Gaussian ==========\n")
summary(garch_normal)

cat("\n========== GARCH(1,1) Student-t ==========\n")
summary(garch_std)

cat("\n========== GARCH(1,1) skewed Student-t ==========\n")
summary(garch_sstd)


# fGarch normalises AIC/BIC by n (slot @fit$ics) and stores the NEGATIVE log-lik
ics_normal <- garch_normal@fit$ics
ics_std <- garch_std@fit$ics
ics_sstd <- garch_sstd@fit$ics

model_comparison <- data.frame(
  Model = c("GARCH(1,1) Normal",
            "GARCH(1,1) Student-t",
            "GARCH(1,1) Skewed Student-t"),
  LogLik = c(-garch_normal@fit$llh,
             -garch_std@fit$llh,
             -garch_sstd@fit$llh),
  AIC = c(ics_normal["AIC"], ics_std["AIC"], ics_sstd["AIC"]),
  BIC = c(ics_normal["BIC"], ics_std["BIC"], ics_sstd["BIC"]),
  row.names = NULL
)
kable(model_comparison, digits = 4,
      caption = "Exercise 3.3 - Information criteria for candidate GARCH(1,1) models")

# Skewed-t wins on AIC for Intel (heavy tails + mild left skew)
best_model <- garch_sstd
best_model_name <- "GARCH(1,1) with skewed Student-t innovations"


coefficients_best <- coef(best_model)
cat("\nFitted", best_model_name, "for Intel monthly log returns:\n\n")
print(round(coefficients_best, 6))

alpha1_hat <- coefficients_best["alpha1"]
beta1_hat <- coefficients_best["beta1"]
omega_hat <- coefficients_best["omega"]
unconditional_variance <- omega_hat / (1 - alpha1_hat - beta1_hat)
cat("\nPersistence alpha_1 + beta_1 =", round(alpha1_hat + beta1_hat, 6), "\n")
cat("Implied unconditional std. dev =", round(sqrt(unconditional_variance), 6), "\n")


# 1- to 5-step-ahead volatility forecasts at origin h = Dec 2003

forecast_horizon <- 5
forecast_output <- predict(best_model, n.ahead = forecast_horizon)
print(forecast_output)

forecast_table <- data.frame(
  Step = 1:forecast_horizon,
  Mean_forecast = round(forecast_output$meanForecast, 6),
  Volatility = round(forecast_output$standardDeviation, 6),
  Variance = round(forecast_output$standardDeviation^2, 8)
)
kable(forecast_table,
      caption = paste("Exercise 3.3 - 1- to 5-step-ahead Intel volatility",
                      "forecasts (origin: December 2003)"))


# Conditional volatility plot + forecast horizon

in_sample_sigma <- best_model@sigma.t

last_date <- max(intel_data$Date)
future_dates <- seq(from = last_date, by = "month",
                    length.out = forecast_horizon + 1)[-1]

historical_segment <- data.frame(Date = intel_data$Date,
                                 sigma = in_sample_sigma)
forecast_segment <- data.frame(Date = c(last_date, future_dates),
                               sigma = c(in_sample_sigma[number_of_obs],
                                         forecast_output$standardDeviation))

plot_volatility_forecast <- ggplot() +
  geom_line(data = historical_segment, aes(x = Date, y = sigma),
            color = "steelblue", linewidth = 0.5) +
  geom_line(data = forecast_segment, aes(x = Date, y = sigma),
            color = "red", linewidth = 0.9) +
  geom_point(data = forecast_segment[-1, ], aes(x = Date, y = sigma),
             color = "red", shape = 1, size = 2.5) +
  geom_hline(yintercept = sqrt(unconditional_variance),
             color = "darkgreen", linewidth = 0.5, linetype = "dashed") +
  annotate("text",
           x = min(intel_data$Date), y = sqrt(unconditional_variance),
           label = "Unconditional std. dev.",
           hjust = 0, vjust = -0.7, color = "darkgreen", size = 4) +
  labs(title = "Intel Conditional Volatility - Fitted and 5-step Forecast",
       subtitle = paste("Estimated by", best_model_name,
                        "- forecast origin: December 2003"),
       x = "Date", y = expression(sigma[t])) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_volatility_forecast)


write.csv(preliminary_summary, paths$result("exercise3_3_preliminary_tests.csv"), row.names = FALSE)
write.csv(model_comparison, paths$result("exercise3_3_model_comparison.csv"), row.names = FALSE)
write.csv(forecast_table, paths$result("exercise3_3_forecasts.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise3_3_timeseries.png"),
       plot = plot_intel_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(filename = paths$figure("exercise3_3_volatility_forecast.png"),
       plot = plot_volatility_forecast,
       width = 11, height = 6, units = "in", dpi = 300)
