# Exercise 2.3
# Monthly simple returns of Deciles 1, 2, 9, 10 (NYSE/AMEX/NASDAQ) from Jan 1970 to Dec 2008.
# Data file: m-dec12910.xlsx. Decile 2 = large caps, Decile 10 = small caps.

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


decile_data <- read.xlsx(paths$data("m-dec12910.xlsx"), detectDates = TRUE)
decile_data$Date <- as.Date(decile_data$Date)

dim(decile_data)
head(decile_data)
tail(decile_data)

# The file actually starts in 1967, so I cut to the exercise window
decile_data <- decile_data[decile_data$Date >= as.Date("1970-01-01") &
                             decile_data$Date <= as.Date("2008-12-31"), ]
dim(decile_data)

returns_dec2 <- decile_data$dec2
returns_dec10 <- decile_data$dec10

number_of_observations <- nrow(decile_data)


# Time series plot

returns_long <- data.frame(
  Date = rep(decile_data$Date, times = 2),
  Decile = factor(rep(c("Decile 2 (large caps)", "Decile 10 (small caps)"),
                      each = number_of_observations),
                  levels = c("Decile 2 (large caps)", "Decile 10 (small caps)")),
  Returns = c(returns_dec2, returns_dec10)
)

plot_decile_timeseries <- ggplot(returns_long, aes(x = Date, y = Returns)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  facet_wrap(~ Decile, ncol = 1, scales = "free_y") +
  labs(title = "Monthly Simple Returns - Decile 2 and Decile 10",
       subtitle = "NYSE/AMEX/NASDAQ, January 1970 - December 2008",
       x = "Date", y = "Simple return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1, "lines"))
print(plot_decile_timeseries)


# (a) Ljung-Box on the first 12 autocorrelations

ljungbox_dec2 <- Box.test(returns_dec2, lag = 12, type = "Ljung-Box")
ljungbox_dec10 <- Box.test(returns_dec10, lag = 12, type = "Ljung-Box")

print(ljungbox_dec2)
print(ljungbox_dec10)

ljungbox_summary <- data.frame(
  Series = c("Decile 2", "Decile 10"),
  Q_statistic = c(ljungbox_dec2$statistic, ljungbox_dec10$statistic),
  df = c(ljungbox_dec2$parameter, ljungbox_dec10$parameter),
  p_value = c(ljungbox_dec2$p.value, ljungbox_dec10$p.value),
  reject_H0 = c(ljungbox_dec2$p.value < 0.05, ljungbox_dec10$p.value < 0.05),
  row.names = NULL
)

kable(ljungbox_summary, digits = 4,
      caption = paste("Exercise 2.3(a) - Ljung-Box test on the first 12 lags",
                      "(H0: rho_1 = ... = rho_12 = 0)"))


# (b) ARMA model for Decile 2

plot_acf_dec2 <- ggAcf(returns_dec2, lag.max = 20) +
  labs(title = "Sample ACF of Decile 2 monthly returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

plot_pacf_dec2 <- ggPacf(returns_dec2, lag.max = 20) +
  labs(title = "Sample PACF of Decile 2 monthly returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(plot_acf_dec2)
print(plot_pacf_dec2)

# PACF cuts off at lag 1, ACF same picture. Candidates: AR(1), MA(1), ARMA(1,1)
ar1_dec2 <- arima(returns_dec2, order = c(1, 0, 0))
ma1_dec2 <- arima(returns_dec2, order = c(0, 0, 1))
arma11_dec2 <- arima(returns_dec2, order = c(1, 0, 1))

model_comparison <- data.frame(
  Model = c("AR(1)", "MA(1)", "ARMA(1,1)"),
  Log_lik = c(ar1_dec2$loglik, ma1_dec2$loglik, arma11_dec2$loglik),
  AIC = c(AIC(ar1_dec2), AIC(ma1_dec2), AIC(arma11_dec2)),
  BIC = c(BIC(ar1_dec2), BIC(ma1_dec2), BIC(arma11_dec2))
)
kable(model_comparison, digits = 4,
      caption = "Exercise 2.3(b) - Candidate ARMA models for Decile 2 returns")

# MA(1) has the smallest AIC/BIC by a narrow margin. I retain AR(1) deliberately
# as a PACF-driven textbook specification; it is not the information-criterion winner.

ar1_dec2

# arima() reports the series mean as "intercept", so I rebuild phi_0 from mu and phi_1
phi_1_hat <- coef(ar1_dec2)["ar1"]
mu_hat <- coef(ar1_dec2)["intercept"]
phi_0_hat <- (1 - phi_1_hat) * mu_hat
sigma_a <- sqrt(ar1_dec2$sigma2)

cat("\nFitted AR(1) for Decile 2:\n")
cat("phi_0 =", round(phi_0_hat, 6), "\n")
cat("phi_1 =", round(phi_1_hat, 6), "\n")
cat("mu =", round(mu_hat, 6), "\n")
cat("sigma_a =", round(sigma_a, 6), "\n\n")


# Residual diagnostics (fitdf = 1 for AR(1))
residuals_ar1 <- residuals(ar1_dec2)

box_resid <- Box.test(residuals_ar1, lag = 12, type = "Ljung-Box", fitdf = 1)
print(box_resid)

plot_residual_diagnostics <- checkresiduals(ar1_dec2, plot = TRUE)


# (c) 1- to 12-step-ahead forecasts

horizon <- 12
forecast_object_dec2 <- forecast(ar1_dec2, h = horizon, level = 95)

# Recover SEs from the 95% prediction interval
forecast_means <- as.numeric(forecast_object_dec2$mean)
forecast_se <- as.numeric((forecast_object_dec2$upper -
                             forecast_object_dec2$mean) / qnorm(0.975))

forecast_table_dec2 <- data.frame(
  Step = 1:horizon,
  Forecast = round(forecast_means, 6),
  Standard_Error = round(forecast_se, 6),
  Lower_95 = round(as.numeric(forecast_object_dec2$lower), 6),
  Upper_95 = round(as.numeric(forecast_object_dec2$upper), 6)
)

kable(forecast_table_dec2, digits = 6,
      caption = paste("Exercise 2.3(c) - 1- to 12-step-ahead forecasts of",
                      "Decile 2 monthly returns from the fitted AR(1)"))


# Forecast plot: last 4 years of data + forecasts with 95% band

months_to_show <- 4 * 12
last_observed_index <- nrow(decile_data)
plot_start_index <- max(1, last_observed_index - months_to_show)
last_observed_date <- decile_data$Date[last_observed_index]

future_dates <- seq(from = last_observed_date,
                    by = "month",
                    length.out = horizon + 1)[-1]

historical_segment <- data.frame(
  Date = decile_data$Date[plot_start_index:last_observed_index],
  Returns = returns_dec2[plot_start_index:last_observed_index])

forecast_segment <- data.frame(
  Date = c(last_observed_date, future_dates),
  Forecast = c(returns_dec2[last_observed_index], forecast_means),
  Lower = c(returns_dec2[last_observed_index],
            as.numeric(forecast_object_dec2$lower)),
  Upper = c(returns_dec2[last_observed_index],
            as.numeric(forecast_object_dec2$upper))
)

plot_forecast_dec2 <- ggplot() +
  geom_ribbon(data = forecast_segment,
              aes(x = Date, ymin = Lower, ymax = Upper),
              fill = "grey75", alpha = 0.5) +
  geom_line(data = historical_segment,
            aes(x = Date, y = Returns),
            color = "black", linewidth = 0.6) +
  geom_line(data = forecast_segment,
            aes(x = Date, y = Forecast),
            color = "red", linewidth = 0.8) +
  geom_point(data = forecast_segment[-1, ],
             aes(x = Date, y = Forecast),
             color = "red", shape = 1, size = 2.5) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  labs(title = "AR(1) Forecast of Decile 2 Monthly Returns",
       subtitle = "Last 4 years of observed data, 12 monthly forecasts and 95% prediction interval",
       x = "Date", y = "Simple return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_forecast_dec2)


write.csv(ljungbox_summary, paths$result("exercise2_3_ljungbox.csv"), row.names = FALSE)
write.csv(model_comparison, paths$result("exercise2_3_model_comparison.csv"), row.names = FALSE)
write.csv(forecast_table_dec2, paths$result("exercise2_3_forecasts.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise2_3_timeseries.png"),
       plot = plot_decile_timeseries,
       width = 11, height = 7, units = "in", dpi = 300)

ggsave(filename = paths$figure("exercise2_3_acf_dec2.png"),
       plot = plot_acf_dec2,
       width = 9, height = 5, units = "in", dpi = 300)

ggsave(filename = paths$figure("exercise2_3_pacf_dec2.png"),
       plot = plot_pacf_dec2,
       width = 9, height = 5, units = "in", dpi = 300)

ggsave(filename = paths$figure("exercise2_3_forecast.png"),
       plot = plot_forecast_dec2,
       width = 11, height = 6, units = "in", dpi = 300)
