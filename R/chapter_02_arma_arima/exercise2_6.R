# Exercise 2.6

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


ew_data <- read.xlsx(paths$data("m-ew6299.xlsx"), detectDates = TRUE)
ew_data$Date <- as.Date(ew_data$Date)

dim(ew_data)
head(ew_data)

# The column holds the simple return in percent, so the log return in percent is
# 100 * log(1 + R/100).
simple_return <- ew_data$ew
log_return <- 100 * log(1 + simple_return / 100)
n_obs <- length(log_return)


plot_ew_timeseries <- ggplot(
  data.frame(Date = ew_data$Date, lew = log_return),
  aes(x = Date, y = lew)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "CRSP Equal-Weighted Index - Monthly Log Returns",
       subtitle = "January 1962 - December 1999",
       x = "Date", y = "Log return (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_ew_timeseries)


# Correlograms to read the orders, plus a Ljung-Box to confirm serial correlation

plot_acf_ew <- ggAcf(log_return, lag.max = 24) +
  labs(title = "Sample ACF of EW index monthly log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

plot_pacf_ew <- ggPacf(log_return, lag.max = 24) +
  labs(title = "Sample PACF of EW index monthly log returns") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(plot_acf_ew)
print(plot_pacf_ew)

ljungbox_ew <- Box.test(log_return, lag = 12, type = "Ljung-Box")
print(ljungbox_ew)


# (a) AR model. I let ar() select the order by AIC, then refit with arima().

ar_select <- ar(log_return, order.max = 12)
ar_order <- ar_select$order
cat("AR order selected by AIC:", ar_order, "\n")

ar_model <- arima(log_return, order = c(ar_order, 0, 0))
print(ar_model)

box_ar <- Box.test(residuals(ar_model), lag = 12, type = "Ljung-Box", fitdf = ar_order)
print(box_ar)


# (b) MA model. I compare MA(1), MA(2), MA(3) and keep the best on AIC.

ma1 <- arima(log_return, order = c(0, 0, 1))
ma2 <- arima(log_return, order = c(0, 0, 2))
ma3 <- arima(log_return, order = c(0, 0, 3))
ma_aic <- c(AIC(ma1), AIC(ma2), AIC(ma3))
ma_order <- which.min(ma_aic)
cat("MA order selected by AIC:", ma_order, "\n")

ma_model <- list(ma1, ma2, ma3)[[ma_order]]
print(ma_model)

box_ma <- Box.test(residuals(ma_model), lag = 12, type = "Ljung-Box", fitdf = ma_order)
print(box_ma)


# (c) 1- and 2-step-ahead forecasts for both models

fc_ar <- forecast(ar_model, h = 2, level = 95)
fc_ma <- forecast(ma_model, h = 2, level = 95)

forecast_table <- data.frame(
  Step = c(1, 2),
  AR_forecast = round(as.numeric(fc_ar$mean), 6),
  AR_se = round(as.numeric((fc_ar$upper - fc_ar$mean) / qnorm(0.975)), 6),
  MA_forecast = round(as.numeric(fc_ma$mean), 6),
  MA_se = round(as.numeric((fc_ma$upper - fc_ma$mean) / qnorm(0.975)), 6)
)
kable(forecast_table, digits = 6,
      caption = "Exercise 2.6(c) - 1- and 2-step-ahead forecasts of the AR and MA models")


# (d) Compare the AR and MA fits on log-likelihood, AIC and BIC

model_comparison <- data.frame(
  Model = c(paste0("AR(", ar_order, ")"), paste0("MA(", ma_order, ")")),
  Log_lik = c(ar_model$loglik, ma_model$loglik),
  AIC = c(AIC(ar_model), AIC(ma_model)),
  BIC = c(BIC(ar_model), BIC(ma_model)),
  row.names = NULL
)
kable(model_comparison, digits = 4,
      caption = "Exercise 2.6(d) - AR versus MA model comparison")


write.csv(forecast_table, paths$result("exercise2_6_forecasts.csv"), row.names = FALSE)
write.csv(model_comparison, paths$result("exercise2_6_model_comparison.csv"), row.names = FALSE)

ggsave(paths$figure("exercise2_6_timeseries.png"), plot_ew_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_6_acf.png"), plot_acf_ew,
       width = 9, height = 5, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_6_pacf.png"), plot_pacf_ew,
       width = 9, height = 5, units = "in", dpi = 300)
