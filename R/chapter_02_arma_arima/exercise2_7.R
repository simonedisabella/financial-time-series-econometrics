# Exercise 2.7

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(tseries)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


gdp_data <- read.xlsx(paths$data("q-gdpdef.xlsx"), detectDates = TRUE)
gdp_data$Date <- as.Date(gdp_data$Date)

dim(gdp_data)
head(gdp_data)
tail(gdp_data)

deflator <- gdp_data$gdpdef
log_deflator <- log(deflator)
n_obs <- length(log_deflator)

# Quarterly inflation = 100 * first difference of the log deflator
inflation <- 100 * diff(log_deflator)


plot_deflator <- ggplot(
  data.frame(Date = gdp_data$Date, deflator = deflator),
  aes(x = Date, y = deflator)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  labs(title = "US GDP Implicit Price Deflator",
       subtitle = "Quarterly, Q1 1947 - Q4 2008 (2000 = 100)",
       x = "Date", y = "Deflator") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_deflator)

plot_inflation <- ggplot(
  data.frame(Date = gdp_data$Date[-1], inflation = inflation),
  aes(x = Date, y = inflation)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  labs(title = "Quarterly Inflation from the GDP Deflator",
       subtitle = "100 x first difference of the log deflator",
       x = "Date", y = "Inflation (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_inflation)


# Order of integration: ADF (H0 unit root) and KPSS (H0 stationary) on the log
# level and on its first difference.

adf_level <- adf.test(log_deflator)
kpss_level <- kpss.test(log_deflator)
adf_diff1 <- adf.test(diff(log_deflator))
kpss_diff1 <- kpss.test(diff(log_deflator))

print(adf_level)
print(kpss_level)
print(adf_diff1)
print(kpss_diff1)

cat("Differences suggested by ndiffs:", ndiffs(log_deflator), "\n")


# Correlograms of the differenced series to read the ARMA orders

plot_acf_diff <- ggAcf(diff(log_deflator), lag.max = 24) +
  labs(title = "Sample ACF of the differenced log deflator") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

plot_pacf_diff <- ggPacf(diff(log_deflator), lag.max = 24) +
  labs(title = "Sample PACF of the differenced log deflator") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(plot_acf_diff)
print(plot_pacf_diff)


# Fit the ARIMA on the log deflator. I let auto.arima search the orders without
# the stepwise shortcut, then read the selected model.

arima_model <- auto.arima(log_deflator, seasonal = FALSE,
                          stepwise = FALSE, approximation = FALSE)
print(summary(arima_model))
cat("Selected ARIMA order (p,d,q):", paste(arimaorder(arima_model)[1:3], collapse = ","), "\n")

box_resid <- Box.test(residuals(arima_model), lag = 12, type = "Ljung-Box")
print(box_resid)
checkresiduals(arima_model)


# Forecast the log deflator for the four quarters of 2009 and translate it into
# the predicted quarterly inflation

horizon <- 4
fc <- forecast(arima_model, h = horizon, level = 95)

fc_log_level <- as.numeric(fc$mean)
predicted_level <- exp(fc_log_level)

log_path <- c(log_deflator[n_obs], fc_log_level)
predicted_inflation <- 100 * diff(log_path)

forecast_table <- data.frame(
  Quarter = c("2009 Q1", "2009 Q2", "2009 Q3", "2009 Q4"),
  Predicted_log_deflator = round(fc_log_level, 6),
  Predicted_deflator = round(predicted_level, 4),
  Predicted_inflation_pct = round(predicted_inflation, 4)
)
kable(forecast_table, digits = 4,
      caption = "Exercise 2.7 - ARIMA forecasts and implied quarterly inflation for 2009")


write.csv(forecast_table, paths$result("exercise2_7_forecasts.csv"), row.names = FALSE)

ggsave(paths$figure("exercise2_7_deflator.png"), plot_deflator,
       width = 11, height = 6, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_7_inflation.png"), plot_inflation,
       width = 11, height = 6, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_7_acf_diff.png"), plot_acf_diff,
       width = 9, height = 5, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_7_pacf_diff.png"), plot_pacf_diff,
       width = 9, height = 5, units = "in", dpi = 300)
