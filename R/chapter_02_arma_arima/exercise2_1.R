# Exercise 2.1
# MA(1) model R_t = a_t + 0.2 * a_{t-1}, sigma_a = 0.025, a_100 = 0.01.
# Forecasts at horizons 1 and 2 from origin t = 100, their SE, and rho_1, rho_2.

library(ggplot2)
library(kableExtra)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


theta <- 0.2
sigma_a <- 0.025
a_100 <- 0.01


# (i) Forecasts at horizons 1 and 2
# For MA(1) the forecast cuts off after the order of the model
forecast_1step <- theta * a_100
forecast_2step <- 0


# (ii) SE of the forecast errors
se_1step <- sigma_a
se_2step <- sigma_a * sqrt(1 + theta^2)


# (iii) Lag-1 and lag-2 autocorrelations
rho_1 <- theta / (1 + theta^2)
rho_2 <- 0


exercise2_1_summary <- data.frame(
  Quantity = c("Forecast 1-step ahead (R_101 hat)",
               "Forecast 2-step ahead (R_102 hat)",
               "Std. error of 1-step forecast error",
               "Std. error of 2-step forecast error",
               "Lag-1 autocorrelation (rho_1)",
               "Lag-2 autocorrelation (rho_2)"),
  Value = c(round(forecast_1step, 6),
            round(forecast_2step, 6),
            round(se_1step, 6),
            round(se_2step, 6),
            round(rho_1, 6),
            round(rho_2, 6))
)

kable(exercise2_1_summary,
      caption = "Exercise 2.1 - MA(1) with theta = 0.2, sigma_a = 0.025, a_100 = 0.01")


# Theoretical ACF for the visual: spike at lag 1, zero from lag 2 on
lags_to_plot <- 1:10
theoretical_acf <- c(rho_1, rep(0, length(lags_to_plot) - 1))

plot_theoretical_acf <- ggplot(data.frame(lag = lags_to_plot, acf = theoretical_acf),
                               aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = lags_to_plot) +
  labs(title = "Theoretical Autocorrelation Function - MA(1) Process",
       subtitle = "R_t = a_t + 0.2 * a_{t-1}.  ACF cuts off at lag 1.",
       x = "Lag", y = "Autocorrelation") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_theoretical_acf)


write.csv(exercise2_1_summary, paths$result("exercise2_1_summary.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise2_1_theoretical_acf.png"),
       plot = plot_theoretical_acf,
       width = 9, height = 5, units = "in", dpi = 300)
