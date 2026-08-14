# Exercise 2.2
# AR(2) with phi_1 = 0: r_t = 0.01 + 0.2 * r_{t-2} + a_t, Var(a_t) = 0.02.
# Mean, variance, rho_1, rho_2, and forecasts at horizons 1-2 with r_100 = -0.01, r_99 = 0.02.

library(ggplot2)
library(kableExtra)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


phi_0 <- 0.01
phi_1 <- 0
phi_2 <- 0.2

sigma_a_squared <- 0.02
sigma_a <- sqrt(sigma_a_squared)

r_99 <- 0.02
r_100 <- -0.01


# (i) Mean of r_t
mean_r <- phi_0 / (1 - phi_1 - phi_2)


# (ii) Yule-Walker autocorrelations
rho_1 <- phi_1 / (1 - phi_2)
rho_2 <- phi_1 * rho_1 + phi_2

# Variance via gamma_0 = sigma_a^2 / (1 - phi_1 rho_1 - phi_2 rho_2)
variance_r <- sigma_a_squared / (1 - phi_1 * rho_1 - phi_2 * rho_2)


# (iii) Recursive forecasts at t = 100
# With phi_1 = 0 the 2-step forecast uses r_100 directly, not the 1-step forecast
forecast_1step <- phi_0 + phi_1 * r_100 + phi_2 * r_99
forecast_2step <- phi_0 + phi_1 * forecast_1step + phi_2 * r_100


# (iv) SE of forecast errors
# 1-step error is a_{101}; 2-step error is a_{102} + phi_1 a_{101} = a_{102} when phi_1 = 0
se_1step <- sigma_a
se_2step <- sigma_a * sqrt(1 + phi_1^2)


exercise2_2_summary <- data.frame(
  Quantity = c("Mean of r_t",
               "Variance of r_t",
               "Lag-1 autocorrelation rho_1",
               "Lag-2 autocorrelation rho_2",
               "Forecast 1-step ahead (r_101 hat)",
               "Forecast 2-step ahead (r_102 hat)",
               "Std. error of 1-step forecast error",
               "Std. error of 2-step forecast error"),
  Value = c(round(mean_r, 6),
            round(variance_r, 6),
            round(rho_1, 6),
            round(rho_2, 6),
            round(forecast_1step, 6),
            round(forecast_2step, 6),
            round(se_1step, 6),
            round(se_2step, 6))
)

kable(exercise2_2_summary,
      caption = paste("Exercise 2.2 - AR(2) with phi_0 = 0.01, phi_1 = 0,",
                      "phi_2 = 0.2, sigma_a^2 = 0.02"))


# Theoretical ACF for the visual
theoretical_acf <- ARMAacf(ar = c(phi_1, phi_2), lag.max = 10)
theoretical_acf <- theoretical_acf[-1]

plot_theoretical_acf <- ggplot(data.frame(lag = 1:10, acf = theoretical_acf),
                               aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Theoretical Autocorrelation Function - AR(2) Process",
       subtitle = "r_t = 0.01 + 0.2 * r_{t-2} + a_t.  rho_1 = 0, rho_2 = 0.2.",
       x = "Lag", y = "Autocorrelation") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_theoretical_acf)


write.csv(exercise2_2_summary, paths$result("exercise2_2_summary.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise2_2_theoretical_acf.png"),
       plot = plot_theoretical_acf,
       width = 9, height = 5, units = "in", dpi = 300)
