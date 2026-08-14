# Exercise 3.2
# Multistep-ahead forecasts for a GARCH(2,1) model at origin h.
# Theoretical exercise: I write the recursion as an R function and check it
# on synthetic parameters resembling monthly equity data.

library(ggplot2)
library(kableExtra)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_03")


# Recursion in compact form (full derivation goes in the LaTeX report):
#   l = 1: sigma^2_h(1) = alpha_0 + alpha_1 a^2_h + alpha_2 a^2_{h-1} + beta_1 sigma^2_h
#   l = 2: sigma^2_h(2) = alpha_0 + (alpha_1 + beta_1) sigma^2_h(1) + alpha_2 a^2_h
#   l >= 3: sigma^2_h(l) = alpha_0 + (alpha_1 + beta_1) sigma^2_h(l-1) + alpha_2 sigma^2_h(l-2)

garch21_forecast <- function(alpha_0, alpha_1, alpha_2, beta_1,
                             a2_h, a2_hm1, sigma2_h,
                             horizon = 10) {
  
  if ((alpha_1 + alpha_2 + beta_1) >= 1) {
    warning("alpha_1 + alpha_2 + beta_1 >= 1 - the GARCH(2,1) is not stationary")
  }
  
  sigma2_forecast <- numeric(horizon)
  
  sigma2_forecast[1] <- alpha_0 +
    alpha_1 * a2_h +
    alpha_2 * a2_hm1 +
    beta_1 * sigma2_h
  
  if (horizon >= 2) {
    sigma2_forecast[2] <- alpha_0 +
      (alpha_1 + beta_1) * sigma2_forecast[1] +
      alpha_2 * a2_h
  }
  
  if (horizon >= 3) {
    for (l in 3:horizon) {
      sigma2_forecast[l] <- alpha_0 +
        (alpha_1 + beta_1) * sigma2_forecast[l - 1] +
        alpha_2 * sigma2_forecast[l - 2]
    }
  }
  
  return(sigma2_forecast)
}


# Same total persistence as 3.1 (0.95) so the two exercises are comparable
alpha_0 <- 0.0010
alpha_1 <- 0.06
alpha_2 <- 0.04
beta_1 <- 0.85

a2_h <- 0.0030
a2_hm1 <- 0.0020
sigma2_h <- 0.0050

sigma2_unconditional <- alpha_0 / (1 - alpha_1 - alpha_2 - beta_1)

forecast_horizon <- 30
variance_forecast <- garch21_forecast(alpha_0, alpha_1, alpha_2, beta_1,
                                      a2_h, a2_hm1, sigma2_h,
                                      horizon = forecast_horizon)

forecast_table <- data.frame(
  Step = 1:10,
  Variance = round(variance_forecast[1:10], 8),
  Volatility = round(sqrt(variance_forecast[1:10]), 6)
)
kable(forecast_table,
      caption = paste("Exercise 3.2 - Multi-step GARCH(2,1) volatility forecasts",
                      "(synthetic parameters)"))


plot_forecast_path <- ggplot(
  data.frame(step = 1:forecast_horizon, variance = variance_forecast),
  aes(x = step, y = variance)) +
  geom_hline(yintercept = sigma2_unconditional,
             color = "red", linewidth = 0.7, linetype = "dashed") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text", x = forecast_horizon, y = sigma2_unconditional,
           label = "Unconditional variance",
           hjust = 1, vjust = -0.7, color = "red", size = 4) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  labs(title = "Multi-step Volatility Forecast - GARCH(2,1) Process",
       subtitle = paste("Synthetic parameters: alpha_0 = 0.001, alpha_1 = 0.06,",
                        "alpha_2 = 0.04, beta_1 = 0.85"),
       x = "Forecast horizon l (steps ahead)",
       y = expression(sigma[h]^2*(l))) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_forecast_path)


write.csv(forecast_table, paths$result("exercise3_2_forecasts.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise3_2_forecast_path.png"),
       plot = plot_forecast_path,
       width = 10, height = 6, units = "in", dpi = 300)
