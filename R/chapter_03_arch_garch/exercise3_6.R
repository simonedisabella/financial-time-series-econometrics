# Exercise 3.6
# Monthly simple returns of GM and S&P 500 from m-gmsp5008.xlsx (Date, gm, sp).
# All five parts operate on GM log returns. fGarch handles (a) and (c),
# rugarch is needed for GARCH-M in (b) and EGARCH in (d).

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(forecast)
library(fGarch)
library(rugarch)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_03")



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


gmsp_data <- read.xlsx(paths$data("m-gmsp5008.xlsx"), sheet = 1, detectDates = TRUE)
gmsp_data$Date <- as.Date(gmsp_data$Date)

dim(gmsp_data)
head(gmsp_data)
tail(gmsp_data)

# Keep S&P in the data frame for completeness, even if unused here
gmsp_data$lgm <- log(1 + gmsp_data$gm)
gmsp_data$lsp <- log(1 + gmsp_data$sp)
gm_log <- gmsp_data$lgm
n_obs <- length(gm_log)


plot_gm_timeseries <- ggplot(
  data.frame(Date = gmsp_data$Date, lgm = gm_log),
  aes(x = Date, y = lgm)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "General Motors Monthly Log Returns",
       subtitle = "January 1950 - December 2008",
       x = "Date", y = "Log return") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_gm_timeseries)


# Preliminary check on ARCH effects before fitting GARCH
demeaned_gm <- gm_log - mean(gm_log)
lb_sq_12 <- Box.test(demeaned_gm^2, lag = 12, type = "Ljung-Box")
arch_LM_gm <- engle_arch_lm_test(demeaned_gm, lags = 12)
cat("\nPreliminary tests for ARCH effects on GM log returns:\n")
print(lb_sq_12)
cat("Engle ARCH LM (lag 12): Chi^2 =",
    round(arch_LM_gm$statistic, 2),
    "  p-value =", format(arch_LM_gm$p.value, digits = 3, scientific = TRUE), "\n")


# (a) GARCH(1,1) with Gaussian innovations

garch_norm <- garchFit(lgm ~ garch(1, 1), data = gmsp_data,
                       cond.dist = "norm", trace = FALSE)

cat("\n========== (a) GARCH(1,1) Gaussian on GM log returns ==========\n")
summary(garch_norm)

coef_a <- coef(garch_norm)
cat("\nFitted GARCH(1,1) Gaussian:\n")
print(round(coef_a, 6))

persistence_a <- coef_a["alpha1"] + coef_a["beta1"]
uncond_var_a <- coef_a["omega"] / (1 - persistence_a)
cat("Persistence alpha+beta =", round(persistence_a, 6), "\n")
cat("Unconditional variance =", round(uncond_var_a, 6), "\n")


# (b) GARCH-M with Gaussian innovations
# r_t = mu + gamma * sigma_t + a_t, GARCH(1,1) variance

spec_garchm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                    archm = TRUE, archpow = 1),
  distribution.model = "norm"
)
garchm_fit <- ugarchfit(spec_garchm, data = gm_log, solver = "hybrid")

cat("\n========== (b) GARCH-M(1,1) Gaussian on GM log returns ==========\n")
show(garchm_fit)

coef_b <- coef(garchm_fit)
cat("\nFitted GARCH-M(1,1) Gaussian:\n")
print(round(coef_b, 6))

gamma_b <- garchm_fit@fit$matcoef["archm", " Estimate"]
gamma_seB <- garchm_fit@fit$matcoef["archm", " Std. Error"]
gamma_pB <- 2 * (1 - pnorm(abs(gamma_b / gamma_seB)))
cat("\nRisk premium gamma =", round(gamma_b, 4),
    " (SE =", round(gamma_seB, 4),
    ", p-value =", format(gamma_pB, digits = 3), ")\n")


# (c) GARCH(1,1) with Student-t + Wald test on the degrees of freedom

garch_t <- garchFit(lgm ~ garch(1, 1), data = gmsp_data,
                    cond.dist = "std", trace = FALSE)
cat("\n========== (c) GARCH(1,1) Student-t on GM log returns ==========\n")
summary(garch_t)

coef_c <- coef(garch_t)
cat("\nFitted GARCH(1,1) Student-t:\n")
print(round(coef_c, 6))

# Wald test: H0 nu = 6 vs HA nu != 6
nu_hat <- coef_c["shape"]
nu_se <- garch_t@fit$matcoef["shape", " Std. Error"]
nu_t_stat <- (nu_hat - 6) / nu_se
nu_p_value <- 2 * (1 - pnorm(abs(nu_t_stat)))

cat("\nHypothesis test on the Student-t degrees of freedom:\n")
cat("nu_hat =", round(nu_hat, 6), "\n")
cat("SE(nu_hat) =", round(nu_se, 6), "\n")
cat("t statistic =", round(nu_t_stat, 4), "\n")
cat("p-value =", format(nu_p_value, digits = 4, scientific = TRUE), "\n")
cat("Reject H0 at 5%? ", nu_p_value < 0.05, "\n")

dof_test_table <- data.frame(
  Parameter = "nu (Student-t degrees of freedom)",
  Estimate = round(nu_hat, 6),
  Std_Error = round(nu_se, 6),
  H0_value = 6,
  t_stat = round(nu_t_stat, 4),
  p_value = nu_p_value,
  reject_H0_5pc = nu_p_value < 0.05
)
kable(dof_test_table, digits = 4,
      caption = "Exercise 3.6(c) - Wald test on the Student-t degrees of freedom (nu = 6)")


# (d) EGARCH model
# rugarch's EGARCH puts alpha_1 as the sign coefficient (leverage if < 0)
# and gamma_1 as the magnitude coefficient

spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)
egarch_fit <- ugarchfit(spec_egarch, data = gm_log, solver = "hybrid")

cat("\n========== (d) EGARCH(1,1) on GM log returns ==========\n")
show(egarch_fit)

coef_d <- coef(egarch_fit)
cat("\nFitted EGARCH(1,1):\n")
print(round(coef_d, 6))

alpha1_d <- coef_d["alpha1"]
gamma1_d <- coef_d["gamma1"]
cat("\nAsymmetry coefficient alpha_1 =", round(alpha1_d, 6),
    "  (negative -> leverage effect)\n")
cat("Magnitude coefficient gamma_1 =", round(gamma1_d, 6), "\n")


# (e) 1- to 6-step volatility forecasts for all four models

horizon <- 6

fc_a_obj <- predict(garch_norm, n.ahead = horizon)
fc_c_obj <- predict(garch_t, n.ahead = horizon)

fc_b_obj <- ugarchforecast(garchm_fit, n.ahead = horizon)
fc_d_obj <- ugarchforecast(egarch_fit, n.ahead = horizon)

fc_a_sigma <- fc_a_obj$standardDeviation
fc_b_sigma <- as.numeric(sigma(fc_b_obj))
fc_c_sigma <- fc_c_obj$standardDeviation
fc_d_sigma <- as.numeric(sigma(fc_d_obj))

forecast_comparison <- data.frame(
  Step = 1:horizon,
  GARCH_Normal = round(fc_a_sigma, 6),
  GARCH_M_Normal = round(fc_b_sigma, 6),
  GARCH_Student_t = round(fc_c_sigma, 6),
  EGARCH_Normal = round(fc_d_sigma, 6)
)
kable(forecast_comparison, digits = 6,
      caption = "Exercise 3.6(e) - 1- to 6-step-ahead volatility forecasts (compared)")


forecast_long <- data.frame(
  Step = rep(1:horizon, times = 4),
  Model = factor(rep(c("GARCH Normal", "GARCH-M Normal",
                       "GARCH Student-t", "EGARCH Normal"),
                     each = horizon),
                 levels = c("GARCH Normal", "GARCH-M Normal",
                            "GARCH Student-t", "EGARCH Normal")),
  Sigma = c(fc_a_sigma, fc_b_sigma, fc_c_sigma, fc_d_sigma)
)

plot_forecast_comparison <- ggplot(forecast_long,
                                   aes(x = Step, y = Sigma,
                                       color = Model, shape = Model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:horizon) +
  scale_color_manual(values = c("steelblue", "darkorange",
                                "darkgreen", "firebrick")) +
  labs(title = "GM Volatility Forecasts - Comparison Across Four Models",
       subtitle = "1- to 6-step ahead conditional standard deviation",
       x = "Forecast horizon (months ahead)",
       y = expression(sigma[h](l))) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "bottom")
print(plot_forecast_comparison)


write.csv(dof_test_table, paths$result("exercise3_6_dof_test.csv"), row.names = FALSE)
write.csv(forecast_comparison, paths$result("exercise3_6_forecast_comparison.csv"), row.names = FALSE)

ggsave(paths$figure("exercise3_6_timeseries.png"),
       plot = plot_gm_timeseries,
       width = 11, height = 6, units = "in", dpi = 300)

ggsave(paths$figure("exercise3_6_forecast_comparison.png"),
       plot = plot_forecast_comparison,
       width = 11, height = 6, units = "in", dpi = 300)
