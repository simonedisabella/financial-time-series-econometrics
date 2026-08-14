# Exercise 1.4
# Daily log returns of AXP from Jan 1999 to Dec 2008 - same series as 1.1.
# Two tests at 5%: H0 skewness = 0, H0 excess kurtosis = 0.

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_01")


three_stocks_data <- read.xlsx(paths$data("d-3stocks9908.xlsx"), detectDates = TRUE)
three_stocks_data$Date <- as.Date(three_stocks_data$Date)

dim(three_stocks_data)
head(three_stocks_data)

simple_returns_axp <- three_stocks_data[, "axp"]
log_returns_axp <- log(1 + simple_returns_axp)
log_percent_axp <- log_returns_axp * 100

n_obs <- length(log_percent_axp)

sample_skewness <- skewness(log_percent_axp)
sample_excess_kurtosis <- kurtosis(log_percent_axp)


# (a) Skewness test
# I use abs(t) in the two-sided p-value so the formula works for negative skewness too
t_stat_skewness <- sample_skewness / sqrt(6 / n_obs)
p_value_skewness <- 2 * (1 - pnorm(abs(t_stat_skewness)))


# (b) Excess kurtosis test
t_stat_kurtosis <- sample_excess_kurtosis / sqrt(24 / n_obs)
p_value_kurtosis <- 2 * (1 - pnorm(abs(t_stat_kurtosis)))


# Jarque-Bera as joint check
jb_test_axp <- normalTest(log_percent_axp, method = "jb")
print(jb_test_axp)


# Summary table
tests_summary <- data.frame(
  Test = c("Skewness = 0", "Excess kurtosis = 0"),
  Statistic = c(round(sample_skewness, 4), round(sample_excess_kurtosis, 4)),
  Std_Error = c(round(sqrt(6 / n_obs), 6), round(sqrt(24 / n_obs), 6)),
  t_stat = c(round(t_stat_skewness, 4), round(t_stat_kurtosis, 4)),
  p_value = c(p_value_skewness, p_value_kurtosis),
  reject_H0_5pc = c(p_value_skewness < 0.05, p_value_kurtosis < 0.05),
  row.names = NULL
)

kable(tests_summary, digits = 4,
      caption = "Exercise 1.4 - Skewness and excess kurtosis tests on AXP log returns (5% level)")


# QQ-plot for visual support
plot_qq_axp <- ggplot(data.frame(log_percent_axp = log_percent_axp),
                      aes(sample = log_percent_axp)) +
  stat_qq(color = "steelblue", alpha = 0.6, size = 0.9) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Normal QQ-Plot of AXP Daily Log Returns",
       subtitle = "January 1999 - December 2008 (red line = theoretical normal quantiles)",
       x = "Theoretical quantiles (standard normal)",
       y = "Sample quantiles (log returns %)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_qq_axp)


write.csv(tests_summary, paths$result("exercise1_4_tests.csv"), row.names = FALSE)

ggsave(filename = paths$figure("exercise1_4_axp_qqplot.png"),
       plot = plot_qq_axp,
       width = 9, height = 7, units = "in", dpi = 300)
