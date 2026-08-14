# Exercise 1.1
# Daily simple returns of AXP, CAT, SBUX from Jan 1999 to Dec 2008.

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_01")


# Load the data
three_stocks_data <- read.xlsx(paths$data("d-3stocks9908.xlsx"), detectDates = TRUE)
three_stocks_data$Date <- as.Date(three_stocks_data$Date)

dim(three_stocks_data)
head(three_stocks_data)

simple_returns_axp <- three_stocks_data$axp
simple_returns_cat <- three_stocks_data$cat
simple_returns_sbux <- three_stocks_data$sbux


# (a) Summary statistics on simple returns in percent

simple_percent_axp <- simple_returns_axp * 100
simple_percent_cat <- simple_returns_cat * 100
simple_percent_sbux <- simple_returns_sbux * 100

# The row labelled "Kurtosis" in basicStats is already excess kurtosis (K - 3)
rows_requested <- c("Mean", "Stdev", "Skewness", "Kurtosis", "Minimum", "Maximum")

stats_simple <- cbind(basicStats(simple_percent_axp),
                      basicStats(simple_percent_cat),
                      basicStats(simple_percent_sbux))
colnames(stats_simple) <- c("AXP", "CAT", "SBUX")
stats_simple_summary <- round(stats_simple[rows_requested, ], 3)
rownames(stats_simple_summary) <- c("Mean", "Std. Dev.", "Skewness",
                                    "Excess Kurtosis", "Minimum", "Maximum")
kable(stats_simple_summary,
      caption = "Daily simple returns in percent")


# (b) Transform simple returns to log returns

log_returns_axp <- log(1 + simple_returns_axp)
log_returns_cat <- log(1 + simple_returns_cat)
log_returns_sbux <- log(1 + simple_returns_sbux)


# (c) Summary statistics on log returns in percent

log_percent_axp <- log_returns_axp * 100
log_percent_cat <- log_returns_cat * 100
log_percent_sbux <- log_returns_sbux * 100

stats_log <- cbind(basicStats(log_percent_axp),
                   basicStats(log_percent_cat),
                   basicStats(log_percent_sbux))
colnames(stats_log) <- c("AXP", "CAT", "SBUX")
stats_log_summary <- round(stats_log[rows_requested, ], 3)
rownames(stats_log_summary) <- c("Mean", "Std. Dev.", "Skewness",
                                 "Excess Kurtosis", "Minimum", "Maximum")
kable(stats_log_summary,
      caption = "Daily log returns in percent")


# Plots: I reshape to long format to facet by stock

log_returns_long <- data.frame(
  Date = rep(three_stocks_data$Date, times = 3),
  Stock = factor(rep(c("AXP", "CAT", "SBUX"), each = nrow(three_stocks_data)),
                 levels = c("AXP", "CAT", "SBUX")),
  log_percent = c(log_percent_axp, log_percent_cat, log_percent_sbux)
)

plot_log_timeseries <- ggplot(log_returns_long, aes(x = Date, y = log_percent)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.35) +
  facet_wrap(~ Stock, ncol = 1, scales = "free_y") +
  labs(title = "Daily Log Returns - AXP, CAT, SBUX",
       subtitle = "January 1999 - December 2008",
       x = "Date", y = "Log Returns (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1, "lines"))
print(plot_log_timeseries)

plot_log_distributions <- ggplot(log_returns_long, aes(x = log_percent)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~ Stock, ncol = 3, scales = "free_x") +
  labs(title = "Distribution of Daily Log Returns",
       subtitle = "Histogram with kernel density estimate (red curve)",
       x = "Log Returns (%)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
print(plot_log_distributions)


# (d) One-sample t-tests on H0: mean of log returns = 0

ttest_axp <- t.test(log_percent_axp)
ttest_cat <- t.test(log_percent_cat)
ttest_sbux <- t.test(log_percent_sbux)

print(ttest_axp)
print(ttest_cat)
print(ttest_sbux)

ttest_summary <- data.frame(
  Stock = c("AXP", "CAT", "SBUX"),
  Mean = c(ttest_axp$estimate, ttest_cat$estimate, ttest_sbux$estimate),
  t_stat = c(ttest_axp$statistic, ttest_cat$statistic, ttest_sbux$statistic),
  df = c(ttest_axp$parameter, ttest_cat$parameter, ttest_sbux$parameter),
  p_value = c(ttest_axp$p.value, ttest_cat$p.value, ttest_sbux$p.value),
  reject_H0 = c(ttest_axp$p.value < 0.05,
                ttest_cat$p.value < 0.05,
                ttest_sbux$p.value < 0.05)
)
kable(ttest_summary, digits = 4,
      caption = "Three separate t-tests on H0: mean of log returns = 0")


# Save tables and plots for the LaTeX report

write.csv(stats_simple_summary, paths$result("exercise1_1_simple_summary.csv"))
write.csv(stats_log_summary, paths$result("exercise1_1_log_summary.csv"))
write.csv(ttest_summary, paths$result("exercise1_1_ttests.csv"), row.names = FALSE)

ggsave(paths$figure("exercise1_1_log_returns_timeseries.png"), plot_log_timeseries,
       width = 11, height = 8, units = "in", dpi = 300)
ggsave(paths$figure("exercise1_1_log_returns_distributions.png"), plot_log_distributions,
       width = 12, height = 5, units = "in", dpi = 300)
