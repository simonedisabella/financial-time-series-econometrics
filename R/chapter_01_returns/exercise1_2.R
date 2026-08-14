# Exercise 1.2
# Monthly returns of GM, CRSP value-weighted (VW), CRSP equal-weighted (EW) and
# S&P composite (SP) from January 1975 to December 2008.

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_01")


# Load the data
monthly_data <- read.xlsx(paths$data("m-gm3dx7508.xlsx"), detectDates = TRUE)
monthly_data$Date <- as.Date(monthly_data$Date)

dim(monthly_data)
head(monthly_data)

simple_returns_gm <- monthly_data$gm
simple_returns_vw <- monthly_data$vw
simple_returns_ew <- monthly_data$ew
simple_returns_sp <- monthly_data$sp


# (a) Summary statistics on simple returns in percent

simple_percent_gm <- simple_returns_gm * 100
simple_percent_vw <- simple_returns_vw * 100
simple_percent_ew <- simple_returns_ew * 100
simple_percent_sp <- simple_returns_sp * 100

rows_requested <- c("Mean", "Stdev", "Skewness", "Kurtosis", "Minimum", "Maximum")

stats_simple <- cbind(basicStats(simple_percent_gm),
                      basicStats(simple_percent_vw),
                      basicStats(simple_percent_ew),
                      basicStats(simple_percent_sp))
colnames(stats_simple) <- c("GM", "VW", "EW", "SP")
stats_simple_summary <- round(stats_simple[rows_requested, ], 3)
rownames(stats_simple_summary) <- c("Mean", "Std. Dev.", "Skewness",
                                    "Excess Kurtosis", "Minimum", "Maximum")
kable(stats_simple_summary,
      caption = "Monthly simple returns in percent")


# (b) Transform simple returns to log returns

log_returns_gm <- log(1 + simple_returns_gm)
log_returns_vw <- log(1 + simple_returns_vw)
log_returns_ew <- log(1 + simple_returns_ew)
log_returns_sp <- log(1 + simple_returns_sp)


# (c) Summary statistics on log returns in percent

log_percent_gm <- log_returns_gm * 100
log_percent_vw <- log_returns_vw * 100
log_percent_ew <- log_returns_ew * 100
log_percent_sp <- log_returns_sp * 100

stats_log <- cbind(basicStats(log_percent_gm),
                   basicStats(log_percent_vw),
                   basicStats(log_percent_ew),
                   basicStats(log_percent_sp))
colnames(stats_log) <- c("GM", "VW", "EW", "SP")
stats_log_summary <- round(stats_log[rows_requested, ], 3)
rownames(stats_log_summary) <- c("Mean", "Std. Dev.", "Skewness",
                                 "Excess Kurtosis", "Minimum", "Maximum")
kable(stats_log_summary,
      caption = "Monthly log returns in percent")


# Plots: reshape to long format and facet by series

log_returns_long <- data.frame(
  Date = rep(monthly_data$Date, times = 4),
  Series = factor(rep(c("GM", "VW", "EW", "SP"), each = nrow(monthly_data)),
                  levels = c("GM", "VW", "EW", "SP")),
  log_percent = c(log_percent_gm, log_percent_vw, log_percent_ew, log_percent_sp)
)

plot_log_timeseries <- ggplot(log_returns_long, aes(x = Date, y = log_percent)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  facet_wrap(~ Series, ncol = 1, scales = "free_y") +
  labs(title = "Monthly Log Returns - GM, VW, EW, SP",
       subtitle = "January 1975 - December 2008",
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
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~ Series, ncol = 2, scales = "free") +
  labs(title = "Distribution of Monthly Log Returns",
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

ttest_gm <- t.test(log_percent_gm)
ttest_vw <- t.test(log_percent_vw)
ttest_ew <- t.test(log_percent_ew)
ttest_sp <- t.test(log_percent_sp)

print(ttest_gm)
print(ttest_vw)
print(ttest_ew)
print(ttest_sp)

ttest_summary <- data.frame(
  Series = c("GM", "VW", "EW", "SP"),
  Mean = c(ttest_gm$estimate, ttest_vw$estimate, ttest_ew$estimate, ttest_sp$estimate),
  t_stat = c(ttest_gm$statistic, ttest_vw$statistic, ttest_ew$statistic, ttest_sp$statistic),
  df = c(ttest_gm$parameter, ttest_vw$parameter, ttest_ew$parameter, ttest_sp$parameter),
  p_value = c(ttest_gm$p.value, ttest_vw$p.value, ttest_ew$p.value, ttest_sp$p.value),
  reject_H0 = c(ttest_gm$p.value < 0.05,
                ttest_vw$p.value < 0.05,
                ttest_ew$p.value < 0.05,
                ttest_sp$p.value < 0.05)
)
kable(ttest_summary, digits = 4,
      caption = "Four separate t-tests on H0: mean of log returns = 0")


# Save tables and plots for the LaTeX report

write.csv(stats_simple_summary, paths$result("exercise1_2_simple_summary.csv"))
write.csv(stats_log_summary, paths$result("exercise1_2_log_summary.csv"))
write.csv(ttest_summary, paths$result("exercise1_2_ttests.csv"), row.names = FALSE)

ggsave(paths$figure("exercise1_2_log_returns_timeseries.png"), plot_log_timeseries,
       width = 11, height = 10, units = "in", dpi = 300)
ggsave(paths$figure("exercise1_2_log_returns_distributions.png"), plot_log_distributions,
       width = 11, height = 9, units = "in", dpi = 300)
