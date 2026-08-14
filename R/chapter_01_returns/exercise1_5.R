# Exercise 1.5
# Daily exchange rates Jan 2000 - Mar 2009: CAUS, JPUS, USEU, USUK.
# The file holds spot rates (levels), so log returns are diff(log(P)).

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_01")


exchange_rates_data <- read.xlsx(paths$data("exrate.xlsx"), detectDates = TRUE)
exchange_rates_data$Date <- as.Date(exchange_rates_data$Date)

dim(exchange_rates_data)
head(exchange_rates_data)
tail(exchange_rates_data)
summary(exchange_rates_data[, -1])


# (a) Daily log returns of each rate

log_returns_caus <- diff(log(exchange_rates_data$CAUS))
log_returns_jpus <- diff(log(exchange_rates_data$JPUS))
log_returns_useu <- diff(log(exchange_rates_data$USEU))
log_returns_usuk <- diff(log(exchange_rates_data$USUK))

dates_returns <- exchange_rates_data$Date[-1]

log_percent_caus <- log_returns_caus * 100
log_percent_jpus <- log_returns_jpus * 100
log_percent_useu <- log_returns_useu * 100
log_percent_usuk <- log_returns_usuk * 100


# (b) Summary statistics on the % log returns
# The exercise requests standard deviation (not variance); keep Stdev from fBasics::basicStats.

stats_caus <- round(basicStats(log_percent_caus), 4)
stats_jpus <- round(basicStats(log_percent_jpus), 4)
stats_useu <- round(basicStats(log_percent_useu), 4)
stats_usuk <- round(basicStats(log_percent_usuk), 4)

stats_all <- cbind(stats_caus, stats_jpus, stats_useu, stats_usuk)
colnames(stats_all) <- c("CAD/USD", "JPY/USD", "USD/EUR", "USD/GBP")

rows_requested <- c("Mean", "Stdev", "Skewness", "Kurtosis", "Minimum", "Maximum")
stats_summary <- stats_all[rows_requested, ]
rownames(stats_summary) <- c("Mean", "Std. Dev.", "Skewness",
                             "Excess Kurtosis", "Minimum", "Maximum")

kable(stats_summary,
      caption = "Descriptive Statistics - Daily Log Returns of Exchange Rates (%)")


# (d) Density plot of USD/EUR log returns
# kernel density (red) against a fitted normal with the same mean and sd (green dashed)

m_useu <- mean(log_percent_useu)
sd_useu <- sd(log_percent_useu)

plot_density_useu <- ggplot(data.frame(log_percent_useu = log_percent_useu),
                            aes(x = log_percent_useu)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = m_useu, sd = sd_useu),
                color = "darkgreen", linewidth = 1, linetype = "dashed") +
  labs(title = "Distribution of Daily Log Returns - USD/EUR Exchange Rate",
       subtitle = "January 2000 - March 2009. Red = kernel density, green dashed = fitted normal",
       x = "Log Returns (%)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_density_useu)


# Companion plot for the report: all four currencies in a 2x2 grid

log_returns_long <- data.frame(
  Date = rep(dates_returns, times = 4),
  Currency = factor(rep(c("CAD/USD", "JPY/USD", "USD/EUR", "USD/GBP"),
                        each = length(dates_returns)),
                    levels = c("CAD/USD", "JPY/USD", "USD/EUR", "USD/GBP")),
  log_percent = c(log_percent_caus, log_percent_jpus,
                  log_percent_useu, log_percent_usuk)
)

plot_distributions_all <- ggplot(log_returns_long, aes(x = log_percent)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~ Currency, ncol = 2, scales = "free") +
  labs(title = "Distribution of Daily Log Returns - Four Exchange Rates",
       subtitle = "Histogram with kernel density estimate (red)",
       x = "Log Returns (%)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
print(plot_distributions_all)


write.csv(stats_summary, paths$result("exercise1_5_summary.csv"))

ggsave(filename = paths$figure("exercise1_5_USEU_density.png"),
       plot = plot_density_useu,
       width = 10, height = 6, units = "in", dpi = 300)

ggsave(filename = paths$figure("exercise1_5_all_distributions.png"),
       plot = plot_distributions_all,
       width = 11, height = 9, units = "in", dpi = 300)
