# Exercise 2.5

library(fBasics)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


aaa_data <- read.xlsx(paths$data("waaa.xlsx"), detectDates = TRUE)
baa_data <- read.xlsx(paths$data("wbaa.xlsx"), detectDates = TRUE)
aaa_data$Date <- as.Date(aaa_data$Date)
baa_data$Date <- as.Date(baa_data$Date)

# Drop any trailing empty rows, keeping Date and yield aligned
aaa_df <- na.omit(data.frame(Date = aaa_data$Date, Yield = aaa_data$WAAA))
baa_df <- na.omit(data.frame(Date = baa_data$Date, Yield = baa_data$WBAA))

yield_aaa <- aaa_df$Yield
yield_baa <- baa_df$Yield
n_aaa <- length(yield_aaa)
n_baa <- length(yield_baa)


# Summary statistics. The "Kurtosis" row from basicStats is already excess kurtosis.

rows_requested <- c("Mean", "Stdev", "Skewness", "Kurtosis", "Minimum", "Maximum")

stats_yields <- cbind(basicStats(yield_aaa), basicStats(yield_baa))
colnames(stats_yields) <- c("Aaa", "Baa")
stats_summary <- round(stats_yields[rows_requested, ], 4)
rownames(stats_summary) <- c("Mean", "Std. Dev.", "Skewness",
                             "Excess Kurtosis", "Minimum", "Maximum")
kable(stats_summary, caption = "Weekly Moody's Aaa and Baa bond yields")


# Tests for skewness and excess kurtosis at the 5% level.

skew_test <- function(x) {
  n <- length(x)
  s <- skewness(x)
  t <- s / sqrt(6 / n)
  c(estimate = as.numeric(s), t_stat = as.numeric(t),
    p_value = 2 * (1 - pnorm(abs(t))))
}

kurt_test <- function(x) {
  n <- length(x)
  k <- kurtosis(x)
  t <- k / sqrt(24 / n)
  c(estimate = as.numeric(k), t_stat = as.numeric(t),
    p_value = 2 * (1 - pnorm(abs(t))))
}

aaa_skew <- skew_test(yield_aaa)
baa_skew <- skew_test(yield_baa)
aaa_kurt <- kurt_test(yield_aaa)
baa_kurt <- kurt_test(yield_baa)

tests_summary <- data.frame(
  Series = c("Aaa", "Aaa", "Baa", "Baa"),
  Test = c("Skewness = 0", "Excess kurtosis = 0",
           "Skewness = 0", "Excess kurtosis = 0"),
  Estimate = round(c(aaa_skew["estimate"], aaa_kurt["estimate"],
                     baa_skew["estimate"], baa_kurt["estimate"]), 4),
  t_stat = round(c(aaa_skew["t_stat"], aaa_kurt["t_stat"],
                   baa_skew["t_stat"], baa_kurt["t_stat"]), 4),
  p_value = c(aaa_skew["p_value"], aaa_kurt["p_value"],
              baa_skew["p_value"], baa_kurt["p_value"]),
  reject_H0_5pc = c(aaa_skew["p_value"] < 0.05, aaa_kurt["p_value"] < 0.05,
                    baa_skew["p_value"] < 0.05, baa_kurt["p_value"] < 0.05),
  row.names = NULL
)
kable(tests_summary, digits = 4,
      caption = "Exercise 2.5 - Skewness and excess kurtosis tests on the yields (5% level)")


# Time series plot of the two yields

yields_long <- data.frame(
  Date = c(aaa_df$Date, baa_df$Date),
  Series = factor(c(rep("Aaa", n_aaa), rep("Baa", n_baa)), levels = c("Aaa", "Baa")),
  Yield = c(yield_aaa, yield_baa)
)

plot_yields <- ggplot(yields_long, aes(x = Date, y = Yield, color = Series)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Weekly Moody's Aaa and Baa Bond Yields",
       subtitle = "January 1962 - April 2009",
       x = "Date", y = "Yield (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.position = "bottom")
print(plot_yields)


# Distribution of the two yield series

plot_distributions <- ggplot(yields_long, aes(x = Yield)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~ Series, ncol = 2, scales = "free") +
  labs(title = "Distribution of Weekly Bond Yields",
       subtitle = "Histogram with kernel density estimate (red curve)",
       x = "Yield (%)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
print(plot_distributions)


write.csv(stats_summary, paths$result("exercise2_5_summary.csv"))
write.csv(tests_summary, paths$result("exercise2_5_tests.csv"), row.names = FALSE)

ggsave(paths$figure("exercise2_5_yields_timeseries.png"), plot_yields,
       width = 11, height = 6, units = "in", dpi = 300)
ggsave(paths$figure("exercise2_5_distributions.png"), plot_distributions,
       width = 11, height = 5, units = "in", dpi = 300)
