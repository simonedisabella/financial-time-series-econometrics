# Exercise 2.4

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_02")


ibm_data <- read.xlsx(paths$data("d-ibm3dx7008.xlsx"), detectDates = FALSE)

# The column headers carry leading/trailing spaces and the date is an integer
# in YYYYMMDD format, so I clean both before doing anything else.
names(ibm_data) <- trimws(names(ibm_data))
ibm_data$Date <- as.Date(as.character(ibm_data$Date), format = "%Y%m%d")

dim(ibm_data)
head(ibm_data)
tail(ibm_data)

# Two trading days have a missing IBM return, so I drop them before the ACF
# (acf() does not accept missing values).
simple_returns_ibm <- ibm_data$rtn[!is.na(ibm_data$rtn)]
absolute_returns <- abs(simple_returns_ibm)
n_obs <- length(simple_returns_ibm)


# First 100 lags of the ACF of the absolute returns. I also compute the ACF of
# the raw returns to contrast short memory against long memory.

max_lag <- 100
acf_absolute <- acf(absolute_returns, lag.max = max_lag, plot = FALSE)
acf_raw <- acf(simple_returns_ibm, lag.max = max_lag, plot = FALSE)

significance_band <- 1.96 / sqrt(n_obs)

acf_abs_values <- as.numeric(acf_absolute$acf)[-1]
acf_raw_values <- as.numeric(acf_raw$acf)[-1]

significant_abs <- sum(abs(acf_abs_values) > significance_band)
significant_raw <- sum(abs(acf_raw_values) > significance_band)

cat("Number of observations:", n_obs, "\n")
cat("Significance band +/- 1.96/sqrt(n):", round(significance_band, 4), "\n")
cat("Absolute returns: significant lags among the first 100 =", significant_abs, "\n")
cat("Raw returns: significant lags among the first 100 =", significant_raw, "\n")


# Representative lags for the report

representative_lags <- c(1, 5, 10, 20, 50, 100)
acf_summary <- data.frame(
  Lag = representative_lags,
  ACF_absolute = round(acf_abs_values[representative_lags], 4),
  ACF_raw = round(acf_raw_values[representative_lags], 4)
)
kable(acf_summary, digits = 4,
      caption = "Exercise 2.4 - Sample ACF of IBM absolute and raw daily returns at selected lags")


# Full ACF stored for the LaTeX report

acf_full <- data.frame(
  Lag = 1:max_lag,
  ACF_absolute = round(acf_abs_values, 6),
  ACF_raw = round(acf_raw_values, 6)
)


# Plot: ACF over 100 lags, absolute returns against raw returns on the same scale

acf_long <- data.frame(
  Lag = rep(1:max_lag, times = 2),
  Series = factor(rep(c("Absolute returns |r_t|", "Raw returns r_t"),
                      each = max_lag),
                  levels = c("Absolute returns |r_t|", "Raw returns r_t")),
  ACF = c(acf_abs_values, acf_raw_values)
)

plot_acf_compare <- ggplot(acf_long, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = c(significance_band, -significance_band),
             color = "blue", linewidth = 0.4, linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), color = "steelblue", linewidth = 0.45) +
  facet_wrap(~ Series, ncol = 1) +
  labs(title = "Sample ACF of IBM Daily Returns - First 100 Lags",
       subtitle = "Absolute returns (long memory) versus raw returns (short memory)",
       x = "Lag", y = "Autocorrelation") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        panel.spacing = unit(1, "lines"))
print(plot_acf_compare)


write.csv(acf_summary, paths$result("exercise2_4_acf_summary.csv"), row.names = FALSE)
write.csv(acf_full, paths$result("exercise2_4_acf_full.csv"), row.names = FALSE)

ggsave(paths$figure("exercise2_4_acf_absolute.png"), plot_acf_compare,
       width = 11, height = 8, units = "in", dpi = 300)
