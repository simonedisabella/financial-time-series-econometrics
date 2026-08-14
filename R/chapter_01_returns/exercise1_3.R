# Exercise 1.3
# Monthly S&P composite returns (Jan 1975 - Dec 2008). Same file as Exercise 1.2.

library(ggplot2)
library(dplyr)
library(kableExtra)
library(openxlsx)

# Repository paths (portfolio version). Run from repository root.
source(file.path("R", "utils.R"))
paths <- project_paths("chapter_01")


monthly_data <- read.xlsx(paths$data("m-gm3dx7508.xlsx"), detectDates = TRUE)
monthly_data$Date <- as.Date(monthly_data$Date)

simple_returns_sp <- monthly_data$sp
log_returns_sp <- log(1 + simple_returns_sp)

number_of_months <- length(log_returns_sp)
number_of_years <- number_of_months / 12


# (a) Average annual log return

# Log returns are additive over time, so the total log return is just the sum
total_log_return <- sum(log_returns_sp)
average_monthly_log <- mean(log_returns_sp)
average_annual_log <- total_log_return / number_of_years

cat("Total log return:", round(total_log_return, 6), "\n")
cat("Average monthly:", round(average_monthly_log, 6), "\n")
cat("Average annual:", round(average_annual_log, 6),
    " (", round(average_annual_log * 100, 3), "%)\n", sep = "")


# (b) Terminal value of $1 invested at the start of 1975

initial_investment <- 1.00

# I compute the final wealth two ways and check they match
final_value_via_simple <- initial_investment * prod(1 + simple_returns_sp)
final_value_via_log <- initial_investment * exp(total_log_return)

cat("Final value (prod 1+R): $", round(final_value_via_simple, 4), "\n", sep = "")
cat("Final value (exp sum r): $", round(final_value_via_log, 4), "\n", sep = "")


ex1_3_summary <- data.frame(
  Quantity = c("Number of monthly observations",
               "Number of years in the sample",
               "Total log return (sum)",
               "Average monthly log return",
               "Average annual log return",
               "Final value of $1.00 invested at start of 1975"),
  Value = c(number_of_months,
            number_of_years,
            round(total_log_return, 4),
            round(average_monthly_log, 6),
            round(average_annual_log, 6),
            round(final_value_via_simple, 4))
)
kable(ex1_3_summary, caption = "S&P composite, Jan 1975 - Dec 2008")


# Cumulative wealth path: V_t = exp(cumulative sum of log returns)

cumulative_wealth <- initial_investment * exp(cumsum(log_returns_sp))
wealth_data <- data.frame(Date = monthly_data$Date, Wealth = cumulative_wealth)

plot_cumulative_wealth <- ggplot(wealth_data, aes(x = Date, y = Wealth)) +
  geom_hline(yintercept = 1, color = "grey60", linewidth = 0.3, linetype = "dashed") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(title = "Value of $1.00 Invested in the S&P Composite Index",
       subtitle = "January 1975 - December 2008 (dividends reinvested, no transaction costs)",
       x = "Date", y = "Wealth ($)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11))
print(plot_cumulative_wealth)


write.csv(ex1_3_summary, paths$result("exercise1_3_summary.csv"), row.names = FALSE)
ggsave(paths$figure("exercise1_3_cumulative_wealth.png"), plot_cumulative_wealth,
       width = 11, height = 6, units = "in", dpi = 300)
