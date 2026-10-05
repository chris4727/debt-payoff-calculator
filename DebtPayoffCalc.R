#########################################################################
#########################################################################
######### Debt Payoff Calculator  #######################################
######### (c) 2022 Thomas Cushman #######################################
#########################################################################
#########################################################################

#Set up the environment
require(tidyverse)
require(lubridate)
require(scales)
gc()
# Clear Console
cat("\014")
# Start with a fresh environment
rm(list = ls())

#1: Set principal as p
p <- 15020.28

#2: Set APY as i represented as full decimal percentage
i <- 25.99

#3: Set desired monthly payment as pmt
pmt <- 3007

#4: Set an optional start date
start_date <- "2026-10-04"

#Now run and enjoy the output
cnt <- 0
tot_interest <- 0

#All the calculation goodness
i <- i/100
while (p > 0) {
  tot_interest <- tot_interest + ((i/12)*p)
  p <- p + ((i/12)*p)
  if (exists("payoff_table")) {
    month <- cnt
    rem_principal <- p
    payoff_table <- add_row(payoff_table, month = cnt, rem_principal = p)
  } else {
    month <- cnt
    rem_principal <- p
    payoff_table <- tibble(month, rem_principal)
  }
  if (p < pmt) {
    final_p <- round(p, 2)
  }
  p <- p - pmt
  cnt <- cnt + 1
}

# Add dates
# Sets date to today if none is entered
if (is.null(start_date)) {
  start_date <- lubridate::today()
}
# Adds dates to payoff_table
payoff_table <- payoff_table |> 
  dplyr::mutate(
    date = as.Date(start_date) %m+% months(month)
  )

#Output variable parsing/reformatting
tot_interest <- round(tot_interest, 2)
final_pmt <- round(as.double(payoff_table[cnt, "rem_principal"]), 2)
final_month <- cnt + 1
rem_principal <- round(rem_principal, 2)
payoff_table$rem_principal <- round(payoff_table$rem_principal, 2)
payoff_date <- format(max(payoff_table$date), "%B %Y")

#Final output code
index <- 1
for (index in 1:nrow(payoff_table)) {
  print(sprintf("Month %i remaining balance: $%.2f",index-1, payoff_table$rem_principal[index]))
}

print(sprintf("Your debt will be paid off in %i months", index))
print(sprintf("Your payoff date is %s", payoff_date))
print(sprintf("Total interest paid will be $%.2f", tot_interest))

# Plot principle over time
line_plot <- payoff_table |> 
  ggplot(aes(
    x = date,
    y = rem_principal,
    group = 1
  )) +
  geom_line() +
  geom_point() +
  labs(
    title = "Principal over Time",
  ) +
  scale_x_date("Month",
               breaks = scales::breaks_width("3 months"),
               labels = scales::label_date_short()
  ) +
  scale_y_continuous("Principal",
                     breaks = scales::breaks_extended(16),
                     labels = scales::label_dollar()
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    )
  )
line_plot

#Uncomment to save plot to working directory
#ggsave("rem_principal_plot.png", width = 5, height = 4, dpi = 300)

