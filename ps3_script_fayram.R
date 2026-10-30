setwd("C:/Users/16085/Documents/ECON/ec422_ps3_fayram")
library(tidyverse)


retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

#Long form of the data
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

# Canadian Gas
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)
# No gaps in the data!

# 1990 on
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

# Average monthly volume of gas sold in Canada
can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
# If we were to average the data by month as opposed to summing it, the proportion of the the seasonal swings relative to one another would be altered. By summing them, we maintain the true cumulative seasonal swing. Even at a lower frequency the data still appears to be seasonal.

# Back to US retail sales
retail_ts |> autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
# RSAFSNA displays a high peak and a sharp drop right at the start of Q1 each year. This seasonal cycle is absent in the RSAFS data, however the RSAFS data precisely follows the overall trend seen in RSAFSNA.

can_gas |> autoplot(Volume) +
  labs(
    title = "Canadian Gasoline Production",
    x = "Date", y = "Billions of Cubic Meters", color = NULL
  ) +
  geom_smooth() +
  theme_minimal()
# 2.7d I added geom_smooth() to the plot to show the overall trend of the data. The amplitude of the seasonal swings varies greatly over the time series and can get difficult to distinguish toward the end. The smooth line clarifies the overall trend of the data for the reader.

# 3.1a The seasonal swings of the series initially appear to be growing over time, but after about 1988 the swings begin to diminish in amplitude.
# Log
log_volume <- can_gas |>
  mutate(log_volume = log(Volume))

log_volume |> ggplot(aes(x = Month, y = log_volume)) +
  geom_line() +
  labs(
    title = "Canadian Gasoline Production",
    x = "Date",
    y = "Log(Billions of Cubic Meters)"
  ) +
  theme_minimal()
# The amplitude of the seasonal swings shrank, and the changing amplitude of these swings over time is relatively more consistent than it was before the log transformation.
# I think that a log of the entire series is not a great idea because as the seasonal swings get smaller toward the end the graph becomes pretty muddled. I think logs could be a good idea if the time frame was limited to just the first 20-25 years of the data where the seasonal swing amplitude is consistently growing with the level.

log_sales <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

log_sales |> ggplot(aes(x = Month, y = log_sales)) +
  geom_line() +
  labs(
    title = "Retail and Food Services Sales",
    x = "Date",
    y = "Log(Millions of dollars)"
  ) +
  theme_minimal()
# Logging this function has succeeded in stabilizing the variance of the seasonal swings over time. I believe this series has a stronger case for a log transformation than the Canadian Gas dataset because the seasonal swing variance grows more consistently with the level over time rather than beginning to shrink again after a while as seen in the gasoline series.