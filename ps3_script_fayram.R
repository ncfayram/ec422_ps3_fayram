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
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |> ggplot(aes(x = Month, y = log_volume)) +
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

# Growth Rates
log_volume <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

log_volume |> autoplot(growth_direct) +
  labs(
    title = "Canadian Gasoline Production Growth Rates",
    x = "Date", y = "Billions of Cubic Meters per Year", color = NULL
  ) +
  theme_minimal()

growth_direct <- 100 * difference(Volume) / lag(Volume)
growth_logdiff <- 100 * difference(log_volume)

can_gas <- log_volume |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |> autoplot(growth_gap) +
  labs(
    title = "Canadian Gasoline Production Growth Rate Differences",
    x = "Date", y = "Billions of Cubic Meters per Year", color = NULL
  ) +
  theme_minimal()
# The log difference estimation is pretty good for the mos part, but every couple of years, the gap between the two methods spikes up to several billions of cubic meters per year. Although the majority of the data is similar, these big gaps imply that the log approximation isn't consistently reliable for this data set.

#SA NSA Growth Rates
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

#The not seasonally adjusted series has significantly more variance and noise in its month-over-month growth rate. This is because those growth rates are greatly exacerbated by the seasonal swings so the the RSAFSNA picks that up, whereas the RSAFS series only accounts for the additional changes in growth rates by less influential factors. For this reason, the two series follow the same trend, but the spikes in the RSAFSNA data are much more pronounced.

# Seasonals
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
# The seasonal pattern is very clear in the RSAFSNA data. Each line representing one year follows the same positive and negative slope trends for the most part as well as increasing gradually over the course of the year. The RSAFS data, however, has no clear seasonal pattern as each year follows a much more linear trend.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gasoline Production",
    y = "Billions of Cubic Meters"
  ) +
  theme_minimal()
# The seasonal pattern in the can_gas data is slightly U-shaped as opposed to the increasing trend with RSAFS. Gasoline production is higher in the winter months and hits a minimum around June/July. This makes sense with the idea that natural gas consumption would increase for heating purposes in the colder months and wouldn't be as necessary in the Summer.
# Subseries
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# X-13ARIMA-SEATS
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
# December seems to have the highest seasonal swing. Maxing out in 1993 around 22% above the trend, but staying consistently 10-20% above the trend for the duration of the series. The seasonal swings get slightly smaller over time.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
# My manual adjustment is largely within one percent of the official seasonal adjustment. The difference only exceeds that margin between 2020 and 2024 which makes sense because COVID-19 was a highly irregular event and therefore is likely to cause some significant estimation uncertainty.
# Linear Trend w/ Seasonal Dummies
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)
can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
# The average monthly growth rate for Canadian gasoline production is 0.004. June has the largest statistical effect on this with a t-statistic of -4.777.

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
# At first glance, the fitted values seem to do an okay job at predicting the actual values. At some point it seems to line up exactly. Where the fitted model falters though is when the overall growth trend is non linear. The growth rate is very high toward the beginning and slows very quickly for example. That is something this fitted model cannot account for. However the model does a great job at predicting the trend and seasonal swing between 1980 and 2000 where the overall trend is roughly linear.
# Testing for white noise residuals
can_gas_fit |> gg_tsresiduals()
can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
# The Ljung-Box p-value is less than 0.05. Therefore we reject the null hypothesis that the residuals are white noise, indicating that there is some autocorrelation in the residuals. This makes sense because the model does not account for the non-linear growth trends in the data. We also see this visually in the ACF graph where there is significant autocorrelation through the 24th lag.

# Part 5
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# The first prediction comes at point 241, which is in January of 1980. The last forecast prediction is in February of 2005 as that is the last point of the pseudo-out-of-sample period. The snaive model has an MAE of 0.562 and an RMSE of 0.702. The tslm model has an MAE of 4.01 and an RMSE of 4.36. Both metrics agree that the snaive model is a better fit for this data.
# DM Test
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
# The DM test statistic is -31.278 with a p-value of <2.2e-16. Therefore at the 5% significance level, we reject the null hypothesis of homoskedasticity and conclude that the snaive model is significantly more accurate than the tslm model for this data.
# Expanding window
gas_cv_fc_roll<- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_roll |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# The MAE of the snaive model is 0.562 and the RMSE is 0.702. The MAE of the tslm model is 2.09 and the RMSE is 2.42. This snaive model is still a better fit for the data with a rolling window, but the tslm model metrics changed significantly more than the snaive model moving from an expanding window to a rolling one. This makes sense because the model only takes the most recent 240 months of data into account, meaning it's more flexible and adaptable to changing growth rates allowing it to fit the series better.