library(tidyverse)
library(fpp3)
library(BVAR)

# 2.1 The Data
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)
library(fpp3)
liquor |>
  autoplot(sales) +
  labs(
    title = "US Liquor Store Retail Sales",
    y = "Sales (millions of dollars)",
    x = "Month"
  )

liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Log of Liquor Store Sales (Seasonal)",
    y = "Log of Sales"
  )
# The trend of liquor store sales in the first graph shows an increase in liquor store sales since January 1992. In terms of seasonal pattern, liquor sales decrease during January and then increase slowly until July, then decrease a little bit and then increase rapidly from Nov-Dec. The seasonal swings do grow with the level of the series, showing that taking the log of the graph was a good idea. 

# 2.2 Fit Both Models
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()
# The ARIMA model says that the non-seasonal model has zero difference, and the seasonal model has 1 difference. The three letters of the chosen ETS are A, A, and A, so additive error, additive trend, and additive seasonality. This means that all three ETS are not multiplicative. 
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
# The residuals do look like white noise, there does not seem to be a trend or pattern

# 2.3 The Horse Race
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
# 216 different forecast origins will be considered in this TCSV
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
# The date of the first forecast is January 2002, and the last forecast date is December 2019. 
liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  knitr::kable(digits = 2)
# The ARIMA model forecasts most accurately because the RMSE and the MAE are both the lowest. The metrics do agree because they are both the lowest for the ARIMA model. 

# 2.4 Are the Differences Significant?
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)
# DM: -2.9062, p-value: .00404. We reject the null because the p-value is less than 5%, so there is enough evidence to conclude that ARIMA and ETS are not equally accurate

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)
# DM: -10.755, p-value: < 2.23e-16. We reject the null because the p-value is less than 5%, there is enough evidence to conclude that ARIMA and snaive are not equally accurate. 
# As the liquor store chain, I would use the ARIMA model

# 3 Forecast Combination
