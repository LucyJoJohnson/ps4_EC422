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
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  select(.model, RMSE, MAE) |>
  knitr::kable(digits = 2)
# Combo RMSE and MAE is slightly higher than the errors of the ARIMA model, and lower than the ETS model, showing that the ARIMA model is once again the best choice for minimizing error
cor(liquor_errs$arima, liquor_errs$ets)
# Based off of the DM test of -2.9062 for ARIMA and ETS, and a correlation of .87 for the combo of ARIMA and ETS, the combo did not beat ARIMA because ETS is more inaccurate than ARIMA, therefore dragging down ARIMA's accuracy when the errors are combined, and also they are closely correlated so they will be wrong in the same spots, which does not improve the combo.

# 4.1 The Data
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]
# We will produce 120 forecasts for each variable ((2019-1990) times 4, plus 4). The first forecast will be Q1 1990, and the last forecast will be Q4 December 2019

# 4.2 OLS VARS
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}
rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)
# On the whole var1 forecasts better, for 7 out of the 8 variables, everything except for GDP price index. The smaller model might forecast better because it is less complicated and does not over fit the model. Less parameters produce less forecast error, so the forecasting for var1 is better.

# 4.3 A BVAR
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)
# All of the values of the BVAR lambda 0.3 are lower than the VAR4 values. This difference is explained by the Minnesota prior which gives more weight to shorter lags by shrinking the longer lags more, which leads to a lower variance. 
# Compared with VAR1, 5 values of the BVAR are greater and 3 are less than the VAR1, so the BVAR forecasting is less accurate than the VAR1
# BVAR with lambda at .05 is less accurate than 7 out of the 8 variables of VAR1, but more accurate than 3 out of the 8 variables for VAR4. This tells me that the amount of sjrinkage is too much. I think the winner is VAR1, and I think that its RMSE is a fair estimate of how accurately it will forecast in the future because the model does not have too many variables and is not being shrunk by a prior. This will lead to less approximation error, and also has enough variables to lead to less estimation error.

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

e_var4 <- actual[, "infl"] - fc_var4[, 5]
e_bvar <- actual[, "infl"] - fc_bvar_03[, 5]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)
# For GDP, there is enough evidence to reject the null and conclude that VAR4 and BVAR lambda .3 are not equally accurate. For GDP price index (infl), there is not enough evidence to concluce that VAR4 and BVAR mabda .3 are not equally accurate, so we fail to reject the null. I think that this does not change my results of the part above because I don't think either of these options are the best for forecasting accurately. If anything the similarity between the GDP price index and the GDP, which are both higher than the GDP error for VAR1, shows me that both VAR4 and BVAR .3 are more inaccurate.
