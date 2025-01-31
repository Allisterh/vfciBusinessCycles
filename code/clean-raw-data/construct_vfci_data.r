library(data.table)
library(stringr)
library(quantmod)
library(vfciBCHelpers)

# FRED data ---------------------------------------------------------------
fred_raw <- data.table::fread("./data-raw/vfci_data_fred.csv") |>
  _[, date := as.IDate(date)]

# Yahoo! Finance -----------------------------------------------------------
yahoo_raw <-
  data.table::fread("./data-raw/vfci_data_yahoo.csv") |>
  _[, symbol := str_replace(symbol, "\\^", "")] |>
  _[, date := as.IDate(date)]

# BCA data -----------------------------------------------------------------
bca_data <- fread("./data-raw/bca_current_data.csv")
bca_data <- bca_data[, .(date, output, consumption)] |>
  tidyfast::dt_pivot_longer(-date, names_to = "symbol", values_to = "price") |>
  _[, date := as.IDate(date)]

fred_raw <- purrr::list_rbind(list(fred_raw, bca_data)) |> copy()


# Create variables --------------------------------------------------------

# clean up fred_raw
fred <-
  fred_raw |>
  _[, date_qtr := round(date, digits = "quarters")] |>
  _[, .(price = mean(price, na.rm = TRUE)), by = .(date_qtr, symbol)] |>
  tidyfast::dt_pivot_wider(names_from = symbol, values_from = price) |>
  setnames("date_qtr", "date")

log_vars <- c("GDPC1", "PCEPILFE", "PCECC96", "CLVMNACSCAB1GQEA19")

fred |>
  _[, c(log_vars) := lapply(.SD, log), .SDcols = log_vars] |>
  _[, gap := 100 * (GDPC1 / GDPPOT - 1)] |>
  _[, baa_aaa := BAA10YM - AAA10YM] |>
  _[, t10y3m := DGS10 - TB3MS] |>
  setnames("TEDRATE", "tedr") |>
  _[date <= as.IDate("1985-10-01"), tedr := LIOR3M - TB3MS] |>
  _[, es := MED3 - TB3MS]

colnames(fred) <- colnames(fred) |> tolower()


## Variables derived from Yahoo! finance data -----------------------------

# clean up yahoo_raw
yahoo_avg_price <-
  yahoo_raw |>
  _[, date_qtr := round(date, digits = "quarters")] |>
  _[, .(adjusted = mean(adjusted, na.rm = TRUE)), by = .(date_qtr, symbol)] |>
  tidyfast::dt_pivot_wider(names_from = symbol, values_from = adjusted) |>
  setnames("date_qtr", "date")

# compute quarterly frequency returns and volatility from daily returns
yahoo_avg_return <-
  yahoo_raw |>
  _[, return := (adjusted / shift(adjusted, 1) - 1), by = .(symbol)] |>
  _[, .(date, symbol, return)] |>
  tidyfast::dt_pivot_wider(names_from = symbol, values_from = return) |>
  _[, date_qtr := round(date, digits = "quarters")] |>
  _[, .(
    gspc_ret = mean(GSPC, na.rm = TRUE),
    gspc_vol = 100 * sqrt(252) * sd(GSPC, na.rm = TRUE)
  ), by = .(
    date_qtr
  )] |>
  setnames("date_qtr", "date")

yahoo <- yahoo_avg_price |>
  merge(yahoo_avg_return, by = "date", all = FALSE)
colnames(yahoo) <- colnames(yahoo) |> tolower()


## S&P 500 returns ---------------------------------------------------------
### Compute S&P 500 returns ------------------------------------------------------
# keep only adjusted end-of-day price
price <-
  yahoo_raw |>
  _[, .(symbol, date, adjusted)]


returns <-
  price |>
  copy() |>
  _[, .(date, adjusted)] |>
  quantmod::allReturns(type = "arithmetic") |>
  as.data.table() |>
  setnames("index", "date") |>
  _[, date := as.IDate(date)] |>
  # annualize
  _[, daily := 252 * daily] |>
  _[, weekly := NULL] |>
  _[, monthly := 12 * monthly] |>
  _[, quarterly := 4 * quarterly]

### Aggregate to quarterly frequency --------------------------------------
#### Returns --------------------------------------------------------------
returns_aggregated_quarterly <-
  returns |>
  _[, date_qtr := round(date, digits = "quarters")] |>
  _[, .(
    daily_avg = mean(daily, na.rm = TRUE),
    quarterly_avg = na.omit(quarterly)
  ), by = .(
    date_qtr
  )] |>
  setnames("date_qtr", "date")


price_aggregated_quarterly <-
  price[, .(date, adjusted)] |>
  _[, date_qtr := round(date, digits = "quarters")] |>
  _[, max_date_qtr := max(date), by = .(date_qtr)] |>
  _[date == max_date_qtr, .(date_qtr, adjusted)] |>
  setnames("date_qtr", "date")

# Compute annual returns at quarterly frequency in four different ways
annual_returns <-
  returns_aggregated_quarterly |>
  merge(price_aggregated_quarterly, by = "date", all = FALSE) |>
  _[, annual_ret_from_daily_avg := 100 * frollmean(daily_avg, n = 4, align = "right")] |>
  _[, annual_cumret_from_quart_daily_avg := 100 * (
    frollapply(daily_avg, n = 4, FUN = function(x) {
      purrr::reduce(x, \(cumret, ret) cumret * (1 + ret / 4), .init = 1)
    }, align = "right") - 1
  )] |>
  _[, annual_avgret_from_quart := 100 * frollmean(quarterly_avg, n = 4, align = "right")] |>
  _[, annual_ret := 100 * (adjusted / shift(adjusted, 4) - 1)]  |>
  _[, .(
    date,
    annual_ret_from_daily_avg,
    annual_cumret_from_quart_daily_avg,
    annual_avgret_from_quart,
    annual_ret
  )]


# Merge all
variables <-
  list(fred, yahoo, annual_returns) |>
  purrr::reduce(merge, by = "date", all = TRUE)

# VFCI setup -------------------------------------------------------
# choose returns from
# annual_ret_from_daily_avg, annual_cumret_from_quart_daily_avg,
# annual_avgret_from_quart, annual_ret
financial_vars <- c("gspc_vol", "annual_ret", "t10y3m", "tb3smffm", "aaa10ym", "baa_aaa")

## Construct PCs from financial variables and merge onto variables
pc_dates <- c("1962-01-01", "2024-10-01")
pca_data <-
  variables |>
  _[date %between% pc_dates] |>
  _[, c("date", financial_vars), with = FALSE]
pca <- pca_data[, -"date"] |> prcomp(center = TRUE, scale. = TRUE)
pcs <- pca$x
pc_vars <- colnames(pcs) <- paste0("pc", seq_len(ncol(pcs)))
pca_data[, c(pc_vars) := as.data.table(pcs)]

variables <-
  variables |>
  merge(pca_data[, -c(financial_vars), with = FALSE], by = "date", all = TRUE)


## Save out VFCI data which will get used to construct VFCI on the fly
fwrite(
  variables,
  "./data/vfci_data.csv"
)
