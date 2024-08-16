library(data.table)
library(dplyr)
library(stringr)
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(tsibble)
library(tidyr)
library(tidyquant)
library(vfciBCHelpers)

## Source helper scripts for constructing VFCI
files <- list.files("./code/clean-raw-data/vfci-data-helpers/", full.names = TRUE)
for (i in files) source(i)

# FRED data ---------------------------------------------------------------
fred_raw <- data.table::fread("./data-raw/vfci_data_fred.csv") %>%
  dplyr::mutate(date = as.Date(date))

# Yahoo! Finance -----------------------------------------------------------
yahoo_raw <- data.table::fread("./data-raw/vfci_data_yahoo.csv") %>%
  dplyr::mutate(symbol = purrr::map_chr(symbol, ~ stringr::str_replace(.x, pattern = "\\^", replacement = ""))) %>%
  dplyr::mutate(date = as.Date(date))

# Create variables --------------------------------------------------------

## Variables derived from FRED data ----------------------------------------

# pick dates, variables
date_begin <- "1960 Q1"
date_end <- "2023 Q1"

# clean up fred_raw
fred <- fred_raw %>%
  tsibble::as_tsibble(key = c("symbol")) %>%
  tsibble::group_by_key() %>%
  tsibble::index_by(qtr = ~ tsibble::yearquarter(.)) %>%
  dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  tsibble::filter_index(date_begin ~ date_end) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = price) %>%
  dplyr::mutate(dplyr::across(!qtr, ~ dplyr::na_if(., NaN))) %>%
  tibble::as_tibble()

# create variables
delta_fred <- 1:30
fred_var_list <- c("GDPC1", "PCEPILFE", "PCECC96", "CLVMNACSCAB1GQEA19")

fred <- fred %>%
  # growth rate over next delta quarters for variables in fred_var_list
  growth_rate_df(fred_var_list, delta_fred, future_growth = TRUE) %>%
  growth_rate_df(fred_var_list, delta_fred, shift = 1, future_growth = TRUE) %>%
  dplyr::mutate(
    # logs
    log = dplyr::across(
      dplyr::all_of(fred_var_list),
      ~ log(.x)
    ),
    # spreads
    gap = 100 * (GDPC1 / GDPPOT - 1),
    baa_aaa = BAA10YM - AAA10YM,
    t10y3m = DGS10 - TB3MS,
    # tedr starts 1970Q1, not 1962Q1 like rest of variables
    tedr = ifelse(tsibble::time_in(qtr, . ~ "1985 Q4"), LIOR3M - TB3MS, TEDRATE),
    es = MED3 - TB3MS,
  )

# rearrange, rename, tidy NA
fred <- tidyr::unnest(fred, cols = everything(), names_sep = ".")
fred <- dplyr::rename_with(fred, ~ tolower(.x))

## Variables derived from Yahoo! finance data -----------------------------
# clean up yahoo_raw
yahoo_avg_prc <- yahoo_raw %>%
  tsibble::as_tsibble(key = c("symbol")) %>%
  tsibble::group_by_key() %>%
  tsibble::index_by(qtr = ~ tsibble::yearquarter(.)) %>%
  dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  tsibble::filter_index(date_begin ~ date_end) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = adjusted) %>%
  dplyr::mutate(dplyr::across(!qtr, ~ dplyr::na_if(., NaN))) %>%
  dplyr::select(c("qtr", "GSPC")) %>%
  tibble::as_tibble()

# create variables

# compute quarterly frequency returns and volatility from daily returns
yahoo_avg_ret <- yahoo_raw %>%
  dplyr::group_by(symbol) %>%
  # daily returns
  tidyquant::tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    type = "arithmetic",
    col_rename = "ret"
  ) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = ret) %>%
  dplyr::mutate(qtr = zoo::as.yearqtr(date)) %>%
  dplyr::group_by(qtr) %>%
  dplyr::filter(dplyr::between(
    zoo::as.Date(qtr),
    zoo::as.Date(zoo::as.yearqtr(date_begin)),
    zoo::as.Date(zoo::as.yearqtr(date_end))
  )) %>%
  dplyr::summarize(
    dplyr::across(
      !date,
      list(
        # average daily returns over the quarter
        ret = ~ mean(.x, na.rm = TRUE),
        # annualized std dev of daily returns over the quarter
        vol = ~ 100 * sqrt(252) * sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  dplyr::mutate(dplyr::across(!qtr, ~ na_if(., NaN)))

# rearrange, rename, merge
yahoo_avg_ret$qtr <- tsibble::yearquarter(yahoo_avg_ret$qtr)

yahoo <- inner_join(yahoo_avg_prc, yahoo_avg_ret)
yahoo <- tidyr::unnest(yahoo, cols = everything(), names_sep = ".")
yahoo <- dplyr::rename_with(yahoo, ~ tolower(.x))

## S&P 500 returns ---------------------------------------------------------
### Compute S&P 500 returns ------------------------------------------------------
# keep only adjusted end-of-day price
price <- yahoo_raw %>%
  dplyr::select(symbol, date, adjusted)

returns <-
  purrr::map(
    list("arithmetic", "log"),
    \(type)
    price %>%
      tidyquant::tq_transmute(
        select = adjusted,
        mutate_fun = allReturns,
        type = type
      ) %>%
      dplyr::rename_with(\(name) if (type == "log") paste0(name, "_log") else name)
  ) %>%
  dplyr::bind_cols() %>%
  # remove weekly returns
  dplyr::select(!c(date_log) & !contains("weekly")) %>%
  # rename variables
  dplyr::rename_if(is.numeric, \(x) paste0(x, "_ret")) %>%
  # annualize
  dplyr::mutate(
    dplyr::across(dplyr::contains("daily"), \(x) 252 * x),
    dplyr::across(dplyr::contains("monthly"), \(x) 12 * x),
    dplyr::across(dplyr::contains("quarterly"), \(x) 4 * x)
  )

### Aggregate to quarterly frequency --------------------------------------
#### Returns --------------------------------------------------------------
returns_aggregated_quarterly <- timetk::summarize_by_time(
  returns,
  .date_var = date,
  .by = "quarter",
  dplyr::across(dplyr::where(is.numeric),
    \(x) aggregation_funs(x),
    .unpack = TRUE
  ),
  .type = "ceiling"
) %>%
  # shift to the last day of the period
  dplyr::mutate(date = timetk::subtract_time(date, "1 day")) %>%
  # remove column if all NA
  dplyr::select_if(~ !all(is.na(.)))

# rename identical columns with the same name
duplicated_cols <- duplicated(as.matrix(returns_aggregated_quarterly), MARGIN = 2)
names(returns_aggregated_quarterly)[duplicated_cols] <- stringr::str_remove_all(names(returns_aggregated_quarterly)[duplicated_cols], "_end_of_period|_avg")
# remove identical columns
returns_aggregated_quarterly <- dplyr::as_tibble(unique(as.matrix(returns_aggregated_quarterly), MARGIN = 2, fromLast = TRUE)) %>%
  dplyr::mutate(dplyr::across(!c("date"), as.numeric)) %>%
  dplyr::mutate(dplyr::across(dplyr::any_of("date"), lubridate::as_date))
# order columns
returns_aggregated_quarterly <- returns_aggregated_quarterly %>%
  dplyr::relocate(sort(names(.))) %>%
  dplyr::relocate(dplyr::any_of(c("date")))

#### Prices --------------------------------------------------------------
price_aggregated_quarterly <- timetk::summarize_by_time(
  price,
  .date_var = date,
  .by = "quarter",
  dplyr::across(adjusted, last, .unpack = TRUE),
  .type = "ceiling"
) %>%
  # shift to the last day of the period
  dplyr::mutate(date = timetk::subtract_time(date, "1 day")) %>%
  # add lags
  timetk::tk_augment_lags(adjusted, .lags = c(1, 4))

# merge returns and price
ts_q <- dplyr::full_join(returns_aggregated_quarterly, price_aggregated_quarterly, by = c("date"))

### Compute annual returns at quarterly frequency in four different ways --------------------------------------------------------------------
annual_returns <- ts_q %>%
  dplyr::mutate(
    # 1. Average daily returns over a year,
    annual_ret_from_daily_avg = 100 * mean_4q(daily_ret_avg),

    # 2. Quarterly returns computed from average daily returns, cumulated over a year
    annual_cumret_from_quart_daily_avg = 100 * (cumret_4q(daily_ret_avg) - 1),

    # 3. Quarterly returns averaged over a year
    annual_avgret_from_quart = 100 * mean_4q(quarterly_ret),

    # 4. Quarterly returns computed from average daily returns, cumulated over a year
    annual_ret = 100 * (adjusted / adjusted_lag4 - 1)
  ) %>%
  dplyr::select(
    date,
    annual_ret_from_daily_avg, # 1
    annual_cumret_from_quart_daily_avg, # 2
    annual_avgret_from_quart, # 3
    annual_ret # 4
  )

annual_returns$qtr <- tsibble::yearquarter(annual_returns$date)
annual_returns <- annual_returns %>%
  dplyr::select(-c(date))

# Merge all
variables <- purrr::reduce(list(fred, yahoo, annual_returns), dplyr::full_join, by = "qtr")

# VFCI setup -------------------------------------------------------
financial_vars <- c("gspc_vol", "annual_ret", "t10y3m", "tb3smffm", "aaa10ym", "baa_aaa") # choose returns from # annual_ret_from_daily_avg, annual_cumret_from_quart_daily_avg, annual_avgret_from_quart, annual_ret
dep_vars <- as.list(c(
  paste0("fgr", delta_fred, "gdpc1"),
  paste0("fgr", delta_fred, "pcecc96"),
  paste0("fgr", delta_fred, "s1gdpc1"),
  paste0("fgr", delta_fred, "s1pcecc96")
))
date_begin <- "1962 Q1"
date_end <- "2022 Q3"

## Construct PCs from financial variables and merge onto variables
match_stata <- FALSE
pca <- get_pc(variables, financial_vars, match_stata = match_stata)
pcs <- if (match_stata) pca$scores else pca$x
pc_vars <- colnames(pcs) <- paste0("pc", seq_len(ncol(pcs)))
pc_qtrs <- tsibble::as_tsibble(variables) |>
  select(all_of(c("qtr", financial_vars))) |>
  tsibble::filter_index(date_begin ~ date_end) |>
  pull(qtr)
pcs <- as_tibble(pcs) |> mutate(qtr = pc_qtrs)
variables <- full_join(variables, pcs, by = "qtr")

## Construct lags of growth rates
num_lags <- 2
lag_vars <- c("gdpc1", "pcecc96")
gr_vars <- paste0("gr1", lag_vars)
variables <- variables |> growth_rate_df(lag_vars, delta = 1)
for (i in 1:num_lags) {
  for (y in gr_vars) {
    lag_y_name <- paste0("l", i, y)
    variables <- variables |> mutate(!!lag_y_name := lag(!!dplyr::sym(y), n = i))
  }
}
lag_gr_vars <-
  stringr::str_extract(dep_vars, paste0(lag_vars, collapse = "|")) |>
  purrr::map(~ paste0("l", 1:num_lags, "gr1", .x))

## Estimate VFCI
results <- dep_vars |>
  purrr::set_names() |>
  purrr::map(\(x) get_vfci(variables, x, pc_vars[1:4]), .progress = TRUE)

results_lags <- seq_along(dep_vars) |>
  purrr::set_names(dep_vars) |>
  purrr::imap(\(i, x) get_vfci(variables, x, c(pc_vars[1:4], lag_gr_vars[[i]])), .progress = TRUE) |>
  purrr::set_names(paste0(dep_vars, "_lags"))

results_exlags <- seq_along(dep_vars) |>
  purrr::set_names(dep_vars) |>
  purrr::imap(\(i, x) get_vfci(variables, x, c(pc_vars[1:4], lag_gr_vars[[i]]), vfci_vars = pc_vars[1:4]), .progress = TRUE) |>
  purrr::set_names(paste0(dep_vars, "_exlags"))

results <- c(results, results_lags, results_exlags)

# conditional vol (vfci)
names_vfci_ts <- purrr::map(names(results), \(x) paste0("vfci_", x))
vfci_ts <- tibble::tibble(
  !!!purrr::map(results, \(x) x$ts$vfci),
  .name_repair = ~ unlist(names_vfci_ts)
) %>%
  tibble::add_column(qtr = results[[1]]$ts$qtr, .before = 1)

# conditional mean
names_mu_ts <- purrr::map(names(results), \(x) paste0("mu_", x))
mu_ts <- tibble::tibble(
  !!!purrr::map(results, \(x) x$ts$mu),
  .name_repair = ~ unlist(names_mu_ts)
) %>%
  tibble::add_column(qtr = results[[1]]$ts$qtr, .before = 1)

# merge
variables <- purrr::reduce(
  list(variables, vfci_ts, mu_ts),
  dplyr::inner_join,
  by = "qtr"
)

# Save ---------------------------------------------------------------------
# save and export
variables$date <- zoo::as.Date(variables$qtr)
variables$yr <- tidyquant::YEAR(variables$date)
variables$quarter <- tidyquant::QUARTER(variables$date)

variables <- variables %>%
  dplyr::relocate(c(
    "date", "yr", "quarter",
    "gdpc1", "gdppot", "pcepilfe", "pcecc96", "fedfunds",
    "dgs10", "tb3ms", "med3", "tb3smffm", "aaa10ym", "wtisplc",
    "baa10ym", "lior3m", "tedrate", "vixcls"
  ))

saveRDS(variables, "./data/vfci_data.rds")
