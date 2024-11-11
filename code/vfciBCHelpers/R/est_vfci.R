#' Wrapper function to estimate VFCI with a variety of options
#'
#' @param y dependent variable, assumed to be given in log levels,
#' will take forward differences
#' @param x independent variables, usually the financial principal components,
#' x = c("pc1", "pc2", "pc3", "pc4")
#' @param het defaults to the same as x, but can be a subset of columns or other columns
#' @param vfci_vars defaults to the same as het, but can be a subset of columns or other columns
#' @param forward how many periods to take forward differences of y
#' @param lags how many lags to include in the independent variables,
#' defaults to 0, which is always ignored. Note that lags are labeled one
#' less than the value passed, because the dependent variable is forwarded by
#' at least 1.
#' @param date_col which column in the data.table identifies the 'date' or time domain
#' @param cols_to_lag which columns to lags of, defaults to just 'y'
#' @param exlags_het boolean, should lags be included in the vfci calculation or excluded
#' @param exlags_vfci boolean, should lags be included in the vfci calculation or excluded
#' @param date_begin start date for the data
#' @param date_end end date for the data
#' @param include_all boolean, whether to return all the get_vfci() data
#' (ie model estimation) or just the time series data
#' @param data A data.table object containing data for the VFCI estimation,
#' defaults to vfci_data.csv file
#'
#' @return data.table object with 'date', 'vfci', and 'mu' columns
#' @export
#'
#' @import data.table
#'
est_vfci <- function(
  y,
  x,
  het = NULL,
  vfci_vars = NULL,
  date_col = "date",
  forward = 1,
  lags = 0,
  lag_difference = 1,
  cols_to_lag = y,
  exlags_het = FALSE,
  exlags_vfci = FALSE,
  date_begin = "1962-01-01",
  date_end = "2022-07-01",
  include_all = FALSE,
  data = fread("./data/vfci_data.csv")
) {

  if (is.null(het)) het <- x
  if (is.null(vfci_vars)) vfci_vars <- het

  data <- data[, unique(c(date_col, y, x, het)), with = FALSE]

  ## Calculate Forward Growth Rates
  y_growth <- paste0(y, "_gr", forward)
  y_forward <- paste0(y, "_fgr", forward)
  data[, (y_growth) := get(y) - shift(get(y), n = forward, type = "lag")]
  data[, (y_forward) := shift(get(y_growth), n = forward, type = "lead")]

  ## Calculate lags
  if (any(lags != 0)) {
    y_growth <- paste0(y, "_gr", lag_difference)
    data[, (y_growth) := get(y) - shift(get(y), n = lag_difference, type = "lag")]

    if (y %in% cols_to_lag) cols_to_lag[cols_to_lag == y] <- y_growth
    lag_cols <- purrr::map(cols_to_lag, ~ paste0(.x, "_lag", (lags[lags != 0] - 1))) |> unlist()
    data[, (lag_cols) := shift(.SD, n = (lags[lags != 0] - 1), type = "lag"), .SDcols = cols_to_lag]

    x <- c(x, lag_cols)
    if (!exlags_het) het <- c(het, lag_cols)
    if (!exlags_vfci && !exlags_het) vfci_vars <- c(vfci_vars, lag_cols)
  }

  ## Trim to dates
  data <- data |>
    _[date %between% c(date_begin, date_end)]

  vfci <- get_vfci(
    data,
    y_forward,
    x,
    het,
    vfci_vars,
    date_col
  )

  if (include_all == TRUE) {
    return(vfci)
  } else {
    return(vfci$ts[, c(date_col, "vfci", "mu", "residuals"), with = FALSE])
  }

}
