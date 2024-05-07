#' Fit a VAR to given data with given lags. Ignores a column called "date" and includes a constant.
#'
#' @param data data.frame, with a date column
#' @param lags integer, number of lags to use in VAR
#' @param date_col string, defaults to "date", name of the date column
#' @param include_const boolean, TRUE (default) includes a constant in the VAR estimation
#'
#' @return A vars::VAR object
#' @export
#'
#' @import data.table
#'
fit_var <- function(
  data,
  lags,
  date_col = "date",
  include_const = TRUE
) {

  ## Fit the VAR
  if (include_const == TRUE) {
    var <- vars::VAR(data[, -date_col, with = FALSE], p = lags, type = "const")
  } else {
    var <- vars::VAR(data[, -date_col, with = FALSE], p = lags)
  }

  return(var)
}
