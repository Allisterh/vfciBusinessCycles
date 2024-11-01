#' Fit a VAR to given data with given lags. Ignores a column called "date" and includes a constant.
#'
#' @param data data.frame, with a date column
#' @param lags integer, number of lags to use in VAR
#' @param y_lead integer, number of periods to lead the dependent variable for a direct VAR model
#' @param date_col string, defaults to "date", name of the date column
#' @param type character, "const" (default) includes a constant in the VAR estimation
#'
#' @return A vars::VAR object
#' @export
#'
#' @import data.table
#'
fit_var <- function(
  data,
  lags,
  y_lead = 0,
  date_col = "date",
  type = "const"
) {

  data <- copy(data)

  ## Fit the VAR
  if (y_lead == 0) {

    var <- vars::VAR(data[, -date_col, with = FALSE], p = lags, type = type)

  } else if (is.numeric(y_lead)) {

    cols <- colnames(data)[colnames(data) != date_col]

    lag_names <- c()
    for (i in 1:lags) {
      lag_names_i <- paste0(cols, "_lag", i)
      data[, (lag_names_i) := lapply(.SD, shift, i, type = "lag"), .SDcols = cols]
      lag_names <- c(lag_names, lag_names_i)
    }

    lead_names <- paste0(cols, "_lead", y_lead)
    data[, (lead_names) := lapply(.SD, shift, y_lead, type = "lead"), .SDcols = cols]

    data <- stats::na.omit(data[, c(lag_names, lead_names), with = FALSE])

    x <- data[, lag_names, with = FALSE]
    y <- data[, lead_names, with = FALSE]

    var <- var_direct(y, x, type = type)

  }

  return(var)
}
