#' Wrapper function to estimate VFCI with a variety of options
#'
#' @param data data.frame
#' @param y dependent variable
#' @param x independent variables
#' @param het heteroskedasticity variables
#' @param vfci_vars variables to use to calculate VFCI, can be a subset of het
#' @param date_col date column
#' @param method "ML" or "twostep", whether to use Maximum Likelihood or two step
#' estimation of the heteroskedastic regression.
#' @param ... additional arguments to pass to hetreg
#' @param gls_opt additional arguments to pass to gls
#'
#' @return list of VFCI ts, hetreg model, and call
#' @export
#'
#' @import data.table
#'
get_vfci <- function(
  data,
  y,
  x,
  het = x,
  vfci_vars = het,
  date_col = "date",
  method = "ML",
  ...,
  gls_opt = NULL
) {
  call <- match.call()

  data <- as.data.table(data)

  het_data <- data |>
    _[, c(date_col, het), with = FALSE] |>
    stats::na.omit()

  hetreg_data <- data |>
    _[, c(date_col, y, x, het), with = FALSE] |>
    stats::na.omit()

  if (method == "ML") {
    h <- hetreg(hetreg_data, y, x, het, ..., gls_opt = gls_opt)
  } else if (method == "twostep") {
    h <- hetreg_twostep(hetreg_data, y, x, het, ...)
  } else {
    stop("method must be 'ML' or 'twostep'")
  }

  if (method == "ML") {
    het_coefs <- h$modelStruct$varStruct |> unlist()
  } else if (method == "twostep") {
    ## Adjusted to be a log vol instead of log variance
    het_coefs <- coef(h$lm2_adj)[-1] / 2
  }
  het_coefs <- het_coefs[which(vfci_vars %in% het)]

  if (method == "ML") {
    vfci_from_resid <- log(attr(h$residuals, "std"))
  } else if (method == "twostep") {
    ## Adjusted to be a log vol instead of log variance
    vfci_from_resid <- h$lm2_adj$fitted / 2
  }

  if (method == "ML") {
    residuals <- h$residuals
  } else if (method == "twostep") {
    residuals <- h$lm1_adj$residuals
  }

  if (method == "ML") {
    fitted <- h$fitted
  } else if (method == "twostep") {
    fitted <- h$lm1_adj$fitted
  }

  if (method == "ML") {
    vfci <- c(log(h$sigma) + as.matrix(het_data[, vfci_vars, with = FALSE]) %*% het_coefs)
  } else if (method == "twostep") {
    ## Adjusted to be a log vol instead of log variance
    vfci <- predict(h$lm2_adj, newdata = het_data[, vfci_vars, with = FALSE]) / 2
  }

  ts <- data.table(
    hetreg_data[, date_col, with = FALSE],
    vfci_from_resid = vfci_from_resid,
    residuals = residuals,
    mu = fitted
  )

  ts_x <- data.table(
    het_data[, date_col, with = FALSE],
    vfci = vfci
  )

  ts <- merge(ts, ts_x, by = date_col, all = TRUE)

  out <- list(
    ts = ts,
    hetreg = h,
    call = call
  )

  return(out)
}
