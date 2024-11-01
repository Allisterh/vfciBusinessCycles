#' Wrapper function to estimate VFCI with a variety of options
#'
#' @param data data.frame
#' @param y dependent variable
#' @param x independent variables
#' @param het heteroskedasticity variables
#' @param vfci_vars variables to use to calculate VFCI, can be a subset of het
#' @param date_col date column
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

  h <- hetreg(hetreg_data, y, x, het, ..., gls_opt = gls_opt)

  het_coefs <- h$modelStruct$varStruct |> unlist()
  het_coefs <- het_coefs[which(vfci_vars %in% het)]

  ts <- data.table(
    hetreg_data[, date_col, with = FALSE],
    vfci_resid = log(attr(h$residuals, "std")),
    mu = unname(h$fitted)
  )

  ts_x <- data.table(
    het_data[, date_col, with = FALSE],
    vfci = c(log(h$sigma) + as.matrix(het_data[, vfci_vars, with = FALSE]) %*% het_coefs)
  )

  ts <- merge(ts, ts_x, by = date_col, all = TRUE)

  out <- list(
    ts = ts,
    hetreg = h,
    call = call
  )

  return(out)
}
