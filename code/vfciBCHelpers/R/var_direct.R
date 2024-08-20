#' Directly estimate a vars::VAR object using `y` and `x` rather than `y` and some number of lags.
#'
#' Estimates a VAR by OLS per equation.
#'
#' @param y endogenous variables
#' @param x exogenous variables
#' @param type a character specifying if a constant or trend is included in the model.
#' @param ... NOT USED
#'
#' @return a list with class attrribute `vars::varest` holding the following elements:
#'
#' - `varresult`: A list of `lm` objects, one per equation.
#' - `datamat`: The data matrix of endogenous and exogenous variables.
#' - `type`: A character specifying if a constant or trend is included in the model.
#' - `p`: an integer specifying the lag order (set to ncol(x) mod ncol(y)).
#' - `K`: An integer specifying the dimension of the VAR.
#' - `nobs`: An integer specifying the number of observations.
#' - `totobs`: An integer specifying the total number of observations.
#' - `restrictions`: NULL
#' - call: the `call` to `VAR_direct()`
#'
#' @export
var_direct <- function(
  y,
  x,
  type = c("const", "trend", "both", "none"),
  ...
) {

  y <- as.matrix(y)
  x <- as.matrix(x)

  p <- ncol(x) %/% ncol(y)

  if (any(is.na(y))) stop("\nNAs in y.\n")
  if (ncol(y) < 2) stop("
  The matrix 'y' should contain at least two variables.
  For univariate analysis consider ar() and arima() in package stats.\n
  ")
  if (is.null(colnames(y))) {
    colnames(y) <- paste("y", seq_len(ncol(y)), sep = "")
    warning(paste(
      "No column names supplied in y, using:",
      paste(colnames(y), collapse = ", "), ", instead.\n"
    ))
  }

  y_obs <- dim(y)[1]
  x_obs <- dim(x)[1]
  if (y_obs != x_obs) stop("The number of observations in y and x must be the same.")

  k <- dim(y)[2]

  type <- match.arg(type, c("const", "trend", "both", "none"))
  if (type == "none") {
    x <- x
  } else if (type == "const") {
    x <- cbind(x, rep(1, x_obs))
    colnames(x) <- c(colnames(x)[-ncol(x)], "const")
  } else if (type == "trend") {
    x <- cbind(x, seq(1 + p, length.out = x_obs))
    colnames(x) <- c(colnames(x)[-ncol(x)], "trend")
  } else if (type == "both") {
    x <- cbind(x, rep(1, x_obs), seq(1 + p, length.out = x_obs))
    colnames(x) <- c(colnames(x)[-c(ncol(x) - 1, ncol(x))], "const", "trend")
  } else {
    stop("type must be one of 'const', 'trend', 'both', or 'none'")
  }

  datamat <- as.data.frame(x)

  equation <- list()
  for (i in 1:k) {
    y_i <- y[, i]
    equation[[colnames(y)[i]]] <- stats::lm(y_i ~ -1 + ., data = datamat)
    if (any(c("const", "both") %in% type)) {
      attr(equation[[colnames(y)[i]]]$terms, "intercept") <- 1
    }
  }

  result <- list(
    varresult = equation,
    datamat = data.frame(cbind(y, x)),
    y = y,
    type = type,
    p = p,
    K = k,
    obs = y_obs,
    totobs = y_obs,
    restrictions = NULL,
    call = match.call()
  )
  class(result) <- "varest"

  return(result)
}
