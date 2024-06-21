#' Print a hetreg object
#' 
#' @param x hetreg object
#' @param ... NOT USED
#'
#' @return NULL
#'
#' @rdname print
#' @name print
#' @aliases print.hetreg
#'
#' @export
print.hetreg <- function(x, ...) {
  print(x$lm1_adj)
  print(x$lm2_adj)
}


#' Print a hetreg waldtest
#' 
#' @param x hetreg_waldtest object
#' @param digits number of digits to display, defaults to 4
#' @param ... NOT USED
#'
#' @return NULL
#'
#' @rdname print
#' @name print
#' @aliases print.hetreg_waldtest
#'
#' @export
print.hetreg_waldtest <- function(x, digits = 4, ...) {
  result <- x$result$chi2

  cat("----- Wald Test -----\n")
  cat("Null Hypothesis: ", x$H0, "\n")
  print(x$coefs[x$terms], quote = FALSE)

  cat("\nChi-squared test:\n")
  cat(
    "X2 = ",
    format(result["chi2"], digits = digits, nsmall = 1),
    ", df = ",
    result["df"],
    ", P(> X2) = ",
    format(result["P"], digits = digits, nsmall = 1), "\n", sep = ""
  )
}


#' Print a hetreg LR test
#'
#' @param x hetreg_lrtest object
#' @param digits number of digits to display, defaults to 4
#' @param ... NOT USED
#'
#' @return NULL
#'
#' @rdname print
#' @name print
#' @aliases print.hetreg_lrtest
#'
#' @export
print.hetreg_lrtest <- function(x, digits = 4, ...) {
  result <- x$result$chi2

  cat("----- LR Test -----\n")

  cat("\nChi-squared test:\n")
  cat(
    "X2 = ",
    format(result["chi2"], digits = digits, nsmall = 1),
    ", df = ",
    result["df"],
    ", P(> X2) = ",
    format(result["P"], digits = digits, nsmall = 1), "\n", sep = ""
  )
}