#' Function to calculate liklihood ratio test for hetreg
#'
#' @param hetreg a hetreg class object, method must be from `hetreg_gls`
#'
#' @return a `hetreg_waldtest` object
#'
#' @export
hetreg_lrtest <- function(
  hetreg
) {

  df <- hetreg$dims$p - 1
  log_like_het <- hetreg$logLik

  ## Fit GLS reg without heteroskedasticity
  data <- nlme::getData(hetreg)
  y <- all.vars(hetreg$terms)[1]
  x1 <- names(hetreg$coefficients)
  x1 <- x1[x1 != "(Intercept)"]
  frm <- stats::formula(paste0(y, " ~ ", paste0(x1, collapse = " + ")))

  reg <- nlme::gls(frm, data = data, method = hetreg$method)

  log_like_reg <- reg$logLik

  ## Difference between log liklihood of het model vs. homeskdastic model
  stat <- 2 * (log_like_het - log_like_reg) |> as.numeric()

  p <- 1 - stats::pchisq(stat, df = df)

  res <- list(chi2 = c(chi2 = stat, df = df, P = p))

  ## Store Result
  lrtest <- list(
    result = res,
    logLik_het = log_like_het,
    logLik_reg = log_like_reg
  )
  class(lrtest) <- "hetreg_lrtest"

  return(lrtest)
}
