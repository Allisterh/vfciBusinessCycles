#' Function to calculate waldtest for hetreg object to test that all
#' coefficients in the heteroskedastic regression are jointly non-zero.
#' Code based on https://github.com/cran/aod/blob/master/R/stat.test.R
#'
#' @param hetreg a hetreg class object, method must be "twostep"
#'
#' @return a `hetreg_waldtest` object
#'
#' @export
hetreg_waldtest <- function(
  hetreg
) {

  if (!hetreg$method %in% c("twostep", "twostep_var")) {
    stop("Wald Test only used for two step hetreg.
          Use the LR test for ML method.")
  }

  vcov <- hetreg$vcov_adj
  coefs <- stats::coef(hetreg$lm2_adj)
  terms <- which(names(coefs) != "(Intercept)")
  w <- length(terms)
  h0 <- rep(0, w)

  lmat <- matrix(rep(0, length(coefs) * w), ncol = length(coefs))
  for (i in 1:w) {
    lmat[i, terms[i]] <- 1
  }

  dimnames(lmat) <-
    list(paste("L", as.character(seq_len(NROW(lmat))), sep = ""), names(coefs))

  ## Calculate Wald Test
  f <- lmat %*% coefs
  mat <- qr.solve(lmat %*% vcov %*% t(lmat))
  stat <- t(f - h0) %*% mat %*% (f - h0)
  p <- 1 - stats::pchisq(stat, df = w)

  res <- list(chi2 = c(chi2 = stat, df = w, P = p))

  ## Store Result
  waldtest <- list(
    result = res,
    vcov = vcov,
    coefs = coefs,
    terms = terms,
    H0 = h0,
    L = lmat
  )
  class(waldtest) <- "hetreg_waldtest"

  return(waldtest)
}
