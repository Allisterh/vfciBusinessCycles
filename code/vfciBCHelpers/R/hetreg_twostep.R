#' Fits a heteroskedastic regression by twostep method
#'
#' @param data dataframe
#' @param y dependent variable
#' @param x1 independent variables for predicting mean
#' @param x2 indepedent varaibles for predicting heteroskedasticity,
#' defaults to x1
#'
#' @return a `hetreg` object
#'
#' @export
hetreg_twostep <- function(
  data,
  y,
  x1,
  x2 = x1
) {

  weights <- NULL

  lnres2 <- "lnres2"

  lm1_formula <- paste0(y, " ~ ", paste0(x1, collapse = " + "))
  lm2_forumla <- paste0(lnres2, "~ ", paste0(x2, collapse = " + "))

  ## Estimate Step 1
  lm1 <- stats::lm(data = data, formula = lm1_formula)

  ## Get the log, squared residuals
  data$residuals <- stats::residuals(lm1)
  data$lnres2 <- log(data$residuals ^ 2)

  ## Estimate Step 2
  lm2 <- stats::lm(data = data, formula = lm2_forumla)

  ## Correct the Intercept of Step 2
  intercept_adjustment <- 1.2704
  intercept <- lm2$coefficients[["(Intercept)"]]

  lm2_adj <- lm2
  lm2_adj$coefficients[["(Intercept)"]] <- intercept + intercept_adjustment

  ## Correct the vcov of Step 2
  z <- cbind(1, lm2$model[, -1]) |> as.matrix()
  vcov_adj <- 4.9328 * solve(t(z) %*% z)

  ## Correct the fitted values of Step 2 as well
  lm2_adj$fitted.values <- lm2$fitted.values + intercept_adjustment
  lm2_adj$residuals <- lm2$fitted.values - intercept_adjustment

  ## Refit Step 1 regression, using predicted lnres2 as the weights
  data$weights <- 1 / exp(lm2_adj$fitted.values)
  lm1_adj <- stats::lm(data = data, formula = lm1_formula, weights = weights)

  hetreg <- list(
    lm1_adj = lm1_adj,
    lm2_adj = lm2_adj,
    vcov_adj = vcov_adj,
    lm1 = lm1,
    lm2 = lm2,
    method = "twostep"
  )

  class(hetreg) <- "hetreg"

  return(hetreg)

}
