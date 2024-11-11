#' Fits a heteroskedastic regression by twostep method
#'
#' @param var VAR object
#' @param y dependent variable
#' @param x2 indepedent varaibles for predicting heteroskedasticity,
#' defaults to all variables in VAR
#' @param horizon forecast error horizon for residuals
#' @param extra_data additional data columns to cbind with the data from the VAR,
#' use to add exogenous variables for x2 that are not in the VAR
#'
#' @return a `hetreg` object
#'
#' @export
hetreg_twostep_var <- function(
  var,
  y,
  x2 = NULL,
  horizon = 1,
  extra_data = NULL
) {

  data <- get_data_from_var(var)

  if (!is.null(extra_data)) {
    data <- cbind(data, extra_data)
  }

  if (is.null(x2)) {
    x2 <- names(data)
  } else if (!all(x2 %in% names(data))) {
    stop("Please pass valid variables in the VAR for x2.")
  }

  lnres2 <- "lnres2"

  lm2_forumla <- paste0(lnres2, " ~ ", paste0(x2, collapse = " + "))

  ## Get the log, squared residuals
  y_loc <- grep(y, colnames(var$y))
  data$fe <-
    c(rep(NA, var$p), fevdid::fe(var, horizon)[, y_loc]) |>
    data.table::shift(n = horizon, type = "lead")
  data$lnres2 <- log(data$fe ^ 2)

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

  hetreg <- list(
    lm1_adj = NA,
    lm2_adj = lm2_adj,
    vcov_adj = vcov_adj,
    lm1 = var,
    lm2 = lm2,
    method = "twostep_var"
  )

  class(hetreg) <- "hetreg"

  return(hetreg)

}
