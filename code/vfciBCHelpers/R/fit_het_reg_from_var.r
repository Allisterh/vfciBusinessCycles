#' Fit a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param lags integer, number of lags to use in het reg, defaults to 0 (just same period values)
#' @param constant, boolean, default to TRUE to include a constant in estimating the het reg
#' @param hetreg_method, defaults to "twostep", can change to "ML"
#' @param hetreg_horizon defaults to 1, number of periods to calculate the forecast error
#' @param cumsum boolean, defaults to FALSE, if TRUE, cumsum the forecast error
#' @param annualize_factor numeric, defaults to 1, factor to annualize the growth rates
#' @param x2 indepedent variables for predicting heteroskedasticity
#' @param extra_data additional data columns to cbind with the data from the VAR,
#' use to add exogenous variables for x2 that are not in the VAR
#'
#' @return A list of a set of linear regressions
#' @export
#'
#' @import data.table
#'
fit_het_reg_from_var <- function(
  var,
  lags = 0,
  constant = TRUE,
  hetreg_method = "twostep",
  hetreg_horizon = 1,
  cumsum = FALSE,
  annualize_factor = 1,
  x2 = NULL,
  extra_data = NULL
) {
  ## Set visible global binding to make R CMD check happy
  . <- horizon <- log_var_fitted <- t <- variable <- var_lag_variables <- NULL

  ## Get the data
  original_data_wide <-
    get_data_from_var(var) |>
    as.data.table() |>
    _[, t := .I - var$p]

  var_colnames <- colnames(original_data_wide[, -"t"]) |> purrr::set_names()

  original_data <-
    original_data_wide |>
    tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "original")

  original_data_wide <- cbind(original_data_wide, extra_data)

  ## Lagged data matrix
  lagged_data <- copy(original_data_wide)

  for (i in c(var_colnames, x2)){
    lagged_data[, paste0(i, "_L", 1:max(var$p, lags)) := shift(.SD, n = 1:max(var$p, lags)), .SDcols = i]
  }

  var_lag_variables <-
    purrr::map(
      1:var$p,
      ~ paste0(c(var_colnames), "_L", .x)
    ) |>
    unlist()

  if (any(lags != 0)) {

    if (!is.null(x2)) {
      x2 <-
        purrr::map(
          1:var$p,
          ~ paste0(x2, "_L", .x)
        ) |>
        unlist() |>
        c(x2)
    }

    hetreg_lag_variables <-
      purrr::map(lags, ~ if (.x == 0) {
        var_colnames
      } else {
        paste0(var_colnames, "_L", .x)
      }) |>
      unlist()

    extra_data <- cbind(extra_data, lagged_data[, hetreg_lag_variables, with = FALSE])
  }

  ## Create a formula with the needed lags and current variables names
  if (is.null(x2)) {
    x2 <- purrr::map(lags, ~ if (.x == 0) {
      var_colnames
    } else {
      paste0(var_colnames, "_L", .x)
    }) |>
      unlist()
  }


  ## Estimate the het-reg
  het_reg_list <-
    var_colnames |>
    purrr::map(~ hetreg_twostep_var(
      var,
      y = .x,
      horizon = hetreg_horizon,
      cumsum = cumsum,
      x2 = x2,
      extra_data = extra_data
    ))

  ## Get the predicted log variance, fitted, and residual values
  forecasts <-
    1:hetreg_horizon |>
    purrr::set_names() |>
    purrr::map(~ {
      forecast_h <- fevdid::forecast(het_reg_list[[1]]$lm1, .x) |> as.data.table()
      forecast_h_aligned <- shift(forecast_h, n = .x, type = "lead") |> as.data.table()
      colnames(forecast_h_aligned) <- colnames(var$y)
      na.trim(forecast_h_aligned, sides = "right")
    }) |>
    purrr::list_rbind(names_to = "horizon")

  if (cumsum == TRUE) {
    fitted <-
      forecasts |>
      copy() |>
      _[, t := rep(seq_len(nrow(var$y)), hetreg_horizon)] |>
      tidyfast::dt_pivot_longer(-c(t, horizon)) |>
      _[, .(value = sum(value)), by = .(t, name)] |>
      tidyfast::dt_pivot_wider(names_from = name, values_from = value) |>
      _[, t := NULL]
    fitted <- fitted[, colnames(var$y), with = FALSE]
  } else {
    fitted <- forecasts[horizon == hetreg_horizon]
    fitted <- fitted[, colnames(var$y), with = FALSE]
  }

  ## Residuals
  fes <-
    1:hetreg_horizon |>
    purrr::set_names() |>
    purrr::map(~ {
      fe_h <-
        rbind(
          matrix(NA, var$p, var$K),
          fevdid::fe(het_reg_list[[1]]$lm1, .x)
        ) |>
        as.data.table()
      fe_h_aligned <- shift(fe_h, n = .x, type = "lead") |> as.data.table()
      colnames(fe_h_aligned) <- colnames(var$y)
      fe_h_aligned
    }) |>
    purrr::list_rbind(names_to = "horizon")

  if (cumsum == TRUE) {
    residuals <-
      fes |>
      copy() |>
      _[, t := rep(seq_len(nrow(var$y)), hetreg_horizon)] |>
      tidyfast::dt_pivot_longer(-c(t, horizon)) |>
      _[, .(value = sum(value)), by = .(t, name)] |>
      tidyfast::dt_pivot_wider(names_from = name, values_from = value) |>
      _[, t := NULL]
    residuals <- residuals[, colnames(var$y), with = FALSE]
  } else {
    residuals <- fes[horizon == hetreg_horizon]
    residuals <- residuals[, colnames(var$y), with = FALSE]
  }


  predicted_log_variance <-
    var_colnames |>
    purrr::map(
      ~ data.table(
        fitted = fitted[, .x, with = FALSE][[1]],
        residuals = residuals[, .x, with = FALSE][[1]],
        log_var_fitted_resid =
          c(
            rep(NA, pmax(0, var$p, hetreg_horizon + 1)),
            rep(NA, pmax(0, max(lags) - var$p + 1)),
            stats::fitted(het_reg_list[[.x]]$lm2_adj)
          ) |>
          data.table::shift(n = hetreg_horizon, type = "lead"),
        log_var_fitted =
          predict(het_reg_list[[.x]]$lm2_adj, newdata = cbind(extra_data, original_data_wide))
      ) |>
        _[, t := .I - var$p]
    ) |>
    purrr::list_rbind(names_to = "variable")

  #### Annualize the Growth Rates
  if (annualize_factor != 1) {
    if (cumsum == TRUE) {
      predicted_log_variance[, fitted := fitted * (annualize_factor / hetreg_horizon)]
      predicted_log_variance[, residuals := residuals * (annualize_factor / hetreg_horizon)]
      predicted_log_variance[, log_var_fitted_resid := log_var_fitted_resid * sqrt(annualize_factor / hetreg_horizon)]
      predicted_log_variance[, log_var_fitted := log_var_fitted * sqrt(annualize_factor / hetreg_horizon)]
    } else {
      predicted_log_variance[, fitted := fitted * annualize_factor]
      predicted_log_variance[, residuals := residuals * annualize_factor]
      predicted_log_variance[, log_var_fitted_resid := log_var_fitted_resid * annualize_factor]
      predicted_log_variance[, log_var_fitted := log_var_fitted * annualize_factor]
    }
  }



  all_data <-
    original_data |>
    merge(predicted_log_variance, by = c("t", "variable"))

  return(list(
    dt = all_data,
    het_regs = het_reg_list
  ))
}
