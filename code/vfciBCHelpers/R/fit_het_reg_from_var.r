#' Fit a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param lags integer, number of lags to use in het reg, defaults to 0 (just same period values)
#' @param constant, boolean, default to TRUE to include a constant in estimating the het reg
#' @param hetreg_method, defaults to "twostep", can change to "ML"
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
  hetreg_method = "twostep"
) {
  ## Set visible global binding to make R CMD check happy
  log_var <- log_var_fitted <- std <- t <- residual <- NULL

  ## Get the data
  original_data_wide <-
    get_data_from_var(var) |>
    as.data.table() |>
    _[, t := .I - var$p]

  var_colnames <- colnames(original_data_wide[, -"t"]) |> purrr::set_names()

  original_data <-
    original_data_wide |>
    tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "original")

  ## Lagged data matrix
  lagged_data <- copy(original_data_wide)

  for (i in var_colnames){
    lagged_data[, paste0(i, "_L", 1:max(var$p, lags)) := shift(.SD, n = 1:max(var$p, lags)), .SDcols = i]
  }

  ## Create a formula with the needed lags and current variables names
  het_independent_variables <-
    purrr::map(lags, ~ if (.x == 0) {var_colnames} else {paste0(var_colnames, "_L", .x)}) |>
    unlist()

  if (hetreg_method == "ML") {
    var_lag_variables <-
      purrr::map(
        1:var$p,
        ~ paste0(var_colnames, "_L", .x)
      ) |>
      unlist()

    ## Estimate the het-reg
    het_reg_list <-
      var_colnames |>
      purrr::map(~ hetreg(
        data = na.omit(lagged_data),
        .x,
        var_lag_variables,
        het = het_independent_variables,
        method = "REML"
      ))

    ## Get the predicted log variance values
    predicted_log_variance <-
      het_reg_list |>
      purrr::map(
        ~ data.table(
          fitted = stats::fitted(.x),
          residuals = stats::residuals(.x),
          std = attr(stats::residuals(.x), "std")
        ) |>
          _[, t := .I]
      ) |>
      purrr::list_rbind(names_to = "variable") |>
      copy() |>
      _[, log_var_fitted := 2 * log(std)]
  }

  if (hetreg_method == "twostep") {
    var_lag_variables <-
      purrr::map(
        1:var$p,
        ~ paste0(var_colnames, "_L", .x)
      ) |>
      unlist()

    ## Estimate the het-reg
    het_reg_list <-
      var_colnames |>
      purrr::map(~ hetreg_twostep(
        data = na.omit(lagged_data),
        y = .x,
        x1 = var_lag_variables,
        x2 = het_independent_variables
      ))

    ## Get the predicted log variance values
    predicted_log_variance <-
      het_reg_list |>
      purrr::map(
        ~ data.table(
          fitted = stats::fitted(.x$lm1_adj),
          residuals = stats::residuals(.x$lm1_adj),
          log_var_fitted = stats::fitted(.x$lm2_adj)
        ) |>
          _[, t := .I]
      ) |>
      purrr::list_rbind(names_to = "variable")
  }

  all_data <-
    original_data |>
    merge(predicted_log_variance, by = c("t", "variable"))

  return(list(
    dt = all_data,
    het_regs = het_reg_list
  ))
}
