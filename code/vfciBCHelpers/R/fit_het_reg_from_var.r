#' Fit a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param lags integer, number of lags to use in het reg, defaults to 0 (just same period values)
#' @param constant, boolean, default to TRUE to include a constant in estimating the het reg
#' @param use_hetreg_func, boolean, default to TRUE to use hetreg function
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
  use_hetreg_func = TRUE
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

  if (use_hetreg_func) {
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
        na.omit(lagged_data),
        .x,
        var_lag_variables,
        het = het_independent_variables,
        method = "ML"
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

  if (!use_hetreg_func) {
    resid_data <-
      stats::residuals(var) |>
      as.data.table() |>
      _[, t := .I] |>
      tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "residual")

    fitted_data <-
      stats::fitted(var) |>
      as.data.table() |>
      _[, t := .I] |>
      tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "fitted")

    data <- original_data |>
      merge(resid_data, by = c("t", "variable")) |>
      merge(fitted_data, by = c("t", "variable"))

    ## Construct the log variance
    data[, log_var := log(residual ^ 2)]

    ## Lagged data matrix
    lagged_data <- copy(original_data_wide)

    for (i in var_colnames){
      lagged_data[, paste0(i, "_L", 0:lags) := shift(.SD, n = 0:lags), .SDcols = i]
    }

    reg_data <- data |>
      merge(lagged_data, by = "t", allow.cartesian = TRUE)

    reg_data_list <- purrr::map(var_colnames, ~ stats::na.omit(reg_data[variable == .x]))

    ## Estimate the het-reg
    het_reg_list <-
      reg_data_list |>
      purrr::map(~ lm(
        data = .x,
        formula = paste0("log_var ~ ", ifelse(constant, "", "0 + "), paste0(het_independent_variables, collapse = " + "))
      ))

    ## Get the predicted log variance values
    predicted_log_variance <-
      het_reg_list |>
      purrr::map(
        ~ stats::fitted(.x) |>
          as.data.table() |>
          setnames("V1", "log_var_fitted") |>
          _[, t := .I - lags] |>
          _[, log_var_residual := stats::residuals(.x)]
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
