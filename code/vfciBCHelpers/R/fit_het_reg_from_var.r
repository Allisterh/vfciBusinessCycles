#' Fit a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param lags integer, number of lags to use in het reg, defaults to 0 (just same period values)
#'
#' @return A list of a set of linear regressions
#' @export
#'
#' @import data.table
#'
fit_het_reg_from_var <- function(
  var,
  lags = 0
) {
  ## Set visible global binding to make R CMD check happy
  log_var <- t <- residuals <- NULL

  ## Get the data
  original_data_wide <-
    get_data_from_var(var) |>
    as.data.table() |>
    _[, t := .I - var$p]

  var_colnames <- colnames(original_data_wide[, -"t"]) |> purrr::set_names()

  original_data <-
    original_data_wide |>
    tidyfast::dt_pivot_longer(-t, names_to = "variable", values_to = "original")

  data[, t := .I - var$p]

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

  ## Create a formula with the needed lags (using "lag(column, 0)" for current values)
  independent_vars <-
    purrr::map_chr(
      0:lags,
      ~paste0(paste0(var_colnames, "_L", .x), collapse = " + ")
    ) |>
    paste0(collapse = " + ")

  reg_data <- data |>
    merge(lagged_data, by = "t", allow.cartesian = TRUE)

  reg_data_list <- purrr::map(var_colnames, ~ stats::na.omit(reg_data[variable == .x]))

  ## Estimate the het-reg
  het_reg_list <-
    reg_data_list |>
    purrr::map(~ lm(
      data = .x,
      formula = paste0("log_var ~ ", independent_vars)
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

  all_data <-
    data |>
    merge(predicted_log_variance, by = c("t", "variable"))

  return(list(
    dt = all_data,
    het_regs = het_reg_list
  ))
}
