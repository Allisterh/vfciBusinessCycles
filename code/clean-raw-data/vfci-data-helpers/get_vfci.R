get_vfci <- function(
  data,
  y,
  x,
  het = x,
  vfci_vars = het,
  ...,
  gls_opt = NULL
) {
  call <- match.call()

  het_data <- data |>
    dplyr::select(dplyr::all_of(c("qtr", het))) |>
    na.omit()

  hetreg_data <- data |>
    dplyr::select(dplyr::all_of(c("qtr", y, x, het))) |>
    na.omit()

  h <- hetreg(hetreg_data, y, x, het, ..., gls_opt = gls_opt)

  het_coefs <- h$modelStruct$varStruct |> unlist()
  het_coefs <- het_coefs[which(vfci_vars %in% het)]

  ts <- tibble::tibble(
    hetreg_data[, c("qtr", y)],
    vfci_resid = log(attr(h$residuals, "std")),
    mu = unname(h$fitted)
  )

  ts_x <- tibble::tibble(
    het_data[, c("qtr", unique(c(vfci_vars, het, x)))],
    vfci = c(log(h$sigma) + as.matrix(het_data[, vfci_vars]) %*% het_coefs)
  )

  ts <- dplyr::full_join(ts, ts_x, by = "qtr")

  out <- list(
    ts = ts,
    hetreg = h,
    call = call
  )

  return(out)
}
