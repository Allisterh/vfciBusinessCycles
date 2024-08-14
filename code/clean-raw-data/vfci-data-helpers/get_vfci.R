get_vfci <- function(
  data,
  y,
  x,
  het = x,
  prcomp = TRUE,
  n_prcomp = 4,
  date_begin = "1962 Q1",
  date_end = "2022 Q3",
  match_stata = FALSE,
  ...,
  gls_opt = NULL
) {
  call <- NULL

  call <- match.call()

  data <- data |>
    tsibble::as_tsibble() |>
    tsibble::filter_index(date_begin ~ date_end) |>
    tibble::as_tibble()

  if (prcomp) {
    pca <- get_pc(data, x, match_stata = match_stata, ...)
    if (match_stata) {
      pcs <- pca$scores # time series of principal components in princomp
    } else {
      pcs <- pca$x # time series of principal components in princomp
    }
    colnames(pcs) <- paste0("pc", seq_len(ncol(pcs)))
    data_pcs <- dplyr::bind_cols(data, tibble::as_tibble(pcs), .name_repair = c("minimal"))
    x_pcs <- paste0("pc", 1:n_prcomp)
    h <- hetreg(data_pcs, y, x_pcs, ..., gls_opt = gls_opt)
    h$pc <- pca
    h$pc_ts <- data_pcs |> dplyr::select(dplyr::all_of(colnames(pcs)))
  } else {
    h <- hetreg(data, y, x, het, ..., gls_opt = gls_opt)
  }

  het_coefs <- h$modelStruct$varStruct |> unlist()

  if (prcomp) {
    out <- list(
      ts = tibble::tibble(
        qtr = data$qtr,
        vfci_resid = log(attr(h$residuals, "std"))[seq_len(nrow(data))],
        vfci = c(log(h$sigma) + as.matrix(data_pcs[, x_pcs]) %*% het_coefs),
        mu = unname(h$fitted)[seq_len(nrow(data))],
        h$pc_ts
      ),
      hetreg = h,
      call = call
    )
  } else {
    out <- list(
      ts = tibble::tibble(
        qtr = data$qtr,
        vfci_resid = log(attr(h$residuals, "std"))[seq_len(nrow(data))],
        vfci = c(log(h$sigma) +  as.matrix(data[, het]) %*% het_coefs),
        mu = unname(h$fitted)[seq_len(nrow(data))]
      ),
      hetreg = h,
      call = call
    )
  }
  return(out)
}
