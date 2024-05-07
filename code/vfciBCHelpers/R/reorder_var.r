#' Reestimate a VAR with the given data columns first.
#' Can take indexes or column names.
#' Can reorder 1, 2, ..., or all columns.
#'
#' @param var vars::VAR, original var
#' @param new_order column names or indexes, columns to be moved to front
#'
#' @return A vars::VAR object
#' @export
#'
reorder_var <- function(
  var,
  new_order
) {

  data <- get_data_from_var(var)
  lags <- var$p
  type <- var$type

  colnames <- colnames(data)
  indexes <- seq_len(ncol(data))

  if (is.character(new_order)) {
    new_order <- match(new_order, colnames)
  }

  if (length(new_order) != length(colnames)) {
    new_order <- c(new_order, indexes[!indexes %in% new_order])
  }

  reordered_data <- data[, new_order]

  reordered_var <- vars::VAR(reordered_data, p = lags, type = type)

  return(reordered_var)
}
