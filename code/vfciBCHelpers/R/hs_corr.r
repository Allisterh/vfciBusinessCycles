#' Calculate the correlation between the structural shocks of a list of VARs.
#'
#' @param var_list a list of vars::VAR
#'
#' @return A data.frame
#' @export
#'
#' @import data.table
#'
hs_corr <- function(
  var_list
) {
  ## Defining variables without visible global bindings to make Cmd check happy
  . <- hs_x <- hs_y <- impulse <- var_x <- var_y <- NULL

  if (is.null(names(var_list))) {
    names(var_list) <- paste0("var_", seq_len(length(var_list)))
  }

  hs_dt <- var_list |>
    purrr::map(~ fevdid::hs(.x)$hs) |>
    purrr::list_rbind(names_to = "var") |>
    setDT()

  cross_dt <- merge(
    hs_dt,
    hs_dt,
    by = c("t", "impulse"),
    allow.cartesian = TRUE,
    suffixes = c("_x", "_y")
  )

  corr_dt <-
    cross_dt[, .(
      corr = stats::cor(hs_x, hs_y)
    ), by = .(
      impulse, var_x, var_y
    )]

  return(corr_dt)
}
