#' Nicely summarize a hetreg object
#'
#' @param object hetreg object
#' @param ... NOT USED
#'
#' @return NULL
#'
#' @rdname summary
#' @name summary
#' @aliases summary.hetreg
#'
#' @export
summary.hetreg <- function(
  object,
  ...
) {
  print(summary(object$lm1_adj))
  print(summary(object$lm2_adj))
  print(hetreg_waldtest(object))
}
