#' Convert recession_dates.csv into a data.table of start and end dates.
#'
#' @param path filepath to the recession_dates.csv
#'
#' @return data.table of recession start and end dates
#' @export
#'
#' @import data.table
#'
get_recession_dt <- function(
  path = "./data-raw/recession_dates.csv"
) {
  recession <- start <- end <- NULL

  recessions <- fread(path)

  recessions[, start := fifelse(recession == 1 & shift(recession, 1, type = "lag") == 0, 1, 0)]
  recessions[, end := fifelse(recession == 1 & shift(recession, 1, type = "lead") == 0, 1, 0)]

  recession_dt <- data.table(
    start = recessions[start == 1, date],
    end = recessions[end == 1, date]
  )

  return(recession_dt)
}
