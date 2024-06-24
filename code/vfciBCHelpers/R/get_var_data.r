#' Function to subset all_analysis_data to a data.table with requested variables.
#' Defaults to the ones used in BCA + vfci.
#'
#' @param data_path string, path to data file to read in
#' @param output string, column name to be labelled output
#' @param investment string, column name to be labelled investment
#' @param consumption string, column name to be labelled consumption
#' @param hours_worked string, column name to be labelled hours_worked
#' @param unemployment string, column name to be labelled unemployment
#' @param labor_share string, column name to be labelled labor_share
#' @param interest string, column name to be labelled interest
#' @param inflation string, column name to be labelled inflation
#' @param productivity string, column name to be labelled productivity
#' @param TFP string, columne name to be labelled TFP
#' @param vfci string, column name to be labelled vfci, defaults to vfci_fgr10gdpc1
#' @param start_date date, start date of data
#' @param end_date date, end date of data
#' @param cols string vector of columns to subset, names will be used as new columns names
#' @param add_cols sting vector of columns to include in the subest with the default ones
#'
#' @return data.table
#' @export
#'
#' @import data.table
#'
get_var_data <- function(
  data_path = "./data/all_analysis_data.rds",
  output = "output",
  investment = "investment",
  consumption = "consumption",
  hours_worked = "hours_worked",
  unemployment = "unemployment",
  labor_share = "labor_share",
  interest = "interest",
  inflation = "inflation",
  productivity = "productivity",
  TFP = "TFP",
  vfci = "vfci_fgr10gdpc1",
  start_date = as.Date("1962-01-01"),
  end_date = as.Date("2017-01-01"),
  make_stationary = FALSE,
  diff_cols = c("output", "investment", "consumption", "hours_worked", "productivity", "TFP"),
  cols = NULL,
  add_cols = NULL
) {

  all_data <- readRDS(data_path)

  if (is.null(cols)) {
    cols <- c(
      output = output,
      investment = investment,
      consumption = consumption,
      hours_worked = hours_worked,
      unemployment = unemployment,
      labor_share = labor_share,
      interest = interest,
      inflation = inflation,
      productivity = productivity,
      TFP = TFP,
      vfci = vfci
    )
  }

  if (!is.null(add_cols)) cols <- c(cols, add_cols)

  cols <- c("date", cols)

  selected_data <- all_data[, .SD, .SDcols = cols]

  names <- names(cols)

  if (any(names != "")) {
    data.table::setnames(
      selected_data,
      old = cols[names != ""],
      new = names[names != ""]
    )
  }

  if (make_stationary) {
   selected_data[, (diff_cols) := .SD - data.table::shift(.SD, 1, type = "lag"), .SDcols = diff_cols]
  }

  selected_data <- stats::na.omit(selected_data)

  filtered_data <- selected_data |>
    _[date >= start_date] |>
    _[date <= end_date]

  return(filtered_data)
}
