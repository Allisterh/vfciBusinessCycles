#functions to collapse from daily to quarterly
aggregation_funs <- function(x) {
  dplyr::tibble(
    avg = na_if(mean(x, na.rm = TRUE), NaN),
    end_of_period = last(x, na_rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}
#functions to go from quarterly to annual
mean_4q <- timetk::slidify(mean, .period = 4, .align = "right")
cumret_4q <- timetk::slidify(\(x) purrr::reduce(x, \(cumret, ret) cumret * (1 + ret / 4), .init = 1), .period = 4, .align = "right")
cumsum_4q <- timetk::slidify(\(x) purrr::reduce(x, \(cumsum, ret) cumsum + ret / 4), .period = 4, .align = "right")
