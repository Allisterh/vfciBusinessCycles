#' Helper function: compute growth rate of a time-series
growth_rate <- function(
    ts,
    ...,
    delta = 1,
    shift = 0,
    type = c("geometric","log","difference"),
    units = c("percent","decimal"),
    future_growth = c(FALSE,TRUE)
  ) {
  type <- match.arg(type)
  units <- match.arg(units)
  stopifnot(is.logical(future_growth))
  gr <- switch(type,
               geometric = ts / dplyr::lag(ts, delta) - 1,
               log = log(ts) - log(dplyr::lag(ts, delta)),
               difference = ts - dplyr::lag(ts, delta),
  )
  gr <- switch(units,
               decimal = gr,
               percent = 100 * gr
  )
  gr <- if(future_growth) dplyr::lead(gr, delta + shift) else gr
  return(gr)
}

#' Helper function: tidy compute growth rates over many periods of a subset of variables of a data frame
growth_rate_df <- function(data, var_list, delta, ...) {
  gr <- function(data, var_list, delta, ...) {
    args <- list(...)
    if(!("future_growth" %in% names(args))) {
      args$future_growth <- FALSE
    }
    args$shift_boo <- FALSE
    if("shift" %in% names(args)) {
      args$shift_boo <- TRUE
    }
    data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(var_list),
          ~ growth_rate(.x, delta = delta, ...),
          .names = paste0(if(args$future_growth) "f" ,"gr{ delta }", if(args$shift_boo) "s{ args$shift }", "{ .col }"),
          .unpack = TRUE
        ),
        .keep = "none"
      )
  }
  dplyr::bind_cols(
    data,
    delta |> purrr::map(\(x) gr(data, var_list, x, ...))
  )
}
