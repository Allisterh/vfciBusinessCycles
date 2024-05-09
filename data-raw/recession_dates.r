##
## Get NBER recession dates, convert to quarterly frequency
## If any part of the quarter is marked as recession, mark quarter as recession.
##
library(fredr)
library(data.table)

fred_key <- "7fdf94c38c6355269067736a82bf7874"
fredr::fredr_set_key(fred_key)

recessions <-
  fredr("USRECD", frequency = "q", aggregation_method = "sum") |>
  as.data.table() |>
  _[value != 0, value := 1]

recessions |>
  _[, .(date, recession = value)] |>
  na.omit() |>
  fwrite("./data-raw/recession_dates.csv")
