library(data.table)
library(fredr)

fredr_set_key("7fdf94c38c6355269067736a82bf7874")

fred_series <- c(
  "GDPC1",
  "PCECC96",
  "ANFCI",
  "NFCI",
  "TEDRATE",
  "PCEPILFE",
  "CPIAUCSL",
  "CPILFESL",
  "PCEPI"
)

fred <-
  purrr::map(
    fred_series,
    fredr::fredr,
    frequency = "q"
  ) |>
  purrr::list_rbind() |>
  setDT() |>
  _[, .(date, name = series_id, value)]

fred_wide <-
  fred |>
  _[, name := tolower(name)] |>
  tidyfast::dt_pivot_wider(names_from = name, values_from = value)

fred_wide |>
  _[, pcepi := 100 * log(pcepi / shift(pcepi))] |>
  _[, pcepilfe := 100 * log(pcepilfe / shift(pcepilfe))] |>
  _[, cpiaucsl := 100 * log(cpiaucsl / shift(cpiaucsl))] |>
  _[, cpilfesl := 100 * log(cpilfesl / shift(cpilfesl))]

fwrite(fred_wide, "./data-raw/fred.csv")
