require(data.table)
require(tidyquant)

get_data <- function(
    fred_series = c(
      "GDPC1",
      "GDPPOT",
      "PCEPILFE",
      "PCECC96",
      "FEDFUNDS",
      "ANFCI",
      "NFCI",
      "DGS10",
      "TB3MS",
      "MED3",
      "TB3SMFFM",
      "AAA10YM",
      "WTISPLC",
      "BAA10YM",
      "LIOR3M",
      "TEDRATE",
      "VIXCLS",
      "CLVMNACSCAB1GQEA19",
      "CPIAUCSL",
      "CPILFESL",
      "PCEPI"
    ),
    yahoo_series = c(
      "^GSPC"
    ),
    observation_start = "1950-01-01",
    observation_end = "2025-01-01",
    realtime_start = NULL,
    realtime_end = NULL) {

  fred_raw <- tidyquant::tq_get(
    fred_series,
    get = "economic.data",
    from = observation_start,
    to = observation_end
  )

  yahoo_raw <- tidyquant::tq_get(
    yahoo_series,
    get = "stock.prices",
    from = observation_start,
    to = observation_end
  )

  list("fred" = fred_raw, "yahoo" = yahoo_raw)
}

data <- get_data()
fwrite(data$fred, "./data-raw/vfci_data_fred.csv")
fwrite(data$yahoo, "./data-raw/vfci_data_yahoo.csv")
