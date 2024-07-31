library(data.table)

data <- fread("./data-raw/bis_dp_search_export_20240731-134219.csv", skip = 3)

data <-
  data[, .(
    date = `TIME_PERIOD:Period`,
    series = `KEY:Timeseries Key`,
    value = `OBS_VALUE:Value`
  )]

data[series == "Q.US.P.A.M.770.A", series := "bis_credit_all"]
data[series == "Q.US.P.B.M.770.A", series := "bis_credit_banks"]

data <- data |> tidyfast::dt_pivot_wider(names_from = series, values_from = value)

data[, date := as.Date(paste0(year(date), "-", month(date) - 2, "-01"))]

fwrite(data, "./data-raw/bis_dp_data.csv")
