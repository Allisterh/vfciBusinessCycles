library(data.table)
library(readstata13)

data <- readstata13::read.dta13("./data-raw/gs_fci_q.dta") |> setDT()

data <- data[, .(date = time, gsfci)]

data <- data[!is.na(gsfci)]

data  |> fwrite("./data-raw/gs_fci.csv")
