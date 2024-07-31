library(data.table)

data <- fread("./data-raw/fci_g_public_quarterly_3yr.csv")

data <- data[, .(date, fci_g = `FCI-G Index (baseline)`)]

data[, date := as.Date(paste0(year(date), "-", month(date) - 2, "-01"))]

data |> fwrite("./data-raw/fci_g.csv")
