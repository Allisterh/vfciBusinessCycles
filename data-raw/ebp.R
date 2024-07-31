library(data.table)

data <- fread("./data-raw/ebp_csv.csv")

data <- data[month(date) %in% c(3, 6, 9, 12)]

data[, date := as.Date(paste0(year(date), "-", month(date) - 2, "-01"))]

data |>
  setnames(
    old = c("gz_spread", "est_prob"),
    new = c("gz", "rec_est_prob")
  )

fwrite(data, "./data-raw/ebp_clean.csv")
