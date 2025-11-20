library(data.table)
library(readxl)
library(ggplot2)

data <- readxl::read_excel("./data-raw/finshocks_public.xlsx", sheet = "daily-level") |> setDT()

data[, date := as.IDate(date)]

data[, yqtr := as.IDate(paste0(year(date), "-", quarter(date) * 3 - 2, "-01"))]
data <-
  data |>
  tidyfast::dt_pivot_longer(-c(date, yqtr)) |>
  _[, .(value = sum(value, na.rm = TRUE)), by = .(yqtr, name)] |>
  tidyfast::dt_pivot_wider(names_from = name, values_from = value)

data |>
  tidyfast::dt_pivot_longer(-yqtr) |>
  ggplot(aes(x = yqtr, y = value, color = name)) +
  geom_line()

setnames(data, "yqtr", "date")

fwrite(data, "./data-raw/finshocks_public.csv")
