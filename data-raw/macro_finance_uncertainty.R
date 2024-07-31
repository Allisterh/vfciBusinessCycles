library(data.table)
library(readxl)

macro_uncert <-
  readxl::read_excel(
    "./data-raw/MacroFinanceUncertainty_202402Update/MacroUncertaintyToCirculate.xlsx"
  ) |>
  setDT()

fin_uncert <-
  readxl::read_excel(
    "./data-raw/MacroFinanceUncertainty_202402Update/FinancialUncertaintyToCirculate.xlsx"
  ) |>
  setDT()

real_uncert <-
  readxl::read_excel(
    "./data-raw/MacroFinanceUncertainty_202402Update/RealUncertaintyToCirculate.xlsx"
  ) |>
  setDT()

macro_uncert <- macro_uncert[, .(
  date = as.IDate(Date),
  macro_uncert = `h=1`,
  macro_uncert_h3 = `h=3`,
  macro_uncert_h12 = `h=12`
)]

fin_uncert <- fin_uncert[, .(
  date = as.IDate(Date),
  fin_uncert = `h=1`,
  fin_uncert_h3 = `h=3`,
  fin_uncert_h12 = `h=12`
)]

real_uncert <- real_uncert[, .(
  date = as.IDate(Date),
  real_uncert = `h=1`,
  real_uncert_h3 = `h=3`,
  real_uncert_h12 = `h=12`
)]

data <- macro_uncert |>
  merge(fin_uncert, by = "date", all = TRUE) |>
  merge(real_uncert, by = "date", all = TRUE)

## Keep only end of quarter values
data <- data[month(date) %in% c(3, 6, 9, 12)]

## Relabel date as first day of quarter
data[, date := as.Date(paste0(year(date), "-", month(date) - 2, "-01"))]

data |> fwrite("./data-raw/mfu.csv")
