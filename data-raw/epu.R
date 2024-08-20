library(data.table)
library(readxl)

cat_epu <- read_excel("./data-raw/epu/Categorical_EPU_Data.xlsx") |> setDT()

## Drop the last row which contains the "source:" info
cat_epu <- cat_epu[-nrow(cat_epu)]

names(cat_epu) <- c(
  "year",
  "month",
  "epu",
  "epu_monetary",
  "epu_fiscal",
  "epu_taxes",
  "epu_govspending",
  "epu_healthcare",
  "epu_natsecurity",
  "epu_entilitlements",
  "epu_regulation",
  "epu_finregulation",
  "epu_trade",
  "epu_sovdebtcurrency"
)

cat_epu[, year := as.numeric(year)]
cat_epu[, date := as.IDate(paste0(year, "-", month, "-01"))]
cat_epu[, c("year", "month") := NULL]

cat_epu <- cat_epu |>
  tidyfast::dt_pivot_longer(-date) |>
  _[, date := as.IDate(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))] |>
  _[, .(value = mean(value)), by = .(date, name)] |>
  tidyfast::dt_pivot_wider(names_from = name, values_from = value)

#####

fin_stress <-
  read_excel("./data-raw/epu/Financial_Stress.xlsx", sheet = "Quarterl Financial Stress") |> setDT()
fin_stress[, date :=  as.IDate(zoo::as.Date(zoo::as.yearqtr(date)))]

setnames(fin_stress, "indicator", "news_fsi")

fin_stress <- fin_stress[, .(date, news_fsi)]

#####

emv <- read_excel("./data-raw/epu/EMV_Data.xlsx") |> setDT()

## Drop the last row which conatins the "source:" info
emv <- emv[-nrow(emv)]

## Only keep some of the columns (there are 40 + options)
emv <- emv[, c(1, 2, 3, 6, 17, 30)]

names(emv)

names(emv) <- c(
  "year",
  "month",
  "emv",
  "emv_macronews",
  "emv_fincrisis",
  "emv_finreg"
)

emv[, date := as.IDate(paste0(year, "-", month, "-01"))]
emv[, c("year", "month") := NULL]

emv <- emv |>
  tidyfast::dt_pivot_longer(-date) |>
  _[, date := as.IDate(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))] |>
  _[, .(value = mean(value)), by = .(date, name)] |>
  tidyfast::dt_pivot_wider(names_from = name, values_from = value)

#####

epu <- cat_epu |>
  merge(fin_stress, by = "date", all = TRUE) |>
  merge(emv, by = "date", all = TRUE)

fwrite(epu, "./data-raw/epu_clean.csv")
