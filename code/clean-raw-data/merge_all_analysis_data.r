##
##  Merge all datasets that may be used in
##  VAR analysis into one organized datafile,
##  with each row as one date.
##
library(data.table)
library(dplyr)

## Load the current BCA data
bca_dt <- fread("./data-raw/bca_current_data.csv")

## Load the exogenous VFCI Data and related series
vfci_dt <-
  readRDS("./data/vfci_data.rds") |>
  dplyr::select(c(
    "date",
    dplyr::starts_with(c("vfci", "mu")),
    "pc1",
    "pc2",
    "pc3",
    "pc4",
    "pc5",
    "pc6",
    "gspc_vol",
    "annual_ret",
    "t10y3m",
    "tb3smffm",
    "aaa10ym",
    "baa_aaa"

  )) |>
  as.data.table() |>
  _[, date := as.IDate(date)]

## Load FRED data
fred_dt <- fread("./data-raw/fred.csv")

## Load the BIS credit data
bis_dt <- fread("./data-raw/bis_dp_data.csv")

## Load the EB and GZ data
ebp_dt <- fread("./data-raw/ebp_clean.csv")

## Load the FCI_G data
fci_g_dt <- fread("./data-raw/fci_g.csv")

## Load the MFU data
mfu_dt <- fread("./data-raw/mfu.csv")

## Load the MFU data
gs_dt <- fread("./data-raw/gs_fci.csv")

## Load the EPU data
epu_dt <- fread("./data-raw/epu_clean.csv")
epu_dt <- epu_dt[year(date) >= 1950] ## No reason to have data before 1950 here

## Merge
dt <- bca_dt |>
  merge(vfci_dt, by = "date", all = TRUE) |>
  merge(fred_dt, by = "date", all = TRUE) |>
  merge(bis_dt, by = "date", all = TRUE) |>
  merge(ebp_dt, by = "date", all = TRUE) |>
  merge(fci_g_dt, by = "date", all = TRUE) |>
  merge(mfu_dt, by = "date", all = TRUE) |>
  merge(gs_dt, by = "date", all = TRUE) |>
  merge(epu_dt, by = "date", all = TRUE)

## Save out the data
saveRDS(dt, "./data/all_analysis_data.rds")


#####
library(ggplot2)
library(tidyfast)
dt |>
  dt_pivot_longer(-date) |>
  _[name %in% c(
    "inflation",
    "pcepi",
    "pcepilfe",
    "cpiaucsl",
    "cpilfesl"
  )] |>
  _[, value := scale(value), by = name] |>
  _[!is.na(value)] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_line() +
  facet_wrap(vars(name), ncol = 1)
