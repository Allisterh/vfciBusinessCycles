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
    "nfci",
    "anfci",
    "baa_aaa",
    "tedr",
    "t10y3m"
  )) |>
  as.data.table() |>
  _[, date := as.IDate(date)]

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

## Merge
dt <- bca_dt |>
  merge(vfci_dt, by = "date", all = TRUE) |>
  merge(bis_dt, by = "date", all = TRUE) |>
  merge(ebp_dt, by = "date", all = TRUE) |>
  merge(fci_g_dt, by = "date", all = TRUE) |>
  merge(mfu_dt, by = "date", all = TRUE) |>
  merge(gs_dt, by = "date", all = TRUE)

## Save out the data
saveRDS(dt, "./data/all_analysis_data.rds")


#####
library(ggplot2)
library(tidyfast)
dt |>
  dt_pivot_longer(-date) |>
  _[name %in% c(
    "unemployment",
    "gz",
    "ebp",
    "fci_g",
    "macro_uncert",
    "fin_uncert",
    "nfci",
    "anfci",
    "gsfci",
    "vfci"
  )] |>
  _[!is.na(value)] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", ncol = 1)
