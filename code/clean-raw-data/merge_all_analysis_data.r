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
    "baa_aaa",
    "tedr",
    "t10y3m"
  )) |>
  as.data.table() |>
  _[, date := as.IDate(date)]

## Merge
dt <- merge(bca_dt, vfci_dt, by = "date", all = TRUE)

## Save out the data
saveRDS(dt, "./data/all_analysis_data.rds")


#####
library(ggplot2)
library(tidyfast)
dt |>
  dt_pivot_longer(-date) |>
  _[name %in% c("unemployment", "interest", "baa_aaa", "tedr", "t10y3m", "nfci", "vfci")] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", ncol = 1)

