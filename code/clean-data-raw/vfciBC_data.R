##
##  Clean VFCI data, merge with BCA data
##
require(data.table)
require(dplyr)
require(tibble)

variables <- readRDS("./data/vfci_variables.RDS") |>
  setDT(variables)

bca_df <- fread("./data-raw/bca_current_data.csv")

## Select and rename variables to keep
vfci_df <- variables %>%
  dplyr::select(dplyr::starts_with(c("vfci", "mu"))) %>%
  tibble::add_column(date = as.IDate(variables$date))

## Merge
df <- merge(bca_df, vfci_df, by = "date", all = TRUE)

## Drop NAs
df <- na.omit(df, cols = c("vfci"))

fwrite(df, "./data/vfciBC_data.csv")
