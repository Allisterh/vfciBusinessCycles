##
##  Clean VFCI data, merge with BCA data
##
require(data.table)

load("./data-raw/variables.Rdata")
setDT(variables)

bca_df <- fread("./data-raw/bca_current_data.csv")

## Select and rename variables to keep
vfci_df <- variables[, .(
    date,
    vfci
    )]

## Merge
df <- merge(bca_df, vfci_df, by = "date", all = TRUE)

## Drop NAs
df <- na.omit(df)

fwrite(df, "./data/vfciBC_data.csv")
