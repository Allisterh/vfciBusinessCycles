require(data.table)
require(bcadata)

fred_key <- "7fdf94c38c6355269067736a82bf7874"

data <- pull_bcadata(fred_key)

fwrite(data, "./data-raw/bca_current_data.csv")
