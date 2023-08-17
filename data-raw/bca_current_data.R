require(data.table)
require(bcadata)

data <- pull_bcadata("7fdf94c38c6355269067736a82bf7874")

fwrite(data, "./data-raw/bca_current_data.csv")
