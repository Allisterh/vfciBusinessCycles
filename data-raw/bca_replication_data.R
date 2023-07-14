require(data.table)
require(bcadata)

data <- pull_bcadata("7fdf94c38c6355269067736a82bf7874", replicate = TRUE)

fwrite(data, "./data-raw/bca_replication_data.csv")