require(ggplot2)
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(bcadata)
require(fevdid)

## Pull the correct vintage of the BCA data
bcadata <- fread("./data/bca_replication_data.csv") |>
    filter(date <= as.Date("2017-01-01"))
x <- bcadata[, -"date"]

## Load the original BCA IRFs
bca_irf_df <- fread("./data/bca_original_var_results.csv")

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v <- VAR(x, p = 2, type = "const")

## Identify the shock
mv <- id_fevdfd(v, target = target_var, freqs = bc_freqs)

## Get the IRF
mv_irf <- irf(mv, impulse = "Main", n.ahead = 40)

## Some data wrangling to get a clean dataframe
## with original and replicated IRFs
bca_irf_df <- bca_irf_df |>
    filter(model == "classical_fd") |>
    rename(h = "horizon") |>
    mutate(h = h - 1) |>
    rename(value = "varirf") |>
    mutate(value = value) |>
    mutate(shock = "Main")

mv_irf_df <- mv_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "Replication")

comb_df <- rbindlist(list(mv_irf_df, bca_irf_df), use.names = TRUE, fill = TRUE)

## Save the combined DF out to disk
fwrite(comb_df, "./data/replicated_bca_classical_VAR_IRF.csv")
