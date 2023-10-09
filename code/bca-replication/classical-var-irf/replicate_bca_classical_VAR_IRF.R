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
bca_irf_df <- fread("./data/bca_original_var_results.csv")[
    model == "classical_fd"][,
    version := "Original BCA"
    ]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v <- VAR(x, p = 2, type = "const")

## Identify the shock
mv <- id_fevdfd(v, target = target_var, freqs = bc_freqs)

## Get the IRF
mv_irf_df <- irf(mv, impulse = "Main", n.ahead = 40)$irf |>
    as_tibble() |>
    mutate(
        model = "classical_fd",
        version = "Replication"
        )

## Combine the two datasets, drop other models
comb_df <- rbindlist(list(
    mv_irf_df,
    bca_irf_df
    ), use.names = TRUE, fill = TRUE)

comb_df <- comb_df[, .(h, impulse, response, irf, model, version)]

## Save the combined DF out to disk
fwrite(comb_df, "./data/replicated_bca_classical_VAR_IRF.csv")
