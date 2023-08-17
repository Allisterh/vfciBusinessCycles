require(data.table)
require(dplyr)
require(vars)
require(svars)
require(bcadata)
require(fevdid)

## Pull the correct vintage of the BCA data
bcadata <- fread("./data/bca_current_data.csv")
x <- bcadata[, -"date"]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v <- VAR(x, p = 2, type = "const")

## Identify the shock
mv <- id_fevdfd(v, target = target_var, freqs = bc_freqs)

## Get the IRF
mv_irf <- vars::irf(mv, impulse = "Main", n.ahead = 40)

## Bootstraps
boot_df_resample_ba <-
    bootstrap(
        mv,
        id_fevdfd,
        nboot = 1000,
        n_ahead = 40,
        method = "resample", design = "recursive", bias_adjust = TRUE,
        target = "unemployment", freqs = bc_freqs
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "classical_fd") |>
    mutate(version = "current")

## Some data wrangling to get a clean dataframe
## with original and replicated IRFs
mv_irf_df <- mv_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    rename(varirf = "value")

comb_df <-
    merge(boot_df_resample_ba, mv_irf_df, by = c("h", "variable", "shock"))

fwrite(comb_df, "./data/current_bca_classical_VAR_IRF_boot.csv")
