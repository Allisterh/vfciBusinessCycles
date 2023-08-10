require(data.table)
require(bcadata)
require(fevdid)
require(BVAR)
require(BVARverse)

## Pull the correct vintage of the BCA data
bcadata <- fread("./data/bca_replication_data.csv")[
    date <= as.Date("2017-10-01"), ]
x <- bcadata[, -"date"]

## Load the original BCA IRFs
bca_irf_df <- fread("./data/bca_original_var_results.csv")

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
#bv <- bvar(x, lags = 2, priors = bv_priors(hyper = "full"))
bv <- bvar(
    x,
    lags = 2,
    priors = bv_priors(
        hyper = c("lambda", "alpha", "psi"),
        mn = bv_mn(
            b = 1,
            lambda = bv_lambda(mode = 0.2),
            alpha = bv_alpha(mode = 4),
            psi = bv_psi(mode = rep(4, 10)),
            var = 10^10)
        ),
    n_draw = 50000L,
    n_burn = 49000L,
    n_thin = 1L
    )

## Identify the shock
mbv <- id_fevdfd(bv, target = target_var, freqs = bc_freqs)
mbvtd4 <- id_fevdtd(bv, target = target_var, horizon = 4)
mbvtd32 <- id_fevdtd(bv, target = target_var, horizon = 32) ## 6:32


## Get IRFs
irf(mbv) <- irf(mbv, bv_irf(horizon = 40L, identification = FALSE))
irf_df <- tidy(irf(mbv, conf_bands = c(0.16))) |> setDT()

irf(mbvtd4) <- irf(mbvtd4, bv_irf(horizon = 40L, identification = FALSE))
irf_dftd4 <- tidy(irf(mbvtd4, conf_bands = c(0.16))) |> setDT()

irf(mbvtd32) <- irf(mbvtd32, bv_irf(horizon = 40L, identification = FALSE))
irf_dftd32 <- tidy(irf(mbvtd32, conf_bands = c(0.16))) |> setDT()


## Tidy datasets
irf_df <-
    irf_df[impulse == "output", .(
        response,
        horizon = time,
        value,
        quantile,
        model = "Replication, Frequency Domain"
    )] |>
    dcast(response + horizon + model ~ quantile) |>
    setnames(
        old = c("response", "16", "50", "84"),
        new = c("variable", "lower", "median", "upper")
    )


irf_dftd4 <-
    irf_dftd4[impulse == "output", .(
        response,
        horizon = time,
        value,
        quantile,
        model = "Replication, Time Domain, Horizon 4"
    )] |>
    dcast(response + horizon + model ~ quantile) |>
    setnames(
        old = c("response", "16", "50", "84"),
        new = c("variable", "lower", "median", "upper")
    )

irf_dftd32 <-
    irf_dftd32[impulse == "output", .(
        response,
        horizon = time,
        value,
        quantile,
        model = "Replication, Time Domain, Horizon 32"
    )] |>
    dcast(response + horizon + model ~ quantile) |>
    setnames(
        old = c("response", "16", "50", "84"),
        new = c("variable", "lower", "median", "upper")
    )

bca_irf_df <- bca_irf_df[, .(
    variable,
    horizon,
    lower = pctl_16,
    median,
    upper = pctl_84,
    model
    )]

## Bind datasets
df <- rbindlist(list(
    bca_irf_df,
    irf_df,
    irf_dftd4,
    irf_dftd32
    ), use.names = TRUE)

## Save it out to disk
fwrite(df, "./data/replicated_bca_bayesian_VAR_IRF.csv")

