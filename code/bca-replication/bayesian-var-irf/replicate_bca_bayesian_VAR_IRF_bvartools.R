##
##  Uses the package `bvartools` to create a Bayesian VAR
##  Then identify the main shock with the `fevdid` package
##
require(data.table)
require(bcadata)
require(bvartools)
require(vars)
require(fevdid)

## sources `estimate_bvartools`
source("./code/helpers/estimate_bvartools.R")

## Business cycle frequency
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
tv <- "unemployment"

## Load data
bcadata <- fread("./data/bca_replication_data.csv")[
    date <= as.Date("2017-10-01"), ]
data <- ts(bcadata[, -"date"], start = year(bcadata[[1, 1]]), frequency = 4)

## Read in original variance priors
origional_prior <- fread("./data/isigma.csv") |> as.matrix()
## bvartools uses a different order
new_order <- unlist(lapply(1:21, function(i) seq(i, 210, by = 21)))
origional_prior <- diag(diag(origional_prior)[new_order])

## Read in original IRF df (for comparison later)
bca_irf_df <- fread("./data/bca_original_var_results.csv")[, .(
        model,
        variable,
        h = horizon,
        median,
        lower = pctl_16,
        upper = pctl_84,
        version = "original"
        )]

## Fit frequentist VAR
v <- bvartools::gen_var(data, p = 2, deterministic = "const")

## Fit Small Replication
bv_small_rep <- estimate_bvartools(v, burnin = 1000)

mbv_small_rep_fd <- id_fevdfd(bv_small_rep, tv, bc_freqs, 1000)
mbv_small_rep_td4 <- id_fevdtd(bv_small_rep, tv, 4)
mbv_small_rep_td32 <- id_fevdtd(bv_small_rep, tv, 32)
mbv_small_rep_td632 <- id_fevdtd(bv_small_rep, tv, 6:32)

irf_df_small_rep_fd <- vars::irf(mbv_small_rep_fd, n.ahead = 40) |> setDT()
irf_df_small_rep_td4 <- vars::irf(mbv_small_rep_td4, n.ahead = 40) |> setDT()
irf_df_small_rep_td32 <- vars::irf(mbv_small_rep_td32, n.ahead = 40) |> setDT()
irf_df_small_rep_td632 <- vars::irf(mbv_small_rep_td632, n.ahead = 40) |> setDT()

irf_df_small_rep_fd[, version := "replication"][, model := "bayesian_fd_small"]
irf_df_small_rep_td4[, version := "replication"][, model := "bayesian_td4_small"]
irf_df_small_rep_td32[, version := "replication"][, model := "bayesian_td32_small"]
irf_df_small_rep_td632[, version := "replication"][, model := "bayesian_td632_small"]


## Fit Replication
bv_rep <- estimate_bvartools(v, burnin = 49000)

mbv_rep_fd <- id_fevdfd(bv_rep, tv, bc_freqs, 1000)
mbv_rep_td4 <- id_fevdtd(bv_rep, tv, 4)
mbv_rep_td32 <- id_fevdtd(bv_rep, tv, 32)
mbv_rep_td632 <- id_fevdtd(bv_rep, tv, 6:32)

irf_df_rep_fd <- vars::irf(mbv_rep_fd, n.ahead = 40) |> setDT()
irf_df_rep_td4 <- vars::irf(mbv_rep_td4, n.ahead = 40) |> setDT()
irf_df_rep_td32 <- vars::irf(mbv_rep_td32, n.ahead = 40) |> setDT()
irf_df_rep_td632 <- vars::irf(mbv_rep_td632, n.ahead = 40) |> setDT()

irf_df_rep_fd[, version := "replication"][, model := "bayesian_fd"]
irf_df_rep_td4[, version := "replication"][, model := "bayesian_td4"]
irf_df_rep_td32[, version := "replication"][, model := "bayesian_td32"]
irf_df_rep_td632[, version := "replication"][, model := "bayesian_td632"]


## Fit Exact Replication
bv_exact_rep <-
    estimate_bvartools(v, burnin = 49000, a_v_i_prior = origional_prior)
mbv_exact_rep_fd <- id_fevdfd(bv_exact_rep, tv, bc_freqs, 1000)
mbv_exact_rep_td4 <- id_fevdtd(bv_exact_rep, tv, 4)
mbv_exact_rep_td32 <- id_fevdtd(bv_exact_rep, tv, 32)
mbv_exact_rep_td632 <- id_fevdtd(bv_exact_rep, tv, 6:32)

irf_df_exact_rep_fd <- vars::irf(mbv_exact_rep_fd, n.ahead = 40) |> setDT()
irf_df_exact_rep_td4 <- vars::irf(mbv_exact_rep_td4, n.ahead = 40) |> setDT()
irf_df_exact_rep_td32 <- vars::irf(mbv_exact_rep_td32, n.ahead = 40) |> setDT()
irf_df_exact_rep_td632 <- vars::irf(mbv_exact_rep_td632, n.ahead = 40) |> setDT()

irf_df_exact_rep_fd[, version := "replication"][, model := "bayesian_fd_exact"]
irf_df_exact_rep_td4[, version := "replication"][, model := "bayesian_td4_exact"]
irf_df_exact_rep_td32[, version := "replication"][, model := "bayesian_td32_exact"]
irf_df_exact_rep_td632[, version := "replication"][, model := "bayesian_td632_exact"]



## Combind data.frames
df <- rbindlist(list(
    irf_df_small_rep_fd[shock == "Main"],
    irf_df_small_rep_td4[shock == "Main"],
    irf_df_small_rep_td32[shock == "Main"],
    irf_df_small_rep_td632[shock == "Main"],
    irf_df_rep_fd[shock == "Main"],
    irf_df_rep_td4[shock == "Main"],
    irf_df_rep_td32[shock == "Main"],
    irf_df_rep_td632[shock == "Main"],
    irf_df_exact_rep_fd[shock == "Main"],
    irf_df_exact_rep_td4[shock == "Main"],
    irf_df_exact_rep_td32[shock == "Main"],
    irf_df_exact_rep_td632[shock == "Main"],
    bca_irf_df
    ), use.names = TRUE, fill = TRUE)


fwrite(df, "./data/replicated_bca_bayesian_VAR_IRF_bvartools.csv")
