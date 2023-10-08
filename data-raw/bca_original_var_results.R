## Code to clean various VAR results from the BCA paper
## into a tidy new file: `bca_original_var_results.csv`.
require(dplyr)
require(data.table)
require(ggplot2)

## Map of variable codes to names
var_names <- tibble(
    varnames = c(
        "output", "investment", "consumption",
        "hours_worked", "unemployment", "labor_share",
        "interest", "inflation", "productivity", "TFP"
        ),
    variable = 1:10)

impulse_names <- c("Main", paste0("Orth_", 2:10))

irf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_fd_sr_IRFsr.csv",
    colClasses = "numeric"
    ) |> as.matrix() |> c()

irf_df <- data.table(
    h = rep(1:40, each = 10 * 1000, times = 10),
    iter = rep(1:1000, each = 10, times = 10 * 40),
    impulse = rep(impulse_names, each = 10 * 1000 * 40),
    response = rep(var_names$varnames, times = 10 * 1000 * 40),
    value = -1 * irf
    )

irf_summ <- irf_df[,.(
        median = median(value),
        pctl_16 = quantile(value, 0.16),
        pctl_84 = quantile(value, 0.84)
    ),
        by = .(h, impulse, response)
    ]

## Only Keep "unemploymnt shock"
df <- irf_summ[impulse == "Main"]

df_bvar_fd <- df[, model := "bayesian_fd"]

## Time Domain, targeting 0 - 4 quarters
mirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td4_mirf.csv",
    colClasses = "numeric"
    )[2, ]
lirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td4_lirf.csv",
    colClasses = "numeric"
    )[2, ]
sirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td4_sirf.csv",
    colClasses = "numeric"
    )[2, ]

df_bvar_td4 <- data.table(
    h = rep(1:40, times = 10),
    impulse = rep("Main", times = 40 * 10),
    response = rep(var_names$varnames, each = 40),
    median = unlist(mirf),
    pctl_16 = unlist(lirf),
    pctl_84 = unlist(sirf),
    model = "bayesian_td4"
)

## Time Domain, targeting 6 - 32 quarters
mirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td632_mirf.csv",
    colClasses = "numeric"
    )[2, ]
lirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td632_lirf.csv",
    colClasses = "numeric"
    )[2, ]
sirf <- fread(
    "./data-raw/bca_original_var_results/benchmark_var_1955_2017_td632_sirf.csv",
    colClasses = "numeric"
    )[2, ]

df_bvar_td632 <- data.table(
    h = rep(1:40, times = 10),
    impulse = rep("Main", times = 40 * 10),
    response = rep(var_names$varnames, each = 40),
    median = unlist(mirf),
    pctl_16 = unlist(lirf),
    pctl_84 = unlist(sirf),
    model = "bayesian_td632"
)

## Classical VAR, Frequency Domain, 6 - 32 quarters
mirf <- fread(
    "./data-raw/bca_original_var_results/classical_var_1955_2017_fd_sr_mirf.csv",
    colClasses = "numeric"
    )
lirf <- fread(
    "./data-raw/bca_original_var_results/classical_var_1955_2017_fd_sr_lirf.csv",
    colClasses = "numeric"
    )
sirf <- fread(
    "./data-raw/bca_original_var_results/classical_var_1955_2017_fd_sr_sirf.csv",
    colClasses = "numeric"
    )

varirf <- fread(
    "./data-raw/bca_original_var_results/classical_var_1955_2017_fd_sr_varirf.csv",
    colClasses = "numeric"
    )

df_var_fd <- data.table(
    h = rep(1:40, times = 10),
    impulse = rep("Main", times = 40 * 10),
    response = rep(var_names$varnames, each = 40),
    irf = unlist(varirf),
    median = unlist(mirf),
    pctl_16 = unlist(lirf),
    pctl_84 = unlist(sirf),
    model = "classical_fd"
)

original_var_results <- rbindlist(list(
    df_bvar_fd,
    df_bvar_td4,
    df_bvar_td632,
    df_var_fd
    ), use.names = TRUE, fill = TRUE)

## Write to Disk
fwrite(original_var_results, "data-raw/bca_original_var_results.csv")
