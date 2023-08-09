##
##  Wrapper function to create a `bvartools` estimation
##  Takes priors and how many itertions to run, and
##  runs a gibbs sampler to find draws of coefficients
##  and the sigma matrix.
##

estimate_bvartools <- function(
    v, ## var from gen_var
    iterations = 1000,
    burnin = 1000,
    mn_priors = list(
        kappa0 = 0.2^2,
        kappa1 = 0.5^2,
        kappa2 = 0.5^2,
        kappa3 = 10^10 / 0.2^2
    ),
    a_v_i_prior = NULL,  ## Can pass a variance matrix to overwrite priors
    ...
) {

    draws <- iterations + burnin

    ## Get frequentist values
    y <- t(v$data$Y)
    x <- t(v$data$Z)
    tt <- ncol(y)
    k <- nrow(y)
    m <- k * nrow(x)
    p <- v$model$endogen$lags

    A_freq <- tcrossprod(y, x) %*% solve(tcrossprod(x))
    u_freq <- y - A_freq %*% x
    u_sigma_freq <- tcrossprod(u_freq) / (ncol(y) - nrow(x))

    ## Set up Bayesain Minnesota Priors
    priors <- bvartools::minnesota_prior(
        v,
        kappa0 = mn_priors$kappa0,
        kappa1 = mn_priors$kappa1,
        kappa2 = mn_priors$kappa2,
        kappa3 = mn_priors$kappa3,
        coint_var = TRUE,
        sigma = "AR"
        )

    ## Set the priors
    a_mu_prior <- priors$mu
    if (is.null(a_v_i_prior)) a_v_i_prior <- priors$v_i

    u_sigma_df_prior <- k
    u_sigma_scale_prior <- diag(1, k)
    u_sigma_df_post <- tt + u_sigma_df_prior

    ## Set initial u_sigma guess
    u_sigma_i <- solve(u_sigma_freq)


    ## Gibbs Sampler drawing
    draws_a <- matrix(NA, m, iterations)
    draws_sigma <- matrix(NA, k^2, iterations)

    for (draw in 1:draws) {
        if (draw %% 10^3 == 0) print(draw)
        # Draw conditional mean parameters
        a <- bvartools::post_normal(y, x, u_sigma_i, a_mu_prior, a_v_i_prior)

        # Draw variance-covariance matrix
        u <- y - matrix(a, k) %*% x # Obtain residuals
        u_sigma_scale_post <- solve(u_sigma_scale_prior + tcrossprod(u))
        u_sigma_i <-
            matrix(rWishart(1, u_sigma_df_post, u_sigma_scale_post)[, , 1], k)

        # Store draws
        if (draw > burnin) {
        draws_a[, draw - burnin] <- a
        draws_sigma[, draw - burnin] <- solve(u_sigma_i)
        }
    }

    bvar_est <- list(
        y = y,
        x = x,
        A = draws_a[1:(p * k * k), ],
        C = draws_a[(p * k * k + 1):(p * k * k + k), ],
        Sigma = draws_sigma)

    ## Custom class, because "bvar" is used by `BVAR` package
    class(bvar_est) <- "bvartools"

    return(bvar_est)
}
