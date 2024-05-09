#' `bvartools` calculates priors slightly differently than
#'  the BCA paper.  This recreates the priors from BCA.
#'
#' @param data, matrix
#' @param lags number of VAR lags
#' @param gamma1 number paramater
#' @param gamma2 number paramater
#' @param gamma3 number paramater
#' @param gamma4 number paramater
#'
#' @return list of minneapolis priors
#' @export
#'
bca_mn_priors <- function(
  data,
  lags,
  gamma1,
  gamma2,
  gamma3,
  gamma4
) {

  k <- ncol(data)
  n <- nrow(data)
  m <- (k * k * lags) + k

  ## Set mean values for the first lags to 1, 0 for all else
  mu <- matrix(0, m, 1)
  mu[seq(1, k * k, by = k + 1)] <- 1


  ## Construct SE by running AR(1) for each variable
  se <- rep(0, k)
  for (i in 1:k){
    yi <- t(data[-1, i])
    xi <- rbind(t(data[-n, i]), rep(1, n - 1))
    betas <- tcrossprod(yi, xi) %*% solve(tcrossprod(xi))
    yi_hat <- betas %*% xi
    epsilon <- yi - yi_hat
    df <- ncol(yi) - nrow(xi)
    se[i] <- sqrt(tcrossprod(epsilon, epsilon) / df)
  }

  ## Calculate the variances for coefficients using BCA formula
  v <- matrix(0, m, m)
  for (i in 1:k) {
    for (j in 1:k) {
      for (l in 1:lags){
        index <- i + (j - 1) * k + (l - 1) * k * k
        if (i == j) {
          v[index, index] <- (gamma1 / l^gamma3)^2
        } else {
          se2 <- (se[i] / se[j])^2
          v[index, index] <- se2 * (gamma1 * gamma2 / l^gamma3)^2
        }
      }
    }
    index <- (k * k * lags) + i
    v[index, index] <- (se[i] * gamma4)^2
  }

  v_i <- solve(v, tol = 1e-20)

  priors <- list(
    mu = mu,
    v_i = v_i,
    sigma_i = diag(se)
  )

  return(priors)
}
