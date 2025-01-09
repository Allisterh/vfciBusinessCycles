#' Use the fit of a linear regression estimating the heteroskedasticity
#' of the variance of the residuals in a VAR to identify a structural VAR.
#'
#' @param var a VAR, either vars::VAR or svars object
#' @param target variable in VAR to target with het_reg
#' @param het_reg_lags lags passed to het_reg
#' @param constant, boolean, default to TRUE for constant in het_reg regression
#' @param hetreg_method, defaults to "twostep", can change to "ML"
#' @param hetreg_horizon defaults to 1, number of periods to calculate the forecast error
#' @param sign Default to "positive". Can be "negative".  Ensures the
#' cummulative impact of the main shock on the target variable is the
#' given sign.
#' @param sign_horizon Default to 1. The horizon through which to accumulate the
#' impact of the shock.
#' @param method, "default" for our derivations for finding the q column,
#' and "mriv" to use Mertens and Ravn (2013) Appendix A IV derivations.
#' @param x2 indepedent variables for predicting heteroskedasticity
#' @param extra_data additional data columns to cbind with the data from the VAR,
#' 
#'
#' @return SVAR
#' @export
#'
#' @import data.table
#'
id_linear_het_reg <- function(
  var,
  target,
  het_reg_lags = 0,
  constant = TRUE,
  hetreg_method = "twostep",
  hetreg_horizon = 1,
  cumsum = FALSE,
  sign = "pos",
  sign_horizon = 1,
  method = "default",
  x2 = NULL,
  extra_data = NULL
) {

  data <- get_data_from_var(var) |> as.data.table()
  y_names <- colnames(data) |> copy()
  data[, t := .I - var$p]

  ## Fit Cholesky SVAR
  cv <- fevdid::id_ordered_chol(var)

  het_reg <- fit_het_reg_from_var(
    var,
    lags = het_reg_lags,
    constant = constant,
    hetreg_method = hetreg_method,
    hetreg_horizon = hetreg_horizon,
    cumsum = cumsum,
    x2 = x2,
    extra_data = extra_data
  )

  ## Find the Q rotation column
  if (method == "default") {
    q <- find_linear_het_reg_q(var, het_reg, target)
  } else if (method == "mriv") {
    q <- find_linear_het_reg_q_mriv(var, het_reg, target)
  }

  ## Make the rest of the columns of Q orthogonal to q
  Q <- matrix(0, var$K, var$K)
  rownames(Q) <- y_names
  Q[, 1] <- q
  Q[, 2:var$K] <- pracma::nullspace(t(q))

  ## Rotate the SVAR Cholesky for Linear Het ID
  hv <- copy(cv)
  hv$B <- cv$B %*% Q
  hv$Q <- Q
  hv$het_reg <- het_reg

  ## Insure the sign is as expected
  irf <- vars::irf(hv, impulse = "Chol_1", response = target, n.ahead = sign_horizon)
  if (sign == "positive" || sign == "pos") {
    if (sum(irf$irf[1:sign_horizon, "irf"]) < 0) {
      hv$Q <- -1 * hv$Q
      hv$B <- -1 * hv$B
    }
  } else if (sign == "negative" || sign == "neg") {
    if (sum(irf$irf[1:sign_horizon, "irf"]) > 0) {
      hv$Q <- -1 * hv$Q
      hv$B <- -1 * hv$B
    }
  }

  return(hv)
}

#' Return a single column vector of the Q rotation matrix.
#' Uses the IV method in Mertens Rvan (2013) Appendix A.
#'
#' @param var a VAR
#' @param het_reg het_reg from fit_het_reg_from_var
#' @param target variable in VAR use to identify Q column
#'
#' @return A single column vector of the orthonormal matrix Q
#' @export
#'
#' @import data.table
#'
find_linear_het_reg_q_mriv <- function(
  var,
  het_reg,
  target
) {
  variable <- NULL

  resid_data <-
    stats::residuals(var) |>
    data.table() |>
    cbind(het_reg$dt[variable == target & t > 0, c("log_var_fitted"), with = FALSE]) |>
    stats::na.omit()

  u <- as.matrix(resid_data[, colnames(var$y), with = FALSE]) |> t()
  m <- as.matrix(resid_data[, "log_var_fitted", with = FALSE]) |> t()
  Sig_uu <- u %*% t(u)
  Sig_mu <- m %*% t(u)

  ## Mertens Rvan (2013) Appendix A method
  Sig_u1u1 <- Sig_uu[1, 1]
  Sig_u2u1 <- Sig_uu[2:var$K, 1]
  Sig_u2u2 <- Sig_uu[2:var$K, 2:var$K]

  Sig_mu1 <- Sig_mu[, 1]
  Sig_mu2 <- Sig_mu[, 2:var$K]

  B21B11I <- t(solve(Sig_mu1) %*% Sig_mu2)
  Z <- B21B11I %*% Sig_u1u1 %*% t(B21B11I) - (Sig_u2u1 %*% t(B21B11I) + B21B11I %*% t(Sig_u2u1)) + Sig_u2u2
  B12B12t <- t(Sig_u2u1 - B21B11I %*% Sig_u1u1) %*% solve(Z) %*% (Sig_u2u1 - B21B11I %*% Sig_u1u1)
  B11B11t <- Sig_u1u1 - B12B12t
  B22B22t <- Sig_u2u2 + B21B11I %*% (B12B12t - Sig_u1u1) %*% t(B21B11I)
  B12B22I <- (B12B12t %*% t(B21B11I) + t(Sig_u2u1 - B21B11I %*% Sig_u1u1)) %*% solve(B22B22t)

  SSt <- (1 - B12B22I %*% B21B11I) %*% B11B11t %*% t(1 - B12B22I %*% B21B11I)
  S <- sqrt(SSt)

  B11 <- solve(1 - B12B22I %*% B21B11I) %*% S
  B21 <- (B21B11I %*% solve(1 - B12B22I %*% B21B11I)) %*% S

  B1 <- c(t(B11), t(B21))
  Q1 <- solve(t(chol(Sig_uu))) %*% B1

  return(Q1)
}


#' Return a single column vector of the Q rotation matrix.
#' Uses our own derivations with the het_reg as an IV.
#'
#' @param var a VAR
#' @param het_reg het_reg from fit_het_reg_from_var
#' @param target variable in VAR use to identify Q column
#'
#' @return A single column vector of the orthonormal matrix Q
#' @export
#'
#' @import data.table
#'
find_linear_het_reg_q <- function(
  var,
  het_reg,
  target
) {

  ## Cholesky Matrix
  P <- t(chol(stats::cov(stats::residuals(var))))

  ## Coefficients from het_reg regression for target variable
  hr <- het_reg$het_regs[[target]]

  if (inherits(hr, "hetreg")) {
    c_0k <- stats::coef(hr$lm2_adj)
    c_0k <- c_0k[names(c_0k) != "(Intercept)"]
    c_0k <- c_0k[!grepl("_L\\d", names(c_0k))]
  } else if (inherits(hr, "gls")) {
    c_0k <- stats::coef(hr$modelStruct)
  }

  ## Find the q rotation and normalize it to length 1
  q_h <- t(P) %*% c_0k
  q_h <- q_h %*% solve(sqrt(c_0k %*% P %*% t(P) %*% c_0k))

  return(q_h)
}
