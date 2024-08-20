test_that("VAR direct coefficients match VAR", {

  data <- svars::USA
  lags <- 1:4
  types <-  c("const", "none", "trend", "both")

  for (t in types) {
    for (p in lags) {
      x <- Reduce(cbind, lapply(1:p, function(.x) {
        x_i <- data[-c(seq(nrow(data) - (.x - 1), length.out = .x), seq_len(p - .x)), ]
        colnames(x_i) <- paste0(colnames(data), "_L", .x)
        x_i
      }))
      y <- data[-seq_len(p), ]
      v <- vars::VAR(data, p = p, type = t)
      vd <- var_direct(y, x, type = t)

      coef_v <- coef(v)
      coef_vd <- coef(vd)

      expect_equal(coef_v, coef_vd, ignore_attr = TRUE, label = paste0("type: ", t, ", lags: ", p))
    }
  }

})


test_that("VAR direct can be used with svars::id.chol", {

  data <- svars::USA
  lags <- 1:4
  types <-  c("const", "none", "trend", "both")

  for (t in types) {
    for (p in lags) {
      x <- Reduce(cbind, lapply(1:p, function(.x) {
        x_i <- data[-c(seq(nrow(data) - (.x - 1), length.out = .x), seq_len(p - .x)), ]
        colnames(x_i) <- paste0(colnames(data), "_L", .x)
        x_i
      }))
      y <- data[-seq_len(p), ]
      v <- vars::VAR(data, p = p, type = t)
      vd <- var_direct(y, x, type = t)

      b_v <- svars::id.chol(v)$B
      b_vd <- svars::id.chol(vd)$B

      expect_equal(b_v, b_vd, ignore_attr = TRUE, label = paste0("type: ", t, ", lags: ", p))
    }
  }

})
