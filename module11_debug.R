## -----------------------------
## Helper: Tukey outlier function
## -----------------------------
tukey.outlier <- function(v, k = 1.5, na.rm = TRUE) {
  qs <- quantile(v, probs = c(0.25, 0.75), na.rm = na.rm, names = FALSE)
  iqr <- qs[2] - qs[1]
  lower <- qs[1] - k * iqr
  upper <- qs[2] + k * iqr
  v < lower | v > upper
}

## -----------------------------
## ORIGINAL (BUGGY) VERSION
## -----------------------------
tukey_multiple <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    # BUG: && only checks a single TRUE/FALSE
    outliers[, j] <- outliers[, j] && tukey.outlier(x[, j])
  }
  outlier.vec <- logical(nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  outlier.vec
}

## -----------------------------
## Reproduce the issue
## -----------------------------
set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)
cat("Buggy version:\n")
print(tukey_multiple(test_mat))
# Error expected:
# Error in outliers[, j] && tukey.outlier(x[, j]) :
#   'length = 10' in coercion to 'logical(1)'

## -----------------------------
## Corrected version
## -----------------------------
corrected_tukey <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  result <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    result[i] <- all(outliers[i, ])
  }
  result
}

cat("\nCorrected version:\n")
print(corrected_tukey(test_mat))
