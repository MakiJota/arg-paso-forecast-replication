# =============================================================================
# Core model: PASO votes x random transition matrices -> election outcomes
# =============================================================================
# A transition matrix P (K x K) has rows = PASO categories (origin) and
# columns = destinations. P[i, j] is the share of PASO category i that ends up
# in destination j. Destination votes are D = v %*% P, with v the PASO votes.
#
# Each cell is drawn independently from U(min_ij, max_ij), clipped to [0, 1]
# and each row is renormalised to sum to 1. This is exactly the generator used
# in the original 2023 scripts (mundos.R, mundos_presi.R), but vectorised.
# =============================================================================

CATS <- c("MILEI", "BULLRICH", "LARRETA", "MASSA", "GRABOIS", "SCHIARETTI",
          "BREGMAN", "OTROS", "BLANCO", "ANULADOS", "NOVOTO")

read_range <- function(path) {
  m <- as.matrix(read.csv(path, row.names = 1, check.names = FALSE))
  stopifnot(identical(rownames(m), CATS), identical(colnames(m), CATS))
  m
}

read_paso <- function(path, col = "votes_provisional") {
  d <- read.csv(path, stringsAsFactors = FALSE)
  stopifnot(identical(d$category, CATS))
  setNames(d[[col]], d$category)
}

#' Draw n transition matrices and return destination votes (n x K matrix).
#'
#' @param votes   named vector of PASO votes (length K, order = CATS)
#' @param lo, hi  K x K matrices of lower / upper bounds
#' @param n       number of draws
#' @param digits  rounding applied to each raw draw (original code used 2)
#' @param chunk   draws processed per block (memory control)
simulate_uniform <- function(votes, lo, hi, n, digits = 2, chunk = 50000) {
  K <- length(votes)
  lo_v <- as.vector(lo); hi_v <- as.vector(hi)          # column-major
  out <- matrix(NA_real_, n, K, dimnames = list(NULL, colnames(lo)))
  done <- 0
  while (done < n) {
    m <- min(chunk, n - done)
    U <- matrix(runif(m * K * K, rep(lo_v, each = m), rep(hi_v, each = m)), m, K * K)
    if (!is.null(digits)) U <- round(U, digits)
    U <- pmin(pmax(U, 0), 1)
    out[done + seq_len(m), ] <- transfer(U, votes, K)
    done <- done + m
  }
  out
}

#' Dirichlet alternative: each row ~ Dirichlet(alpha * mu_i), where mu_i is the
#' row of midpoints of the clipped ranges (renormalised). `alpha` controls how
#' concentrated draws are around mu_i (larger = more certain experts).
simulate_dirichlet <- function(votes, lo, hi, n, alpha, chunk = 50000) {
  K <- length(votes)
  mid <- (pmin(pmax(lo, 0), 1) + pmin(pmax(hi, 0), 1)) / 2
  mu  <- mid / rowSums(mid)
  shape <- as.vector(alpha * mu)
  out <- matrix(NA_real_, n, K, dimnames = list(NULL, colnames(lo)))
  done <- 0
  while (done < n) {
    m <- min(chunk, n - done)
    U <- matrix(0, m, K * K)
    pos <- which(shape > 0)                 # zero-mean cells stay at zero
    U[, pos] <- matrix(rgamma(m * length(pos), rep(shape[pos], each = m)), m)
    out[done + seq_len(m), ] <- transfer(U, votes, K)
    done <- done + m
  }
  out
}

# U holds unnormalised cell weights (m x K^2, column-major cell order).
transfer <- function(U, votes, K) {
  D <- matrix(0, nrow(U), K)
  for (i in seq_len(K)) {
    idx <- i + (seq_len(K) - 1) * K        # cells of origin row i
    W <- U[, idx, drop = FALSE]
    z <- rowSums(W) == 0                   # only possible with widened ranges (stress test):
    if (any(z)) W[z, ] <- 1                # agnostic fallback = equal split
    D <- D + votes[i] * W / rowSums(W)
  }
  D
}

# -----------------------------------------------------------------------------
# Outcome rules
# -----------------------------------------------------------------------------
FIRST_ROUND_TICKETS <- c("MASSA", "MILEI", "BULLRICH", "SCHIARETTI", "BREGMAN")

#' Shares of the vote among tickets.
#' positive = TRUE: denominator = votes for tickets only (votos afirmativos
#'   validamente emitidos, Art. 97-98 of the Constitution; blank votes excluded).
#' positive = FALSE: replicates the 2023 scripts, which also put BLANCO in the
#'   denominator.
shares <- function(D, tickets, positive = TRUE) {
  den <- rowSums(D[, tickets, drop = FALSE])
  if (!positive) den <- den + D[, "BLANCO"]
  100 * D[, tickets, drop = FALSE] / den
}

#' Classify each draw: first-round winner or runoff pairing (Art. 97-98).
classify_first_round <- function(S) {
  o <- t(apply(S, 1, order, decreasing = TRUE))
  top1 <- S[cbind(seq_len(nrow(S)), o[, 1])]
  top2 <- S[cbind(seq_len(nrow(S)), o[, 2])]
  win  <- top1 >= 45 | (top1 >= 40 & top1 - top2 >= 10)
  n1 <- colnames(S)[o[, 1]]; n2 <- colnames(S)[o[, 2]]
  pair <- ifelse(n1 < n2, paste(n1, n2, sep = "-"), paste(n2, n1, sep = "-"))
  data.frame(first = n1, second = n2, win = win,
             outcome = ifelse(win, paste("Outright", n1), paste("Runoff", pair)),
             stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# Forecast evaluation helpers
# -----------------------------------------------------------------------------
pit <- function(draws, actual) mean(draws <= actual)        # probability integral transform
interval_cover <- function(draws, actual, level) {
  q <- quantile(draws, c((1 - level) / 2, 1 - (1 - level) / 2))
  actual >= q[1] && actual <= q[2]
}
#' Multi-category Brier score (0 = perfect, 2 = worst).
brier <- function(probs, realised) {
  y <- as.numeric(names(probs) == realised)
  sum((probs - y)^2)
}
#' Continuous ranked probability score from draws (lower is better).
crps_sample <- function(draws, actual, m = 20000) {
  x <- if (length(draws) > m) sample(draws, m) else draws
  mean(abs(x - actual)) - 0.5 * mean(abs(x - sample(x)))
}
