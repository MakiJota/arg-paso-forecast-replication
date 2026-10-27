# =============================================================================
# Agnostic "retention + random noise" model, chained first round -> runoff.
#
# Each PASO electorate keeps a share rho ~ U(rho_lo, rho_hi) at its "home"
# destination (own candidate; coalition nominee for internal losers; blank,
# null and non-voters stay so). The remaining 1 - rho is scattered by a flat
# Dirichlet over all destinations. Electorates without a home (minor lists)
# are scattered entirely at random.
#
# Runoff: in every draw that goes to a runoff, the two finalists are whoever
# finished first and second IN THAT DRAW. Electorates whose home is a finalist
# keep rho there; all others (including eliminated candidates' electorates)
# split completely at random across {finalist 1, finalist 2, blank, null,
# abstention}. Nothing is conditioned on the realised pairing.
# =============================================================================
rdirichlet_flat <- function(n, k) { g <- matrix(rexp(n * k), n, k); g / rowSums(g) }

transfer_agnostic <- function(votes, home, dest, n, rho_lo, rho_hi) {
  D <- matrix(0, n, length(dest), dimnames = list(NULL, dest))
  for (i in names(votes)) {
    W <- rdirichlet_flat(n, length(dest)); colnames(W) <- dest
    h <- home[[i]]
    if (!is.na(h) && h %in% dest) {
      rho <- runif(n, rho_lo, rho_hi)
      W <- W * (1 - rho); W[, h] <- W[, h] + rho
    }
    D <- D + votes[[i]] * W
  }
  D
}

#' @param votes     named PASO votes (origin categories)
#' @param home      named vector origin -> home destination (NA = none)
#' @param tickets   first-round tickets
#' @return data.frame per draw: first, second, outright, president, and the
#'         two-candidate share of the eventual winner where relevant
agnostic_cycle <- function(votes, home, tickets, n, rho_lo, rho_hi) {
  others <- c("BLANCO", "ANULADOS", "NOVOTO")
  D1 <- transfer_agnostic(votes, home, c(tickets, others), n, rho_lo, rho_hi)
  S <- 100 * D1[, tickets] / rowSums(D1[, tickets])
  cl <- classify_first_round(S)
  president <- ifelse(cl$win, cl$first, NA_character_)
  pairs <- unique(cl[!cl$win, c("first", "second")])
  for (k in seq_len(nrow(pairs))) {
    f <- c(pairs$first[k], pairs$second[k])
    idx <- which(!cl$win & cl$first == f[1] & cl$second == f[2])
    D2 <- transfer_agnostic(votes, home, c(f, others), length(idx), rho_lo, rho_hi)
    president[idx] <- ifelse(D2[, f[1]] > D2[, f[2]], f[1], f[2])
  }
  data.frame(first = cl$first, second = cl$second, outright = cl$win,
             pair = ifelse(cl$win, NA, paste(pmin(cl$first, cl$second), pmax(cl$first, cl$second), sep = "-")),
             president = president, stringsAsFactors = FALSE)
}
