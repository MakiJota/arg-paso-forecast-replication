# =============================================================================
# Validation step 2: estimate the transfers that actually happened
# (PASO -> general, PASO -> runoff) by ecological inference on 528 departments,
# and compare them with the ex-ante transfer bounds used in the forecast.
#
# Estimators
#   * lphom (Romero et al. 2020): linear-programming EI, the point estimate.
#   * eiPack::ei.MD.bayes (Rosen et al. 2001): Bayesian multinomial-Dirichlet
#     R x C model; gives 90% credible intervals. Slow: run with RUN_BAYES=TRUE.
#
# Outputs: paper/output/tables/validation_*.csv, paper/output/figures/fig_validation_*.png,
#          paper/output/key_numbers_validation.tex
# Run from the repository root (after 01_build_department_data.R).
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(lphom); library(ggplot2) })
source("paper/code/R/model.R")
RUN_BAYES <- identical(Sys.getenv("RUN_BAYES"), "TRUE")
set.seed(20231119)
OUT <- "paper/output"

dep <- fread("paper/data/departments_2023.csv")
ORIG <- CATS                                                     # 11 PASO categories
DEST <- list(gen = c("MILEI", "BULLRICH", "MASSA", "SCHIARETTI", "BREGMAN", "BLANCO", "ANULADOS", "NOVOTO"),
             run = c("MILEI", "MASSA", "BLANCO", "ANULADOS", "NOVOTO"))

# Origin counts rescaled to the destination electorate of each department, so that
# row and column totals match (the electoral roll changes slightly between elections).
build <- function(stage) {
  X <- as.matrix(dep[, paste0("paso_", ORIG), with = FALSE]); colnames(X) <- ORIG
  Y <- as.matrix(dep[, paste0(stage, "_", DEST[[stage]]), with = FALSE]); colnames(Y) <- DEST[[stage]]
  X <- X / rowSums(X) * rowSums(Y)
  X <- round(X); X[, "NOVOTO"] <- X[, "NOVOTO"] + (rowSums(Y) - rowSums(X))   # exact totals
  list(X = X, Y = Y)
}

# Forecast's implied transition matrix: distribution of the renormalised cells.
forecast_cells <- function(lo, hi, dest, n = 20000) {
  K <- length(CATS); lo_v <- as.vector(lo); hi_v <- as.vector(hi)
  U <- matrix(runif(n * K * K, rep(lo_v, each = n), rep(hi_v, each = n)), n, K * K)
  U <- pmin(pmax(round(U, 2), 0), 1)
  P <- array(NA_real_, c(n, K, K), dimnames = list(NULL, CATS, CATS))
  for (i in seq_len(K)) { idx <- i + (seq_len(K) - 1) * K; P[, i, ] <- U[, idx] / rowSums(U[, idx]) }
  # destinations absent from the election (LARRETA, GRABOIS, OTROS, and in the runoff the
  # eliminated tickets) received no votes in the forecast's tally: they are effectively
  # abstention, so they are merged into NOVOTO for comparison.
  gone <- setdiff(CATS, dest)
  Q <- P[, , dest, drop = FALSE]
  Q[, , "NOVOTO"] <- Q[, , "NOVOTO"] + apply(P[, , gone, drop = FALSE], c(1, 2), sum)
  Q
}

summarise_stage <- function(stage, lo, hi) {
  d <- build(stage)
  fit <- lphom(as.data.frame(d$X), as.data.frame(d$Y), new_and_exit_voters = "raw", verbose = FALSE)
  ei <- fit$VTM / 100                                   # rows = origin, proportions
  ei <- ei[ORIG, DEST[[stage]]]
  # second estimator (Pavia & Romero 2024): less prone to corner solutions
  fit_ns <- nslphom(as.data.frame(d$X), as.data.frame(d$Y), new_and_exit_voters = "raw", verbose = FALSE)
  ei_ns <- (fit_ns$VTM / 100)[ORIG, DEST[[stage]]]
  Q <- forecast_cells(lo, hi, DEST[[stage]])
  f_mean <- apply(Q, c(2, 3), mean); f_lo <- apply(Q, c(2, 3), quantile, .05); f_hi <- apply(Q, c(2, 3), quantile, .95)
  tab <- CJ(origin = ORIG, destination = DEST[[stage]], sorted = FALSE)
  tab[, `:=`(ei = ei[cbind(origin, destination)],
             forecast_mean = f_mean[cbind(origin, destination)],
             forecast_q05 = f_lo[cbind(origin, destination)],
             forecast_q95 = f_hi[cbind(origin, destination)])]
  tab[, inside := ei >= forecast_q05 - 0.005 & ei <= forecast_q95 + 0.005]   # 0.5 pt tolerance
  tab[, gap := ei - forecast_mean]
  tab[, ei_ns := ei_ns[cbind(origin, destination)]]
  tab[, inside_ns := ei_ns >= forecast_q05 - 0.005 & ei_ns <= forecast_q95 + 0.005]
  # structural zeros: forecast fixed at zero AND both estimators (near) zero -> trivial agreement
  tab[, structural_zero := forecast_q95 < 0.005 & ei < 0.005 & ei_ns < 0.005]

  # Error attribution: start from the forecast mean matrix and replace one origin
  # row at a time with its EI estimate; record the change in each candidate's share.
  v <- colSums(d$X); v <- v / sum(v) * sum(read_paso("paper/data/paso_2023.csv"))
  cands <- intersect(DEST[[stage]], FIRST_ROUND_TICKETS)
  share <- function(P) { y <- as.vector(v %*% P); names(y) <- DEST[[stage]]; 100 * y[cands] / sum(y[cands]) }
  base <- share(f_mean)
  attrib <- rbindlist(lapply(ORIG, function(i) {
    P <- f_mean; P[i, ] <- ei[i, ]
    data.table(origin = i, t(share(P) - base))
  }))
  list(tab = tab, ei = ei, ei_ns = ei_ns, fit = fit, fit_ns = fit_ns, attrib = attrib, base = base,
       ei_share = share(ei), ei_ns_share = share(ei_ns), d = d)
}

res <- list(gen = summarise_stage("gen", read_range("paper/data/ranges_first_round_min.csv"),
                                          read_range("paper/data/ranges_first_round_max.csv")),
            run = summarise_stage("run", read_range("paper/data/ranges_runoff_min.csv"),
                                          read_range("paper/data/ranges_runoff_max.csv")))

for (s in names(res)) {
  fwrite(res[[s]]$tab,    file.path(OUT, sprintf("tables/validation_%s_cells.csv", s)))
  fwrite(res[[s]]$attrib, file.path(OUT, sprintf("tables/validation_%s_error_attribution.csv", s)))
  cat(sprintf("\n================ %s ================\n", toupper(s)))
  cat("lphom heterogeneity index (HETe, %):", round(res[[s]]$fit$HETe, 2), "\n")
  cat("\nEI transition matrix (rows = PASO origin, %):\n"); print(round(100 * res[[s]]$ei, 1))
  cat("\nCandidate shares: forecast-mean matrix vs EI matrix (national PASO votes):\n")
  print(round(rbind(forecast_mean_matrix = res[[s]]$base, ei_matrix = res[[s]]$ei_share), 2))
  big <- res[[s]]$tab[origin %in% c("MILEI","BULLRICH","LARRETA","MASSA","GRABOIS","SCHIARETTI","BREGMAN","NOVOTO") &
                      (abs(gap) > 0.05 | !inside)][order(-abs(gap))]
  cat("\nCells outside the forecast 90% range or off by >5 pts (main origins):\n")
  print(big[, .(origin, destination, ei = round(100*ei,1), fc_mean = round(100*forecast_mean,1),
                fc_q05 = round(100*forecast_q05,1), fc_q95 = round(100*forecast_q95,1), inside)])
  cat("\nShare of cells inside the forecast 90% range (weighted by origin size):\n")
  w <- colSums(res[[s]]$d$X)[res[[s]]$tab$origin] * res[[s]]$tab$ei
  print(round(c(unweighted = mean(res[[s]]$tab$inside), vote_weighted = sum(w * res[[s]]$tab$inside) / sum(w)), 3))
  cat("\nError attribution (change in share when one origin row is set to its EI estimate):\n")
  print(res[[s]]$attrib[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
}

# ---------------------------------------------------------------- Figure: forecast range vs EI
plot_cells <- function(tab, title, file) {
  t <- tab[origin %in% c("MILEI","BULLRICH","LARRETA","MASSA","GRABOIS","SCHIARETTI","BREGMAN","NOVOTO")]
  t[, origin := factor(origin, levels = rev(c("MILEI","BULLRICH","LARRETA","MASSA","GRABOIS","SCHIARETTI","BREGMAN","NOVOTO")))]
  p <- ggplot(t, aes(y = origin)) +
    geom_linerange(aes(xmin = 100 * forecast_q05, xmax = 100 * forecast_q95), linewidth = 2.2, colour = "grey75") +
    geom_point(aes(x = 100 * forecast_mean), shape = 124, size = 3, colour = "grey35") +
    geom_point(aes(x = 100 * ei, colour = inside), size = 2) +
    scale_colour_manual(values = c(`TRUE` = "#3189e0", `FALSE` = "#d1495b"),
                        labels = c(`TRUE` = "inside forecast range", `FALSE` = "outside"), name = NULL) +
    facet_wrap(~ destination, nrow = 1) +
    labs(x = "% of PASO electorate moving to destination", y = "PASO origin", title = title,
         caption = "Grey bar: forecast 90% range; tick: forecast mean; dot: ecological-inference estimate (lphom).") +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(OUT, paste0("figures/", file, ".png")), p, width = 11, height = 4.2, dpi = 300)
  ggsave(file.path(OUT, paste0("figures/", file, ".pdf")), p, width = 11, height = 4.2)
}
plot_cells(res$gen$tab[destination != "ANULADOS"], "PASO to first round", "fig_validation_general")
plot_cells(res$run$tab[destination != "ANULADOS"], "PASO to runoff", "fig_validation_runoff")

# ---------------------------------------------------------------- Key numbers
fmt <- function(x, d = 1) formatC(unname(x), format = "f", digits = d)
cell <- function(s, o, de) res[[s]]$tab[origin == o & destination == de]
K <- list(
  NDepartments  = nrow(dep),
  ValInsideGen  = fmt(100 * mean(res$gen$tab$inside), 0),
  ValInsideRun  = fmt(100 * mean(res$run$tab$inside), 0),
  EiBullMileiRun  = fmt(100 * cell("run", "BULLRICH", "MILEI")$ei),
  FcBullMileiRun  = fmt(100 * cell("run", "BULLRICH", "MILEI")$forecast_mean),
  EiLarrMileiRun  = fmt(100 * cell("run", "LARRETA", "MILEI")$ei),
  FcLarrMileiRun  = fmt(100 * cell("run", "LARRETA", "MILEI")$forecast_mean),
  EiBullMileiGen  = fmt(100 * cell("gen", "BULLRICH", "MILEI")$ei),
  FcBullMileiGen  = fmt(100 * cell("gen", "BULLRICH", "MILEI")$forecast_mean),
  EiSchiaSchiaGen = fmt(100 * cell("gen", "SCHIARETTI", "SCHIARETTI")$ei),
  RunEiShareMilei = fmt(res$run$ei_share[["MILEI"]], 1),
  # agreement excluding structural zeros, and the second estimator
  ValInsideGenNZ = fmt(100 * res$gen$tab[!(structural_zero), mean(inside)], 0),
  ValInsideRunNZ = fmt(100 * res$run$tab[!(structural_zero), mean(inside)], 0),
  ValInsideGenNsNZ = fmt(100 * res$gen$tab[!(structural_zero), mean(inside_ns)], 0),
  ValInsideRunNsNZ = fmt(100 * res$run$tab[!(structural_zero), mean(inside_ns)], 0),
  HeteGen = fmt(res$gen$fit$HETe, 1), HeteRun = fmt(res$run$fit$HETe, 1),
  NsBullMileiGen = fmt(100 * cell("gen", "BULLRICH", "MILEI")$ei_ns),
  NsBullMileiRun = fmt(100 * cell("run", "BULLRICH", "MILEI")$ei_ns),
  NsLarrMileiRun = fmt(100 * cell("run", "LARRETA", "MILEI")$ei_ns),
  NsSchiaMileiRun = fmt(100 * cell("run", "SCHIARETTI", "MILEI")$ei_ns),
  EiSchiaMileiRun = fmt(100 * cell("run", "SCHIARETTI", "MILEI")$ei),
  FcSchiaMassaRun = fmt(100 * cell("run", "SCHIARETTI", "MASSA")$forecast_mean),
  # mobilisation of PASO non-voters, first round: share that voted, and its split
  FcNovotoVote = fmt(100 * (1 - cell("gen", "NOVOTO", "NOVOTO")$forecast_mean), 0),
  EiNovotoVote = fmt(100 * (1 - cell("gen", "NOVOTO", "NOVOTO")$ei), 0),
  NsNovotoVote = fmt(100 * (1 - cell("gen", "NOVOTO", "NOVOTO")$ei_ns), 0),
  FcNovotoMassaShare = fmt(100 * cell("gen", "NOVOTO", "MASSA")$forecast_mean / (1 - cell("gen", "NOVOTO", "NOVOTO")$forecast_mean), 0),
  EiNovotoMassaShare = fmt(100 * cell("gen", "NOVOTO", "MASSA")$ei / (1 - cell("gen", "NOVOTO", "NOVOTO")$ei), 0),
  FcNovotoBullShare = fmt(100 * cell("gen", "NOVOTO", "BULLRICH")$forecast_mean / (1 - cell("gen", "NOVOTO", "NOVOTO")$forecast_mean), 0),
  EiNovotoBullShare = fmt(100 * cell("gen", "NOVOTO", "BULLRICH")$ei / (1 - cell("gen", "NOVOTO", "NOVOTO")$ei), 0),
  # effective (post-renormalisation) forecast retention, to contrast with stated bounds
  FcBullBullGen = fmt(100 * cell("gen", "BULLRICH", "BULLRICH")$forecast_mean),
  EiBullBullGen = fmt(100 * cell("gen", "BULLRICH", "BULLRICH")$ei),
  FcSchiaMassaGen = fmt(100 * cell("gen", "SCHIARETTI", "MASSA")$forecast_mean),
  RunNsShareMilei = fmt(res$run$ei_ns_share[["MILEI"]], 1)
)
writeLines(c("% Auto-generated by paper/code/validation/02_ecological_inference.R",
             sprintf("\\newcommand{\\%s}{%s}", names(K), unlist(K))),
           file.path(OUT, "key_numbers_validation.tex"))
saveRDS(res, file.path(OUT, "validation_lphom.rds"))

# ---------------------------------------------------------------- Bayesian EI (optional, slow)
if (RUN_BAYES) {
  suppressPackageStartupMessages(library(eiPack))
  for (s in names(res)) {
    d <- res[[s]]$d
    df <- data.frame(d$X, d$Y[, DEST[[s]]]); names(df) <- c(paste0("o_", ORIG), paste0("d_", DEST[[s]]))
    f <- as.formula(sprintf("cbind(%s) ~ cbind(%s)", paste0("d_", DEST[[s]], collapse = ","), paste0("o_", ORIG, collapse = ",")))
    # ret.beta = "s" writes one .txt.gz per unit x cell into the WORKING DIRECTORY
    # (~50k files per run), so run it inside a dedicated, git-ignored folder.
    draws_dir <- file.path(OUT, "ei_bayes_draws", paste0("2023_", s))
    dir.create(draws_dir, recursive = TRUE, showWarnings = FALSE)
    old_wd <- setwd(draws_dir)
    m <- tryCatch(ei.MD.bayes(f, data = df, total = NULL, sample = 2000, burnin = 5000, thin = 5, ret.beta = "s"),
                  finally = setwd(old_wd))
    b <- lambda.MD(m, columns = paste0("d_", DEST[[s]]))
    q <- apply(b, 2, quantile, c(.05, .5, .95))
    saveRDS(list(model = m, lambda_q = q), file.path(OUT, sprintf("validation_bayes_%s.rds", s)))
    cat("\nBayesian EI (", s, ") medians and 90% CrI saved.\n")
  }
}
