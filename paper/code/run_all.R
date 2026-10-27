# =============================================================================
# Reproduces every number, table and figure in the manuscript.
# Run from the repository root:   Rscript paper/code/run_all.R
# Outputs go to paper/output/{tables,figures} and
# paper/output/key_numbers.tex (LaTeX macros read by the manuscript).
# =============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
source("paper/code/R/model.R")

set.seed(20230813)                       # PASO election day
N   <- 250000                            # same as the 2023 runs
OUT <- "paper/output"
dir.create(file.path(OUT, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)

votes  <- read_paso("paper/data/paso_2023.csv")
fr_lo  <- read_range("paper/data/ranges_first_round_min.csv")
fr_hi  <- read_range("paper/data/ranges_first_round_max.csv")
ro_lo  <- read_range("paper/data/ranges_runoff_min.csv")
ro_hi  <- read_range("paper/data/ranges_runoff_max.csv")
actual <- read.csv("paper/data/actual_results_2023.csv")
act_g  <- with(subset(actual, election == "general"), setNames(pct_positive, candidate))
act_r  <- with(subset(actual, election == "runoff"),  setNames(pct_positive, candidate))

K <- list()                              # key numbers for the manuscript
fmt <- function(x, d = 1) formatC(unname(x), format = "f", digits = d)

# -----------------------------------------------------------------------------
# 1. First round: PASO -> 22 October
# -----------------------------------------------------------------------------
D1 <- simulate_uniform(votes, fr_lo, fr_hi, N)
S_orig <- shares(D1, FIRST_ROUND_TICKETS, positive = FALSE)   # 2023 denominator
S_pos  <- shares(D1, FIRST_ROUND_TICKETS, positive = TRUE)    # legal denominator

cls_orig <- classify_first_round(S_orig)
cls_pos  <- classify_first_round(S_pos)
p_orig <- prop.table(table(cls_orig$outcome))
p_pos  <- prop.table(table(cls_pos$outcome))

tab_fr <- data.frame(
  candidate = FIRST_ROUND_TICKETS,
  mean      = colMeans(S_pos),
  sd        = apply(S_pos, 2, sd),
  q05       = apply(S_pos, 2, quantile, 0.05),
  q95       = apply(S_pos, 2, quantile, 0.95),
  actual    = act_g[FIRST_ROUND_TICKETS],
  pit       = sapply(FIRST_ROUND_TICKETS, function(k) pit(S_pos[, k], act_g[k])),
  in90      = sapply(FIRST_ROUND_TICKETS, function(k) interval_cover(S_pos[, k], act_g[k], .90)),
  crps      = sapply(FIRST_ROUND_TICKETS, function(k) crps_sample(S_pos[, k], act_g[k])),
  mean_orig = colMeans(S_orig)
)
tab_fr$abs_err <- abs(tab_fr$mean - tab_fr$actual)
write.csv(tab_fr, file.path(OUT, "tables/first_round_shares.csv"), row.names = FALSE)

tab_out <- merge(data.frame(outcome = names(p_pos),  p_legal = as.numeric(p_pos)),
                 data.frame(outcome = names(p_orig), p_original = as.numeric(p_orig)),
                 all = TRUE)
tab_out[is.na(tab_out)] <- 0
tab_out <- tab_out[order(-tab_out$p_legal), ]
write.csv(tab_out, file.path(OUT, "tables/first_round_outcomes.csv"), row.names = FALSE)

realised_fr <- "Runoff MASSA-MILEI"
p_vec <- setNames(tab_out$p_legal, tab_out$outcome)
K$BrierFR       <- fmt(brier(p_vec, realised_fr), 3)
K$PRunoffLegal  <- fmt(100 * mean(!cls_pos$win), 0)
K$PRunoffOrig   <- fmt(100 * mean(!cls_orig$win), 0)
K$PMMLegal      <- fmt(100 * p_vec[realised_fr], 0)
K$PMMOrig       <- fmt(100 * p_orig[realised_fr], 0)
K$PMassaOutLegal<- fmt(100 * mean(cls_pos$win & cls_pos$first == "MASSA"), 0)
K$PMassaOutOrig <- fmt(100 * mean(cls_orig$win & cls_orig$first == "MASSA"), 0)
K$PMassaFirst   <- fmt(100 * mean(cls_pos$first == "MASSA"), 0)
ord <- S_pos[, "MASSA"] > S_pos[, "MILEI"] & S_pos[, "MILEI"] > S_pos[, "BULLRICH"]
K$POrderExact   <- fmt(100 * mean(ord), 0)
K$MAEModel      <- fmt(mean(tab_fr$abs_err), 2)
for (k in FIRST_ROUND_TICKETS) {
  nm <- tools::toTitleCase(tolower(k))
  K[[paste0("Mean", nm)]]   <- fmt(tab_fr[k, "mean"])
  K[[paste0("Actual", nm)]] <- fmt(act_g[k], 2)
  K[[paste0("Pit", nm)]]    <- fmt(tab_fr[k, "pit"], 2)
}
K$CoverNinety <- sum(tab_fr$in90)

# Naive baseline (same definition as Table 1 / 03_paso_history.R): each coalition's
# full PASO total (all its lists), definitive count, renormalised over the five tickets.
# FIT-U = Bregman + Solano (Solano, definitive: 191,498; included in OTROS in paso_2023.csv).
vdef <- read_paso("paper/data/paso_2023.csv", "votes_definitive")
naive <- c(MILEI = vdef[["MILEI"]],
           BULLRICH = vdef[["BULLRICH"]] + vdef[["LARRETA"]],
           MASSA = vdef[["MASSA"]] + vdef[["GRABOIS"]],
           SCHIARETTI = vdef[["SCHIARETTI"]], BREGMAN = vdef[["BREGMAN"]] + 191498)
naive <- 100 * naive / sum(naive)

# Sensitivity: the same forecast with the definitive PASO count as input
S_def <- shares(simulate_uniform(vdef, fr_lo, fr_hi, 100000), FIRST_ROUND_TICKETS)
K$MAEDefinitiveInput <- fmt(mean(abs(colMeans(S_def) - act_g[FIRST_ROUND_TICKETS])), 2)
K$PMassaFirstDefinitiveInput <- fmt(100 * mean(classify_first_round(S_def)$first == "MASSA"), 0)
K$MAENaive <- fmt(mean(abs(naive[FIRST_ROUND_TICKETS] - act_g[FIRST_ROUND_TICKETS])), 2)
write.csv(data.frame(candidate = names(naive), naive_paso = naive, actual = act_g[names(naive)]),
          file.path(OUT, "tables/naive_baseline.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. Runoff: PASO -> 19 November (Milei vs Massa), as in mundos_presi.R
# -----------------------------------------------------------------------------
D2 <- simulate_uniform(votes, ro_lo, ro_hi, N)
milei2 <- 100 * D2[, "MILEI"] / (D2[, "MILEI"] + D2[, "MASSA"])   # two-party share
K$RunoffMean <- fmt(mean(milei2))
K$RunoffLoNinetyFive <- fmt(quantile(milei2, .025)); K$RunoffHiNinetyFive <- fmt(quantile(milei2, .975))
K$RunoffLoNinety <- fmt(quantile(milei2, .05));  K$RunoffHiNinety <- fmt(quantile(milei2, .95))
K$PMileiWins <- fmt(100 * mean(milei2 > 50), 0)
K$RunoffPit  <- fmt(pit(milei2, act_r[["MILEI"]]), 2)
K$RunoffActual <- fmt(act_r[["MILEI"]], 2)
K$RunoffBrier <- fmt((mean(milei2 > 50) - 1)^2 + (mean(milei2 <= 50) - 0)^2, 3)
write.csv(data.frame(stat = c("mean", "sd", "q025", "q05", "q50", "q95", "q975", "p_milei_wins", "pit_actual"),
                     value = c(mean(milei2), sd(milei2), quantile(milei2, c(.025, .05, .5, .95, .975)),
                               mean(milei2 > 50), pit(milei2, act_r[["MILEI"]]))),
          file.path(OUT, "tables/runoff_summary.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Robustness: Dirichlet generator with different expert certainty (alpha)
# -----------------------------------------------------------------------------
rob <- do.call(rbind, lapply(c(10, 25, 50, 100), function(a) {
  Dd <- simulate_dirichlet(votes, fr_lo, fr_hi, 50000, alpha = a)
  Sd <- shares(Dd, FIRST_ROUND_TICKETS); cd <- classify_first_round(Sd)
  Dr <- simulate_dirichlet(votes, ro_lo, ro_hi, 50000, alpha = a)
  mr <- 100 * Dr[, "MILEI"] / (Dr[, "MILEI"] + Dr[, "MASSA"])
  data.frame(alpha = a,
             mean_massa = mean(Sd[, "MASSA"]), mean_milei = mean(Sd[, "MILEI"]),
             mean_bullrich = mean(Sd[, "BULLRICH"]),
             p_runoff = mean(!cd$win), p_massa_milei = mean(cd$outcome == realised_fr),
             mae = mean(abs(colMeans(Sd) - act_g[FIRST_ROUND_TICKETS])),
             runoff_milei_mean = mean(mr), p_milei_wins = mean(mr > 50))
}))
write.csv(rob, file.path(OUT, "tables/robustness_dirichlet.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. Why t-tests on simulation draws are meaningless: they scale with N
# -----------------------------------------------------------------------------
mc <- do.call(rbind, lapply(c(1e3, 1e4, 1e5, N), function(n) {
  x <- milei2[seq_len(n)]; p <- mean(x > 50)
  tt <- t.test(100 * D2[seq_len(n), "MILEI"] / rowSums(D2[seq_len(n), c("MILEI", "MASSA", "BLANCO")]),
               100 * D2[seq_len(n), "MASSA"] / rowSums(D2[seq_len(n), c("MILEI", "MASSA", "BLANCO")]))
  data.frame(n = n, p_milei = p, mc_se = sqrt(p * (1 - p) / n),
             q025 = quantile(x, .025), q975 = quantile(x, .975),
             welch_t = unname(tt$statistic), welch_p = tt$p.value)
}))
write.csv(mc, file.path(OUT, "tables/monte_carlo_size.csv"), row.names = FALSE)
K$TsmallN <- fmt(mc$welch_t[1], 1); K$TlargeN <- fmt(mc$welch_t[nrow(mc)], 1)

# -----------------------------------------------------------------------------
# 5. Figures
# -----------------------------------------------------------------------------
pal <- c(MASSA = "#3189e0", MILEI = "#9e57ad", BULLRICH = "#d9b600",
         SCHIARETTI = "#314fe0", BREGMAN = "#e0316c")
thin <- S_pos[sample(N, 50000), ]
long <- data.frame(candidate = factor(rep(colnames(thin), each = nrow(thin)), levels = FIRST_ROUND_TICKETS),
                   share = as.vector(thin))
f1 <- ggplot(long, aes(share, fill = candidate)) +
  geom_density(alpha = .55, colour = NA) +
  geom_vline(data = data.frame(candidate = factor(FIRST_ROUND_TICKETS, levels = FIRST_ROUND_TICKETS),
                               actual = act_g[FIRST_ROUND_TICKETS]),
             aes(xintercept = actual, colour = candidate), linetype = 2, linewidth = .6) +
  scale_fill_manual(values = pal) + scale_colour_manual(values = pal, guide = "none") +
  labs(x = "First-round share of positive votes (%)", y = "Density", fill = NULL,
       caption = "Dashed lines: official result, 22 October 2023.") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(OUT, "figures/fig_first_round_density.pdf"), f1, width = 7, height = 4.2)
ggsave(file.path(OUT, "figures/fig_first_round_density.png"), f1, width = 7, height = 4.2, dpi = 300)

top <- head(tab_out, 7)
top$outcome <- factor(top$outcome, levels = rev(top$outcome))
f2 <- ggplot(top, aes(p_legal, outcome)) +
  geom_col(fill = "grey60") +
  geom_col(data = subset(top, outcome == realised_fr), fill = "#3189e0") +
  geom_text(aes(label = scales::percent(p_legal, 1)), hjust = -0.15, size = 3.3) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, .15))) +
  labs(x = "Share of simulations", y = NULL, caption = "Highlighted: realised outcome.") +
  theme_minimal(base_size = 11)
ggsave(file.path(OUT, "figures/fig_first_round_outcomes.pdf"), f2, width = 6.5, height = 3.5)
ggsave(file.path(OUT, "figures/fig_first_round_outcomes.png"), f2, width = 6.5, height = 3.5, dpi = 300)

f3 <- ggplot(data.frame(m = milei2[seq_len(50000)]), aes(m)) +
  geom_histogram(aes(fill = m > 50), binwidth = .25, boundary = 50) +
  scale_fill_manual(values = c(`TRUE` = "#9e57ad", `FALSE` = "#3189e0"),
                    labels = c(`TRUE` = "Milei wins", `FALSE` = "Massa wins"), name = NULL) +
  geom_vline(xintercept = 50) +
  geom_vline(xintercept = act_r[["MILEI"]], linetype = 2) +
  annotate("text", x = act_r[["MILEI"]], y = Inf, vjust = 1.5, hjust = -0.05, size = 3.3,
           label = paste0("Official: ", fmt(act_r[["MILEI"]], 2), "%")) +
  labs(x = "Milei two-candidate share in the runoff (%)", y = "Simulations") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(OUT, "figures/fig_runoff.pdf"), f3, width = 6.5, height = 3.8)
ggsave(file.path(OUT, "figures/fig_runoff.png"), f3, width = 6.5, height = 3.8, dpi = 300)

# -----------------------------------------------------------------------------
# 6. Key numbers -> LaTeX macros
# -----------------------------------------------------------------------------
K$NSims <- format(N, big.mark = ",")
macro <- sprintf("\\newcommand{\\%s}{%s}", names(K), unlist(K))
writeLines(c("% Auto-generated by paper/code/run_all.R. Do not edit by hand.", macro),
           file.path(OUT, "key_numbers.tex"))

cat("\n== First round (legal denominator) ==\n"); print(tab_fr, digits = 3)
cat("\n== Outcomes ==\n"); print(tab_out, digits = 3)
cat("\n== Naive PASO baseline ==\n"); print(round(naive, 2))
cat("\n== Robustness ==\n"); print(rob, digits = 3)
cat("\n== Monte Carlo size ==\n"); print(mc, digits = 4)
cat("\n== Key numbers ==\n"); print(unlist(K))
writeLines(capture.output(sessionInfo()), file.path(OUT, "sessionInfo.txt"))
