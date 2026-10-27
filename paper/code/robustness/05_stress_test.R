# =============================================================================
# Stress test: how much of the 2023 outcome is determined by the PASO itself,
# and how much by the transfer assumptions? First round and runoff are CHAINED:
# the runoff is played between the two finalists of each draw, so nothing is
# conditioned on the realised Massa-Milei pairing.
#
#   A. Author's ranges widened k = 1, 1.5, 2, 3 times around their centre.
#      First round from the (widened) first-round ranges. The author specified
#      runoff ranges only for Massa-Milei: those draws use the (widened) runoff
#      ranges; draws with any other pairing use the agnostic low-change runoff
#      ("hybrid"). The conditional P(Milei wins | Massa-Milei) is also reported.
#   B. Agnostic retention + noise (paper/code/R/agnostic.R), low / mid / high
#      change and chaos (rho = 0).
# Outputs: paper/output/tables/stress_test.csv, figures/fig_stress_test.*,
#          paper/output/key_numbers_stress.tex
# Run from the repository root.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
source("paper/code/R/model.R")
source("paper/code/R/agnostic.R")
set.seed(20230814)
N   <- 50000
OUT <- "paper/output"
votes <- read_paso("paper/data/paso_2023.csv")            # provisional: as on PASO night
actual <- read.csv("paper/data/actual_results_2023.csv")
act_g <- with(subset(actual, election == "general"), setNames(pct_positive, candidate))

HOME <- c(MILEI = "MILEI", BULLRICH = "BULLRICH", LARRETA = "BULLRICH", MASSA = "MASSA",
          GRABOIS = "MASSA", SCHIARETTI = "SCHIARETTI", BREGMAN = "BREGMAN", OTROS = NA,
          BLANCO = "BLANCO", ANULADOS = "ANULADOS", NOVOTO = "NOVOTO")
DEST_R <- c("MILEI", "MASSA", "BLANCO", "ANULADOS", "NOVOTO")
LOW <- c(.85, .95)

widen <- function(lo, hi, k) { c <- (lo + hi) / 2; h <- (hi - lo) / 2; list(lo = c - k * h, hi = c + k * h) }
collapse_dest <- function(D11, dest) {
  gone <- setdiff(colnames(D11), dest)
  D <- D11[, dest, drop = FALSE]; D[, "NOVOTO"] <- D[, "NOVOTO"] + rowSums(D11[, gone, drop = FALSE]); D
}

summarise_cycle <- function(fr_shares, cl, president, mm_milei, family, scenario) {
  data.table(family = family, scenario = scenario,
             p_runoff = mean(!cl$win),
             p_massa_milei = mean(!cl$win & paste(pmin(cl$first, cl$second), pmax(cl$first, cl$second)) == "MASSA MILEI"),
             p_massa_first = mean(cl$first == "MASSA"), p_milei_first = mean(cl$first == "MILEI"),
             mae_first_round = mean(abs(colMeans(fr_shares) - act_g[FIRST_ROUND_TICKETS])),
             p_milei_wins_given_mm = mean(mm_milei),
             p_milei_president = mean(president == "MILEI"),
             p_massa_president = mean(president == "MASSA"),
             p_bullrich_president = mean(president == "BULLRICH"))
}

fr_lo <- read_range("paper/data/ranges_first_round_min.csv"); fr_hi <- read_range("paper/data/ranges_first_round_max.csv")
ro_lo <- read_range("paper/data/ranges_runoff_min.csv");      ro_hi <- read_range("paper/data/ranges_runoff_max.csv")

res <- list()
# ---------------------------------------------------------------- A. author ranges (hybrid)
for (k in c(1, 1.5, 2, 3)) {
  g <- widen(fr_lo, fr_hi, k); r <- widen(ro_lo, ro_hi, k)
  S <- shares(simulate_uniform(votes, g$lo, g$hi, N), FIRST_ROUND_TICKETS)
  cl <- classify_first_round(S)
  president <- ifelse(cl$win, cl$first, NA_character_)
  mm <- which(!cl$win & ((cl$first == "MASSA" & cl$second == "MILEI") | (cl$first == "MILEI" & cl$second == "MASSA")))
  Dr <- collapse_dest(simulate_uniform(votes, r$lo, r$hi, length(mm)), DEST_R)
  mm_milei <- Dr[, "MILEI"] > Dr[, "MASSA"]
  president[mm] <- ifelse(mm_milei, "MILEI", "MASSA")
  oth <- which(!cl$win & is.na(president))
  for (p in unique(paste(cl$first[oth], cl$second[oth]))) {
    idx <- oth[paste(cl$first[oth], cl$second[oth]) == p]; f <- strsplit(p, " ")[[1]]
    D2 <- transfer_agnostic(votes, HOME, c(f, "BLANCO", "ANULADOS", "NOVOTO"), length(idx), LOW[1], LOW[2])
    president[idx] <- ifelse(D2[, f[1]] > D2[, f[2]], f[1], f[2])
  }
  res[[length(res) + 1]] <- summarise_cycle(S, cl, president, mm_milei, "A. Author ranges widened", sprintf("x%.1f", k))
}
# ---------------------------------------------------------------- B. agnostic, chained
levels_b <- list(`Low change (85-95% stay)` = c(.85, .95), `Mid change (60-80% stay)` = c(.60, .80),
                 `High change (30-60% stay)` = c(.30, .60), `Chaos (0% stay)` = c(0, 0))
for (nm in names(levels_b)) {
  rr <- levels_b[[nm]]
  others <- c("BLANCO", "ANULADOS", "NOVOTO")
  D1 <- transfer_agnostic(votes, HOME, c(FIRST_ROUND_TICKETS, others), N, rr[1], rr[2])
  S <- 100 * D1[, FIRST_ROUND_TICKETS] / rowSums(D1[, FIRST_ROUND_TICKETS])
  cl <- classify_first_round(S)
  president <- ifelse(cl$win, cl$first, NA_character_)
  mm_milei <- logical(0)
  for (p in unique(paste(cl$first[!cl$win], cl$second[!cl$win]))) {
    idx <- which(!cl$win & paste(cl$first, cl$second) == p); f <- strsplit(p, " ")[[1]]
    D2 <- transfer_agnostic(votes, HOME, c(f, others), length(idx), rr[1], rr[2])
    president[idx] <- ifelse(D2[, f[1]] > D2[, f[2]], f[1], f[2])
    if (all(sort(f) == c("MASSA", "MILEI"))) mm_milei <- c(mm_milei, president[idx] == "MILEI")
  }
  res[[length(res) + 1]] <- summarise_cycle(S, cl, president, mm_milei, "B. Agnostic retention + noise", nm)
}
tab <- rbindlist(res)
fwrite(tab, file.path(OUT, "tables/stress_test.csv"))
print(tab[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

# ---------------------------------------------------------------- figure
long <- melt(tab, id.vars = c("family", "scenario"),
             measure.vars = c("p_massa_first", "p_massa_milei", "p_milei_president"),
             variable.name = "metric", value.name = "p")
long[, metric := factor(metric, levels = c("p_massa_first", "p_massa_milei", "p_milei_president"),
                        labels = c("Massa first in round 1", "Massa-Milei runoff", "Milei becomes president"))]
long[, scenario := factor(scenario, levels = unique(tab$scenario))]
p <- ggplot(long, aes(scenario, p, fill = metric)) +
  geom_col(position = position_dodge(.8), width = .75) +
  geom_hline(yintercept = .5, linetype = 2, colour = "grey40") +
  facet_grid(~ family, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("#3189e0", "#7f7f7f", "#9e57ad"), name = NULL) +
  labs(x = NULL, y = "Share of simulations",
       caption = "First round and runoff chained: each draw's runoff is between that draw's finalists. All three events occurred in 2023.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(OUT, "figures/fig_stress_test.png"), p, width = 10, height = 4.5, dpi = 300)
ggsave(file.path(OUT, "figures/fig_stress_test.pdf"), p, width = 10, height = 4.5)

# ---------------------------------------------------------------- key numbers
fmt <- function(x) formatC(100 * unname(x), format = "f", digits = 0)
g <- function(s, m) tab[scenario == s][[m]]
K <- c(StressPresMainHybrid = fmt(g("x1.0", "p_milei_president")),
       StressMassaPresMain  = fmt(g("x1.0", "p_massa_president")),
       StressBullPresMain   = fmt(g("x1.0", "p_bullrich_president")),
       StressPresWideThree  = fmt(g("x3.0", "p_milei_president")),
       StressCondMain       = fmt(g("x1.0", "p_milei_wins_given_mm")),
       StressPresLow   = fmt(g("Low change (85-95% stay)", "p_milei_president")),
       StressPresMid   = fmt(g("Mid change (60-80% stay)", "p_milei_president")),
       StressPresHigh  = fmt(g("High change (30-60% stay)", "p_milei_president")),
       StressPresChaos = fmt(g("Chaos (0% stay)", "p_milei_president")),
       StressMMLow     = fmt(g("Low change (85-95% stay)", "p_massa_milei")),
       StressCondLow   = fmt(g("Low change (85-95% stay)", "p_milei_wins_given_mm")),
       StressMassaPresLow = fmt(g("Low change (85-95% stay)", "p_massa_president")),
       StressBullPresLow  = fmt(g("Low change (85-95% stay)", "p_bullrich_president")),
       StressMassaFirstLow = fmt(g("Low change (85-95% stay)", "p_massa_first")),
       StressMassaFirstHigh = fmt(g("High change (30-60% stay)", "p_massa_first")))
writeLines(c("% Auto-generated by paper/code/robustness/05_stress_test.R",
             sprintf("\\newcommand{\\%s}{%s}", names(K), K)), file.path(OUT, "key_numbers_stress.tex"))
