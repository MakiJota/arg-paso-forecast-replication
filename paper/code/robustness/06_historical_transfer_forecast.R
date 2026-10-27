# =============================================================================
# (1) Did the 2015 PASO alone point to the 2015 president? Chained agnostic
#     model (paper/code/R/agnostic.R), same as 05_stress_test.R.
# (2) Retrospective cross-cycle validation, 2015 -> 2023: 2015 PASO->runoff
#     transfers (ecological inference, 04_ei_2015.R) mapped to 2023 by ROLE.
#     NB: the role mapping was chosen after the 2023 result was known, and it
#     conditions on the realised Massa-Milei pairing; it is not an ex-ante forecast.
# (3) Reverse test, 2023 -> 2015: 2023 PASO->runoff transfers (02_ecological_
#     inference.R) mapped to 2015 by the same roles.
#
# Role mapping (runoff):
#   Kirchnerist (incumbent-coalition) finalist     SCIOLI 2015 <-> MASSA 2023
#   Opposition finalist                            MACRI 2015  <-> MILEI 2023
#   Internal losers of a finalist's coalition      SANZ, CARRIO 2015 <-> GRABOIS 2023
#   Eliminated non-Kirchnerist electorates (incl. dissident Peronists)
#        MASSA, DELASOTA, STOLBIZER, RODRIGUEZSAA 2015 <-> BULLRICH, LARRETA, SCHIARETTI 2023
#   Eliminated left                                DELCANO, ALTAMIRA 2015 <-> BREGMAN 2023
#   OTROS, BLANCO, ANULADOS, NOVOTO                same category
# Columns: to Kirchnerist finalist / to opposition finalist / blank / null / abstention.
# Ranges: min-max across the 2015 members of each pooled role (and lphom vs nslphom);
# centre = vote-weighted mean.
# Outputs: paper/output/tables/historical_transfer_*.csv, key_numbers_historical.tex
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })
source("paper/code/R/model.R")
set.seed(20151122)
N <- 50000
OUT <- "paper/output"

# ---------------------------------------------------------------- 2015 inputs
chk <- fread(file.path(OUT, "tables/ei2015_national_check.csv"))
paso15 <- chk[election == "paso" & !item %in% c("electores", "votantes", "mesas"), setNames(national, item)]
paso15[["NOVOTO"]] <- chk[election == "paso" & item == "electores", national] -
                      chk[election == "paso" & item == "votantes", national]
ro_l <- as.matrix(fread(file.path(OUT, "tables/ei2015_runoff_matrix.csv")), rownames = 1) / 100
ro_n <- as.matrix(fread(file.path(OUT, "tables/ei2015_runoff_matrix_nslphom.csv")), rownames = 1) / 100
act15 <- c(MACRI = 51.34, SCIOLI = 48.66)   # TODO verify: CNE definitive count, 22 Nov 2015 (provisional 51.40 / 48.60)

# ---------------------------------------------------------------- (1) 2015 agnostic test (chained)
source("paper/code/R/agnostic.R")
TICKETS15 <- c("SCIOLI", "MACRI", "MASSA", "DELCANO", "STOLBIZER", "RODRIGUEZSAA")
HOME15 <- c(SCIOLI = "SCIOLI", MACRI = "MACRI", SANZ = "MACRI", CARRIO = "MACRI",
            MASSA = "MASSA", DELASOTA = "MASSA", DELCANO = "DELCANO", ALTAMIRA = "DELCANO",
            STOLBIZER = "STOLBIZER", RODRIGUEZSAA = "RODRIGUEZSAA", OTROS = NA,
            BLANCO = "BLANCO", ANULADOS = "ANULADOS", NOVOTO = "NOVOTO")
lv <- list(`Low change (85-95% stay)` = c(.85, .95), `Mid change (60-80% stay)` = c(.60, .80),
           `High change (30-60% stay)` = c(.30, .60), `Chaos (0% stay)` = c(0, 0))
t15 <- rbindlist(lapply(names(lv), function(nm) {
  cyc <- agnostic_cycle(paso15[names(HOME15)], HOME15, TICKETS15, N, lv[[nm]][1], lv[[nm]][2])
  mm <- cyc$pair %in% "MACRI-SCIOLI"
  data.table(scenario = nm, p_runoff = mean(!cyc$outright), p_scioli_macri = mean(mm),
             p_macri_wins_given_pair = mean(cyc$president[mm] == "MACRI"),
             p_macri_president = mean(cyc$president == "MACRI"),
             p_scioli_president = mean(cyc$president == "SCIOLI"),
             p_massa_president = mean(cyc$president == "MASSA"))
}))
# The EI matrix applied to the 2015 PASO (sanity: should reproduce the result)
v15 <- paso15[rownames(ro_l)]
y15 <- as.vector(v15 %*% ro_l); names(y15) <- colnames(ro_l)
cat("\n== (1) 2015: agnostic PASO-only model, Macri two-candidate share ==\n"); print(t15, digits = 3)
cat("EI matrix reproduces Macri =", round(100 * y15["MACRI"] / (y15["MACRI"] + y15["SCIOLI"]), 2), "% (official", act15[["MACRI"]], ")\n")
fwrite(t15, file.path(OUT, "tables/historical_transfer_2015_agnostic.csv"))

# ---------------------------------------------------------------- (2) role-based 2023 forecast
roles <- list(KIRCHNERIST_FINALIST = "SCIOLI", OPPOSITION_FINALIST = "MACRI",
              FINALIST_INTERNAL_LOSER = c("SANZ", "CARRIO"),
              ELIMINATED_NONKIRCHNERIST = c("MASSA", "DELASOTA", "STOLBIZER", "RODRIGUEZSAA"),
              ELIMINATED_LEFT = c("DELCANO", "ALTAMIRA"),
              # alternative roles for robustness
              MASSA2015_ONLY = "MASSA",                                     # most even 2015 split
              DISSIDENT_PERONIST_ONLY = c("MASSA", "DELASOTA", "RODRIGUEZSAA"),
              OTROS = "OTROS", BLANCO = "BLANCO", ANULADOS = "ANULADOS", NOVOTO = "NOVOTO")
# re-express 2015 columns as role columns
recol <- function(M) cbind(KFINALIST = M[, "SCIOLI"], OPPFINALIST = M[, "MACRI"],
                           BLANCO = M[, "BLANCO"], ANULADOS = M[, "ANULADOS"], NOVOTO = M[, "NOVOTO"])
Ml <- recol(ro_l); Mn <- recol(ro_n)
role_rows <- rbindlist(lapply(names(roles), function(r) {
  mem <- roles[[r]]; w <- paso15[mem] / sum(paso15[mem])
  centre <- colSums(Ml[mem, , drop = FALSE] * w)
  lo <- pmin(apply(Ml[mem, , drop = FALSE], 2, min), apply(Mn[mem, , drop = FALSE], 2, min))
  hi <- pmax(apply(Ml[mem, , drop = FALSE], 2, max), apply(Mn[mem, , drop = FALSE], 2, max))
  data.table(role = r, members = paste(mem, collapse = "+"), dest = names(centre), centre = centre, lo = lo, hi = hi)
}))
# For a pooled role with one member, give the range at least +/- 5 pts around the centre
role_rows[, `:=`(lo = pmin(lo, pmax(centre - .05, 0)), hi = pmax(hi, pmin(centre + .05, 1)))]
fwrite(role_rows, file.path(OUT, "tables/historical_transfer_role_ranges.csv"))

base_map <- c(MILEI = "OPPOSITION_FINALIST", MASSA = "KIRCHNERIST_FINALIST", GRABOIS = "FINALIST_INTERNAL_LOSER",
              BULLRICH = "ELIMINATED_NONKIRCHNERIST", LARRETA = "ELIMINATED_NONKIRCHNERIST", SCHIARETTI = "ELIMINATED_NONKIRCHNERIST",
              BREGMAN = "ELIMINATED_LEFT", OTROS = "OTROS", BLANCO = "BLANCO", ANULADOS = "ANULADOS", NOVOTO = "NOVOTO")
# Note on GRABOIS: in 2015 the internal losers belonged to the NON-Peronist finalist's coalition,
# so their "own finalist" flow is the OPPFINALIST column; for Grabois we map own-finalist -> MASSA.
mappings <- list(
  `Main: pooled eliminated non-left electorates` = base_map,
  `Alt 1: Schiaretti as dissident Peronist (Massa+De la Sota+Rodriguez Saa 2015)` =
    replace(base_map, "SCHIARETTI", "DISSIDENT_PERONIST_ONLY"),
  `Alt 2: all eliminated electorates behave like Massa 2015 (most even split)` =
    replace(base_map, c("BULLRICH", "LARRETA", "SCHIARETTI"), "MASSA2015_ONLY"))

votes <- read_paso("paper/data/paso_2023.csv")
DEST23 <- c("MILEI", "MASSA", "BLANCO", "ANULADOS", "NOVOTO")
forecast_with <- function(map23) {
  lo23 <- hi23 <- matrix(0, length(CATS), length(DEST23), dimnames = list(CATS, DEST23))
  for (o in CATS) {
    rr <- role_rows[role == map23[[o]]]
    get <- function(col, d) rr[dest == d][[col]]
    to_milei <- if (o == "GRABOIS") "KFINALIST" else "OPPFINALIST"
    to_massa <- if (o == "GRABOIS") "OPPFINALIST" else "KFINALIST"
    for (col in c("lo", "hi")) {
      vals <- c(MILEI = get(col, to_milei), MASSA = get(col, to_massa), BLANCO = get(col, "BLANCO"),
                ANULADOS = get(col, "ANULADOS"), NOVOTO = get(col, "NOVOTO"))
      if (col == "lo") lo23[o, ] <- vals[DEST23] else hi23[o, ] <- vals[DEST23]
    }
  }
  # simulate: uniform within role ranges, renormalised (same generator as the main model)
  K <- length(DEST23); lo_v <- as.vector(lo23); hi_v <- as.vector(hi23)
  U <- matrix(runif(N * length(CATS) * K, rep(lo_v, each = N), rep(hi_v, each = N)), N)
  D <- matrix(0, N, K, dimnames = list(NULL, DEST23))
  for (i in seq_along(CATS)) {
    idx <- i + (seq_len(K) - 1) * length(CATS)
    W <- U[, idx, drop = FALSE]; W[rowSums(W) == 0, ] <- 1
    D <- D + votes[[CATS[i]]] * W / rowSums(W)
  }
  100 * D[, "MILEI"] / (D[, "MILEI"] + D[, "MASSA"])
}
res_all <- rbindlist(lapply(names(mappings), function(nm) {
  milei <- forecast_with(mappings[[nm]])
  data.table(model = nm, milei_mean = mean(milei), milei_q05 = quantile(milei, .05),
             milei_q95 = quantile(milei, .95), p_milei_wins = mean(milei > 50), actual = 55.65)
}))
res23 <- res_all[1]
cat("\n== (2) 2023 runoff forecast from 2015 role-based transfers ==\n"); print(res_all, digits = 3)
fwrite(res_all, file.path(OUT, "tables/historical_transfer_2023_forecast.csv"))

# ---------------------------------------------------------------- (3) reverse test: 2023 -> 2015
ei23 <- readRDS(file.path(OUT, "validation_lphom.rds"))$run$ei           # rows CATS; MILEI, MASSA, BLANCO, ANULADOS, NOVOTO
v23 <- read_paso("paper/data/paso_2023.csv")
# 2023 rows in role terms: own finalist / other finalist (for finalists and internal losers),
# or Kirchnerist / opposition finalist (for everyone else)
row_k_opp <- function(o) c(KFIN = ei23[o, "MASSA"], OPP = ei23[o, "MILEI"], BLANCO = ei23[o, "BLANCO"],
                          ANULADOS = ei23[o, "ANULADOS"], NOVOTO = ei23[o, "NOVOTO"])
pool <- function(members) {
  M <- t(sapply(members, row_k_opp)); w <- v23[members] / sum(v23[members])
  list(centre = colSums(M * w), lo = apply(M, 2, min), hi = apply(M, 2, max))
}
roles23 <- list(KFIN = pool("MASSA"), OPP = pool("MILEI"),
                LOSER_OWN_K = pool("GRABOIS"),                     # Grabois: own finalist = Kirchnerist
                ELIM_NONK = pool(c("BULLRICH", "LARRETA", "SCHIARETTI")),
                LEFT = pool("BREGMAN"), OTROS = pool("OTROS"), BLANCO = pool("BLANCO"),
                ANULADOS = pool("ANULADOS"), NOVOTO = pool("NOVOTO"))
widen5 <- function(r) list(lo = pmin(r$lo, pmax(r$centre - .05, 0)), hi = pmax(r$hi, pmin(r$centre + .05, 1)))
map15 <- c(SCIOLI = "KFIN", MACRI = "OPP", SANZ = "LOSER_OWN_K", CARRIO = "LOSER_OWN_K",
           MASSA = "ELIM_NONK", DELASOTA = "ELIM_NONK", STOLBIZER = "ELIM_NONK", RODRIGUEZSAA = "ELIM_NONK",
           DELCANO = "LEFT", ALTAMIRA = "LEFT", OTROS = "OTROS", BLANCO = "BLANCO", ANULADOS = "ANULADOS", NOVOTO = "NOVOTO")
DEST15 <- c("SCIOLI", "MACRI", "BLANCO", "ANULADOS", "NOVOTO")
D15 <- matrix(0, N, 5, dimnames = list(NULL, DEST15))
for (o in names(map15)) {
  r <- widen5(roles23[[map15[[o]]]])
  lo <- r$lo; hi <- r$hi
  if (map15[[o]] == "LOSER_OWN_K") {   # Sanz/Carrio: their own finalist is Macri (opposition)
    lo[c("KFIN", "OPP")] <- lo[c("OPP", "KFIN")]; hi[c("KFIN", "OPP")] <- hi[c("OPP", "KFIN")]
  }
  names(lo) <- names(hi) <- DEST15                  # KFIN -> SCIOLI, OPP -> MACRI
  W <- matrix(runif(N * 5, rep(lo, each = N), rep(hi, each = N)), N); W[rowSums(W) == 0, ] <- 1
  D15 <- D15 + paso15[[o]] * W / rowSums(W)
}
macri <- 100 * D15[, "MACRI"] / (D15[, "MACRI"] + D15[, "SCIOLI"])
rev15 <- data.table(model = "2023 role-based transfers applied to the 2015 PASO", macri_mean = mean(macri),
                    macri_q05 = quantile(macri, .05), macri_q95 = quantile(macri, .95),
                    p_macri_wins = mean(macri > 50), actual = act15[["MACRI"]])
cat("\n== (3) Reverse test: 2015 runoff from 2023 role-based transfers ==\n"); print(rev15, digits = 3)
fwrite(rev15, file.path(OUT, "tables/historical_transfer_2015_reverse.csv"))

fmt <- function(x, d = 0) formatC(unname(x), format = "f", digits = d)
pct <- function(x) ifelse(x > .99, ">99", fmt(100 * x))
Kn <- c(FifteenAgnosticMacriLow  = fmt(100 * t15[scenario == "Low change (85-95% stay)", p_macri_president]),
        FifteenAgnosticMacriMid  = fmt(100 * t15[scenario == "Mid change (60-80% stay)", p_macri_president]),
        FifteenAgnosticMacriHigh = fmt(100 * t15[scenario == "High change (30-60% stay)", p_macri_president]),
        FifteenAgnosticSciolLow  = fmt(100 * t15[scenario == "Low change (85-95% stay)", p_scioli_president]),
        HistTransferMileiMean = fmt(res23$milei_mean, 1),
        HistTransferMileiLo = fmt(res23$milei_q05, 1), HistTransferMileiHi = fmt(res23$milei_q95, 1),
        HistTransferPMilei = pct(res23$p_milei_wins),
        ReverseMacriMean = fmt(rev15$macri_mean, 1), ReverseMacriLo = fmt(rev15$macri_q05, 1),
        ReverseMacriHi = fmt(rev15$macri_q95, 1), ReversePMacri = pct(rev15$p_macri_wins),
        ActualMacriFifteen = fmt(act15[["MACRI"]], 2),
        HistTransferAltOneMean = fmt(res_all[2]$milei_mean, 1), HistTransferAltOneP = pct(res_all[2]$p_milei_wins),
        HistTransferAltTwoMean = fmt(res_all[3]$milei_mean, 1), HistTransferAltTwoP = pct(res_all[3]$p_milei_wins))
writeLines(c("% Auto-generated by paper/code/robustness/06_historical_transfer_forecast.R",
             sprintf("\\newcommand{\\%s}{%s}", names(Kn), Kn)), file.path(OUT, "key_numbers_historical.tex"))
