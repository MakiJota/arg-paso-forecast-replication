# 03_paso_history.R
# How well did the national PASO presidential result predict the general
# election (and runoff) in 2011, 2015, 2019 and 2023?
#
# Input : paper/data/raw/history_api/res_<year>_e<id>_c1.json, written by
#         paper/code/validation/03_paso_history.py (DINE results API,
#         provisional count). e1 = PASO, e2 = general, e3 = runoff.
# Output: paper/data/paso_history_national.csv
#         paper/output/tables/paso_history_*.csv
#         paper/output/figures/fig_paso_history.{png,pdf}
#         paper/output/key_numbers_history.tex
# Run from the repo root: Rscript paper/code/validation/03_paso_history.R

suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(ggplot2); library(ggrepel)
})

raw_dir <- file.path("paper", "data", "raw", "history_api")
tab_dir <- file.path("paper", "output", "tables")
fig_dir <- file.path("paper", "output", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

years <- c(2011, 2015, 2019, 2023)
elec_lab <- c(`1` = "paso", `2` = "general", `3` = "runoff")

norm_name <- function(x) {
  x <- enc2utf8(trimws(x))
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9() .-]", "", x)
  toupper(gsub("\\s+", " ", trimws(x)))
}

# ---- 1. read raw API responses ---------------------------------------------
read_res <- function(y, e) {
  f <- file.path(raw_dir, sprintf("res_%d_e%d_c1.json", y, e))
  # a district-aggregated national file is used only when the national
  # query came back empty (see 03_paso_history.py)
  fa <- file.path(raw_dir, sprintf("res_%d_e%d_c1_aggregated.json", y, e))
  if (file.exists(fa)) f <- fa
  if (!file.exists(f)) return(NULL)
  d <- fromJSON(f, simplifyVector = FALSE)
  er <- d$estadoRecuento; ot <- d$valoresTotalizadosOtros
  pos <- d$valoresTotalizadosPositivos
  if (length(pos) == 0 || is.null(er$cantidadVotantes) || er$cantidadVotantes == 0) {
    message(sprintf("WARNING: %d election %d returned no data (%s)", y, e, f))
    return(NULL)
  }
  parties <- rbindlist(lapply(pos, function(a) data.table(
    agrupacion_raw = trimws(a$nombreAgrupacion),
    votos = as.numeric(a$votos),
    n_listas = length(a$listas))))
  parties[, `:=`(year = y, election = elec_lab[as.character(e)],
                 agrupacion_norm = norm_name(agrupacion_raw))]
  totals <- data.table(
    year = y, election = elec_lab[as.character(e)],
    source_file = basename(f),
    mesas = er$mesasTotalizadas,
    electores = er$cantidadElectores, votantes = er$cantidadVotantes,
    positivos = sum(parties$votos),
    blanco = ot$votosEnBlanco, nulos = ot$votosNulos,
    recurridos_impugnados = ot$votosRecurridosComandoImpugnados)
  list(parties = parties, totals = totals)
}

res <- list()
for (y in years) for (e in 1:3) {
  r <- read_res(y, e)
  if (!is.null(r)) res[[length(res) + 1]] <- r
}
parties <- rbindlist(lapply(res, `[[`, "parties"))
totals  <- rbindlist(lapply(res, `[[`, "totals"))
parties[, share_all := 100 * votos / sum(votos), by = .(year, election)]

# ---- 2. coalition matching (manual, documented) -----------------------------
# key: short label; paso/general/runoff: regex on the normalised API name.
match_spec <- rbindlist(list(
  data.table(year = 2011, label = "FPV (Fernandez de Kirchner)",
             short = "FPV",  pat = "^ALIANZA FRENTE PARA LA VICTORIA$"),
  data.table(year = 2011, label = "FAP (Binner)", short = "FAP",
             pat = "^ALIANZA FRENTE AMPLIO PROGRESISTA$"),
  data.table(year = 2011, label = "UDESO (Alfonsin)", short = "UDESO",
             pat = "^ALIANZA UNION PARA EL DESARROLLO SOCIAL"),
  data.table(year = 2011, label = "Compromiso Federal (Rodriguez Saa)", short = "CF",
             pat = "^ALIANZA COMPROMISO FEDERAL$"),
  data.table(year = 2011, label = "Frente Popular (Duhalde)", short = "FP",
             pat = "^ALIANZA FRENTE POPULAR$"),
  data.table(year = 2011, label = "Coalicion Civica ARI (Carrio)", short = "CC-ARI",
             pat = "^COALICION CIVICA"),
  data.table(year = 2011, label = "FIT (Altamira)", short = "FIT",
             pat = "^ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES$"),

  data.table(year = 2015, label = "FPV (Scioli)", short = "FPV",
             pat = "^(ALIANZA )?FRENTE PARA LA VICTORIA$"),
  data.table(year = 2015, label = "Cambiemos (Macri)", short = "Cambiemos",
             pat = "^(ALIANZA )?CAMBIEMOS$"),
  data.table(year = 2015, label = "UNA (Massa)", short = "UNA",
             pat = "^ALIANZA UNIDOS POR UNA NUEVA ALTERNATIVA"),
  data.table(year = 2015, label = "Progresistas (Stolbizer)", short = "Progresistas",
             pat = "^ALIANZA PROGRESISTAS$"),
  data.table(year = 2015, label = "FIT (Del Cano)", short = "FIT",
             pat = "^ALIANZA FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES$"),
  data.table(year = 2015, label = "Compromiso Federal (Rodriguez Saa)", short = "CF",
             pat = "^ALIANZA COMPROMISO FEDERAL$"),

  data.table(year = 2019, label = "Frente de Todos (Fernandez)", short = "FdT",
             pat = "^FRENTE DE TODOS$"),
  data.table(year = 2019, label = "Juntos por el Cambio (Macri)", short = "JxC",
             pat = "^JUNTOS POR EL CAMBIO$"),
  data.table(year = 2019, label = "Consenso Federal (Lavagna)", short = "CF",
             pat = "^CONSENSO FEDERAL$"),
  data.table(year = 2019, label = "FIT-Unidad (Del Cano)", short = "FIT-U",
             pat = "^(FIT|FRENTE DE IZQUIERDA Y DE (LOS )?TRABAJADORES)( - |-| )UNIDAD$"),
  data.table(year = 2019, label = "Frente NOS (Gomez Centurion)", short = "NOS",
             pat = "^FRENTE NOS$"),
  data.table(year = 2019, label = "Unite (Espert)", short = "Unite",
             pat = "^UNITE"),

  data.table(year = 2023, label = "La Libertad Avanza (Milei)", short = "LLA",
             pat = "^LA LIBERTAD AVANZA$"),
  data.table(year = 2023, label = "Union por la Patria (Massa)", short = "UxP",
             pat = "^UNION POR LA PATRIA$"),
  data.table(year = 2023, label = "Juntos por el Cambio (Bullrich)", short = "JxC",
             pat = "^JUNTOS POR EL CAMBIO$"),
  data.table(year = 2023, label = "Hacemos por Nuestro Pais (Schiaretti)", short = "HNP",
             pat = "^HACEMOS POR NUESTRO PAIS$"),
  data.table(year = 2023, label = "FIT-Unidad (Bregman)", short = "FIT-U",
             pat = "^FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD$")
))

parties[, short := NA_character_]
for (i in seq_len(nrow(match_spec))) {
  m <- match_spec[i]
  idx <- parties$year == m$year & grepl(m$pat, parties$agrupacion_norm)
  if (any(!is.na(parties$short[idx]))) stop("double match: ", m$short, " ", m$year)
  parties$short[idx] <- m$short
}

# matching table: raw API names in each round
matching <- dcast(
  parties[!is.na(short), .(year, short, election, agrupacion_raw)],
  year + short ~ election, value.var = "agrupacion_raw")
matching <- merge(match_spec[, .(year, short, label)], matching,
                  by = c("year", "short"), all.x = TRUE, sort = FALSE)
for (col in setdiff(c("paso", "general", "runoff"), names(matching)))
  matching[, (col) := NA_character_]
setcolorder(matching, c("year", "short", "label", "paso", "general", "runoff"))
fwrite(matching, file.path(tab_dir, "paso_history_matching.csv"))

# sanity: every general-ballot party must be matched to a PASO party
gen_unmatched <- parties[election == "general" & is.na(short)]
if (nrow(gen_unmatched)) {
  print(gen_unmatched); stop("general-election party without a PASO match")
}
# ---- 2b. 2023: replace provisional with CNE definitive counts -----------------
# (the paper's official series; see paper/data/README.md). Coalition totals:
def23 <- rbindlist(list(
  data.table(election = "paso", short = c("LLA", "JxC", "UxP", "HNP", "FIT-U"),
             votos = c(7352244, 6895941, 6719042, 914812, 642773), positivos = 23269296),
  data.table(election = "general", short = c("UxP", "LLA", "JxC", "HNP", "FIT-U"),
             votos = c(9853492, 8034990, 6379023, 1802068, 722061), positivos = 26791634),
  data.table(election = "runoff", short = c("LLA", "UxP"),
             votos = c(14554560, 11598720), positivos = 14554560 + 11598720)))
for (i in seq_len(nrow(def23))) {
  idx <- parties$year == 2023 & parties$election == def23$election[i] &
         !is.na(parties$short) & parties$short == def23$short[i]
  parties$votos[idx] <- def23$votos[i]
  parties$share_all[idx] <- 100 * def23$votos[i] / def23$positivos[i]
}

paso_unmatched <- parties[election == "paso" & is.na(short),
                          .(year, agrupacion_raw, votos, share_all = round(share_all, 2))]
fwrite(paso_unmatched, file.path(tab_dir, "paso_history_paso_eliminated.csv"))

# ---- 3. national table ------------------------------------------------------
p <- parties[election == "paso" & !is.na(short),
             .(year, short, paso_votes = votos, paso_share_all = share_all)]
g <- parties[election == "general" & !is.na(short),
             .(year, short, general_votes = votos)]
nat <- merge(p, g, by = c("year", "short"), all = TRUE)
nat <- merge(match_spec[, .(year, short, label)], nat, by = c("year", "short"),
             sort = FALSE)
nat[, paso_forecast := 100 * paso_votes / sum(paso_votes), by = year]
nat[, general_share := 100 * general_votes / sum(general_votes), by = year]
nat[, error := general_share - paso_forecast]
r <- parties[election == "runoff" & !is.na(short), .(year, short, runoff_votes = votos)]
nat <- merge(nat, r, by = c("year", "short"), all.x = TRUE, sort = FALSE)
nat[, runoff_share := 100 * runoff_votes / sum(runoff_votes, na.rm = TRUE), by = year]
setorder(nat, year, -paso_forecast)

nat_out <- copy(nat)
num <- c("paso_share_all", "paso_forecast", "general_share", "error", "runoff_share")
nat_out[, (num) := lapply(.SD, round, 2), .SDcols = num]
fwrite(nat_out, file.path("paper", "data", "paso_history_national.csv"))
fwrite(nat_out[, .(year, coalition = label, paso_share_all, paso_forecast,
                   general_share, error)],
       file.path(tab_dir, "paso_history_national_table.csv"))

# ---- 4. metrics -------------------------------------------------------------
outcome_class <- function(share, lab) {
  o <- order(-share); s1 <- share[o[1]]; s2 <- share[o[2]]
  if (s1 >= 45 || (s1 >= 40 && s1 - s2 >= 10)) paste0("outright: ", lab[o[1]])
  else paste0("runoff: ", lab[o[1]], " vs ", lab[o[2]])
}
top2 <- function(share, lab) paste(sort(lab[order(-share)][1:2]), collapse = "+")

metrics <- nat[, {
  op <- order(-paso_forecast); og <- order(-general_share)
  cp <- outcome_class(paso_forecast, short); cg <- outcome_class(general_share, short)
  .(n_coalitions = .N,
    mae_pp = mean(abs(error)),
    max_abs_error_pp = max(abs(error)),
    max_error_coalition = short[which.max(abs(error))],
    paso_leader = short[op[1]], general_winner = short[og[1]],
    leader_correct = short[op[1]] == short[og[1]],
    top_two_correct = top2(paso_forecast, short) == top2(general_share, short),
    order_exact = identical(short[op], short[og]),
    spearman = cor(paso_forecast, general_share, method = "spearman"),
    leader_margin_paso_pp = paso_forecast[op[1]] - paso_forecast[op[2]],
    leader_margin_general_pp = general_share[og[1]] - general_share[og[2]],
    outcome_paso_projection = cp, outcome_general = cg,
    outcome_correct = cp == cg)
}, by = year]

tt <- dcast(totals, year ~ election,
            value.var = c("electores", "votantes", "blanco", "positivos"))
tt[, `:=`(
  turnout_paso = 100 * votantes_paso / electores_paso,
  turnout_general = 100 * votantes_general / electores_general,
  blank_pct_paso = 100 * blanco_paso / votantes_paso,
  blank_pct_general = 100 * blanco_general / votantes_general)]
# Official turnout. The provisional count leaves some mesas' votes out of
# `votantes` while `electores` is the full roll, which understates turnout.
# 2011-2019: Buquet & Gallo (2022, Table 2); 2023: CNE definitive count.
official_turnout <- data.table(year = c(2011, 2015, 2019, 2023),
                               paso = c(78.66, 74.91, 76.41, 70.43),
                               general = c(79.39, 81.07, 81.31, 77.04))
tt <- merge(tt, official_turnout, by = "year")
tt[, `:=`(turnout_paso = paso, turnout_general = general)][, c("paso", "general") := NULL]
tt[, `:=`(turnout_change_pp = turnout_general - turnout_paso,
          extra_voters = votantes_general - votantes_paso,
          blank_change_pp = blank_pct_general - blank_pct_paso,
          blank_change_votes = blanco_general - blanco_paso)]
# share of PASO positive vote cast for coalitions absent from the general ballot
elim <- parties[election == "paso", .(paso_eliminated_share = sum(share_all[is.na(short)])),
                by = year]
metrics <- merge(metrics, tt[, .(year, turnout_paso, turnout_general, turnout_change_pp,
                                 extra_voters, blank_pct_paso, blank_pct_general,
                                 blank_change_pp, blank_change_votes)], by = "year")
metrics <- merge(metrics, elim, by = "year")
m_out <- copy(metrics)
nums <- names(m_out)[sapply(m_out, is.double)]
m_out[, (nums) := lapply(.SD, round, 3), .SDcols = nums]
fwrite(m_out, file.path(tab_dir, "paso_history_metrics.csv"))

tot_out <- copy(totals)
fwrite(tot_out, file.path(tab_dir, "paso_history_totals.csv"))

# ---- 5. runoffs (2015, 2023) -----------------------------------------------
ro <- nat[!is.na(runoff_votes)]
ro[, paso_share_finalists := 100 * paso_votes / sum(paso_votes), by = year]
ro[, general_share_finalists := 100 * general_votes / sum(general_votes), by = year]
ro_out <- ro[, .(year, coalition = label, paso_share_all = round(paso_share_all, 2),
                 paso_forecast = round(paso_forecast, 2),
                 paso_share_finalists = round(paso_share_finalists, 2),
                 general_share = round(general_share, 2),
                 general_share_finalists = round(general_share_finalists, 2),
                 runoff_share = round(runoff_share, 2),
                 runoff_minus_paso_finalists = round(runoff_share - paso_share_finalists, 2))]
fwrite(ro_out, file.path(tab_dir, "paso_history_runoff.csv"))

# ---- 5b. 2019 PASO cross-check ---------------------------------------------
# The national 2019 PASO query is empty in the API, so 2019 PASO is the sum of
# 525 section-level queries. Cross-check against the PolAr/electorArg
# mesa-level compilation (party columns are list codes without names; they
# are matched to API agrupaciones by vote rank, and the match is verified by
# the share differences printed below).
polar_f <- file.path(raw_dir, "polar_arg_presi_paso2019.csv")
xc <- NULL
if (file.exists(polar_f) && any(totals$source_file == "res_2019_e1_c1_aggregated.json")) {
  pl <- fread(polar_f, colClasses = list(character = 1:5))
  pcols <- grep("^[0-9]+$", names(pl), value = TRUE)
  ps <- sort(sapply(pl[, ..pcols], sum), decreasing = TRUE)
  api19 <- parties[year == 2019 & election == "paso"][order(-votos)]
  stopifnot(length(ps) == nrow(api19))
  xc <- data.table(agrupacion = api19$agrupacion_raw, short = api19$short,
                   polar_code = names(ps),
                   api_votes = api19$votos, polar_votes = as.numeric(ps))
  xc[, `:=`(api_share_all = 100 * api_votes / sum(api_votes),
            polar_share_all = 100 * polar_votes / sum(polar_votes))]
  xc[, diff_share_pp := api_share_all - polar_share_all]
  xc[!is.na(short), `:=`(
    api_forecast = 100 * api_votes / sum(api_votes),
    polar_forecast = 100 * polar_votes / sum(polar_votes))]
  xc[, polar_n_mesas := nrow(pl)]
  xc[, api_n_mesas := totals[year == 2019 & election == "paso", mesas]]
  stopifnot(max(abs(xc$diff_share_pp)) < 0.5)
  # sensitivity: 2019 MAE with PolAr-based PASO shares
  sens <- merge(xc[!is.na(short), .(short, polar_forecast)],
                nat[year == 2019, .(short, general_share)], by = "short")
  xc[, mae_2019_with_polar := mean(abs(sens$general_share - sens$polar_forecast))]
  nn <- names(xc)[sapply(xc, is.double)]
  xc[, (nn) := lapply(.SD, round, 3), .SDcols = nn]
  fwrite(xc, file.path(tab_dir, "paso_history_2019_crosscheck.csv"))
}

# ---- 6. figure --------------------------------------------------------------
lim <- c(0, ceiling(max(nat$paso_forecast, nat$general_share) / 5) * 5)
fig <- ggplot(nat, aes(paso_forecast, general_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(size = 2.2, colour = "#2b5d8a") +
  geom_text_repel(aes(label = short), size = 3, colour = "grey20",
                  min.segment.length = 0.2, seed = 1, max.overlaps = Inf) +
  geom_text(data = metrics, aes(x = lim[1] + 1, y = lim[2] - 1,
            label = sprintf("MAE = %.2f pp", mae_pp)),
            hjust = 0, vjust = 1, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~year, nrow = 1) +
  coord_equal(xlim = lim, ylim = lim) +
  labs(x = "PASO share (%, renormalised over general-ballot coalitions)",
       y = "General-election share (% of positive votes)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
ggsave(file.path(fig_dir, "fig_paso_history.png"), fig, width = 11, height = 3.6, dpi = 300,
       bg = "white")
ggsave(file.path(fig_dir, "fig_paso_history.pdf"), fig, width = 11, height = 3.6)

# ---- 7. key numbers ---------------------------------------------------------
yw <- c(`2011` = "Eleven", `2015` = "Fifteen", `2019` = "Nineteen", `2023` = "TwentyThree")
yn <- function(x) ifelse(x, "yes", "no")
mac <- c("% Auto-generated by paper/code/validation/03_paso_history.R. Do not edit by hand.")
add <- function(name, val) mac <<- c(mac, sprintf("\\newcommand{\\%s}{%s}", name, val))
for (i in seq_len(nrow(metrics))) {
  m <- metrics[i]; w <- yw[as.character(m$year)]
  add(paste0("HistMAE", w), sprintf("%.2f", m$mae_pp))
  add(paste0("HistMargin", w), sprintf("%.1f", m$leader_margin_paso_pp))
  add(paste0("HistMaxErr", w), sprintf("%.2f", m$max_abs_error_pp))
  add(paste0("HistLeader", w), yn(m$leader_correct))
  add(paste0("HistTopTwo", w), yn(m$top_two_correct))
  add(paste0("HistOutcome", w), yn(m$outcome_correct))
  add(paste0("HistSpearman", w), sprintf("%.2f", m$spearman))
  add(paste0("HistTurnoutPaso", w), sprintf("%.1f", m$turnout_paso))
  add(paste0("HistTurnoutGen", w), sprintf("%.1f", m$turnout_general))
  add(paste0("HistBlankPaso", w), sprintf("%.1f", m$blank_pct_paso))
  add(paste0("HistBlankGen", w), sprintf("%.1f", m$blank_pct_general))
}
if (!is.null(xc)) add("HistMAENineteenPolar", sprintf("%.2f", xc$mae_2019_with_polar[1]))
add("HistMAEMean", sprintf("%.2f", mean(metrics$mae_pp)))
add("HistNYears", nrow(metrics))
add("HistNLeaderCorrect", sum(metrics$leader_correct))
add("HistNOutcomeCorrect", sum(metrics$outcome_correct))
writeLines(mac, file.path("paper", "output", "key_numbers_history.tex"))

cat("\nNational table:\n"); print(nat_out[, .(year, short, paso_share_all, paso_forecast,
                                              general_share, error, runoff_share)])
cat("\nMetrics:\n"); print(m_out)
cat("\nRunoff:\n"); print(ro_out)
cat("\nMatching:\n"); print(matching)
cat("\nPASO parties not on general ballot:\n"); print(paso_unmatched)
cat("\nTotals:\n"); print(totals)
cat("
2019 cross-check:
"); print(xc)
