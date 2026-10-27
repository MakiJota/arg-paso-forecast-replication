# 04_ei_2015.R -- Observed 2015 vote transfers (PASO -> general, PASO -> runoff)
# by ecological inference on department-level official provisional results.
#
# Input : paper/data/raw/api_2015/{paso,generales,balotaje}/*.json
#         (downloaded by paper/code/validation/04_fetch_2015.py)
# Output: paper/data/departments_2015.csv
#         paper/output/tables/ei2015_*.csv
#         paper/output/figures/fig_ei2015_*.png
#         paper/output/validation_2015_lphom.rds
# Run from the repository root:  Rscript paper/code/validation/04_ei_2015.R [--bayes]
#
# Method: lphom (Romero et al. 2020), new_and_exit_voters = "raw". PASO origin
# counts are rescaled to each department's destination electorate (origin
# shares x destination electors, rounded, residual into NOVOTO) so that
# row/column totals match. nslphom is run as a comparison. With --bayes,
# eiPack::ei.MD.bayes gives 90% credible intervals.

suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(lphom); library(ggplot2)
})
RAW  <- file.path("paper", "data", "raw", "api_2015")
TAB  <- file.path("paper", "output", "tables")
FIG  <- file.path("paper", "output", "figures")
RUN_BAYES <- "--bayes" %in% commandArgs(trailingOnly = TRUE)
dir.create(TAB, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------ mapping
# PASO: list level (list numbers are national and identical in every department;
# verified against national list totals printed below).
PASO_LISTS <- c("3166" = "SCIOLI",      # FpV, Lista Celeste y Blanca K
                "3182" = "MACRI",       # Cambiemos, Lista G El camino del cambio
                "3185" = "SANZ",        # Cambiemos, Lista E El poder de la union
                "3243" = "CARRIO",      # Cambiemos, Lista F Republica de libres e iguales
                "3192" = "MASSA",       # UNA, Lista A Frente Renovador
                "3214" = "DELASOTA",    # UNA, Lista D Movimiento de unidad
                "3014" = "DELCANO",     # FIT, Lista 1A Renovar y fortalecer el frente
                "3261" = "ALTAMIRA",    # FIT, Lista Unidad 2U
                "3008" = "STOLBIZER",   # Progresistas
                "3100" = "RODRIGUEZSAA")# Compromiso Federal
# General / runoff: agrupacion level (one candidate per agrupacion)
AGR <- function(name) {
  n <- toupper(trimws(name))
  fifelse(grepl("FRENTE PARA LA VICTORIA", n), "SCIOLI",
  fifelse(grepl("CAMBIEMOS", n), "MACRI",
  fifelse(grepl("NUEVA ALTERNATIVA|\\(UNA\\)", n), "MASSA",
  fifelse(grepl("FRENTE DE IZQUIERDA", n), "DELCANO",
  fifelse(grepl("PROGRESISTAS", n), "STOLBIZER",
  fifelse(grepl("COMPROMISO FEDERAL", n), "RODRIGUEZSAA", "OTROS"))))))
}
ORIG <- c("SCIOLI", "MACRI", "SANZ", "CARRIO", "MASSA", "DELASOTA", "DELCANO",
          "ALTAMIRA", "STOLBIZER", "RODRIGUEZSAA", "OTROS", "BLANCO", "ANULADOS", "NOVOTO")
GEN  <- c("SCIOLI", "MACRI", "MASSA", "DELCANO", "STOLBIZER", "RODRIGUEZSAA",
          "OTROS", "BLANCO", "ANULADOS", "NOVOTO")
RUN  <- c("SCIOLI", "MACRI", "BLANCO", "ANULADOS", "NOVOTO")

# ------------------------------------------------------------------ parsing
parse_one <- function(f, election) {
  r <- fromJSON(f, simplifyVector = FALSE)
  pos <- r$valoresTotalizadosPositivos
  v <- setNames(numeric(0), character(0))
  if (election == "paso") {
    for (a in pos) for (l in a$listas) {
      k <- PASO_LISTS[l$numero]; if (is.na(k)) k <- "OTROS"
      v[k] <- sum(v[k], l$votos, na.rm = TRUE)
    }
  } else {
    for (a in pos) { k <- AGR(a$nombreAgrupacion); v[k] <- sum(v[k], a$votos, na.rm = TRUE) }
  }
  o <- r$valoresTotalizadosOtros
  v["BLANCO"]   <- o$votosEnBlanco
  v["ANULADOS"] <- o$votosNulos + o$votosRecurridosComandoImpugnados
  e <- r$estadoRecuento
  list(votes = v, electores = e$cantidadElectores, votantes = e$cantidadVotantes,
       mesas = e$mesasTotalizadas, n_pos = length(pos))
}

ELECT <- c(paso = "paso", gen = "generales", run = "balotaje")
CATS  <- list(paso = ORIG, gen = GEN, run = RUN)
menu  <- fromJSON(file.path(RAW, "menu.json"), simplifyVector = FALSE)
names_tab <- rbindlist(lapply(menu, function(e) {
  c1 <- Filter(function(c) grepl("PRESIDENTE", toupper(c$Cargo)), e$Cargos)[[1]]
  rbindlist(lapply(c1$Distritos, function(d) rbindlist(lapply(d$SeccionesProvinciales, function(sp)
    rbindlist(lapply(sp$Secciones, function(s) if (is.null(s$IdSeccion)) NULL else
      data.table(dist_id = d$IdDistrito, sec_id = s$IdSeccion,
                 distrito = toupper(d$Distrito), seccion = s$Seccion)))))))
}))
names_tab <- unique(names_tab, by = c("dist_id", "sec_id"))

build <- function(s) {
  fs <- list.files(file.path(RAW, ELECT[[s]]), pattern = "^d\\d+_s\\d+\\.json$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    id <- as.integer(regmatches(basename(f), gregexpr("\\d+", basename(f)))[[1]])
    p <- parse_one(f, s)
    v <- p$votes[CATS[[s]]]; v[is.na(v)] <- 0; names(v) <- CATS[[s]]
    cast <- sum(v[setdiff(CATS[[s]], "NOVOTO")])
    v["NOVOTO"] <- p$electores - cast
    dt <- as.data.table(as.list(v)); setnames(dt, paste0(s, "_", names(dt)))
    cbind(data.table(dist_id = id[1], sec_id = id[2]), dt,
          setnames(data.table(p$electores, p$votantes, p$mesas, cast),
                   paste0(s, c("_electores", "_votantes", "_mesas", "_cast"))))
  }))
}
D <- lapply(names(ELECT), build); names(D) <- names(ELECT)
dep <- Reduce(function(a, b) merge(a, b, by = c("dist_id", "sec_id"), all = TRUE), D)
dep <- merge(names_tab, dep, by = c("dist_id", "sec_id"), all.y = TRUE)
setorder(dep, dist_id, sec_id)
# Empty responses (no mesas) are treated as missing
for (s in names(ELECT)) {
  bad <- dep[[paste0(s, "_mesas")]] %in% c(0, NA) | dep[[paste0(s, "_electores")]] %in% c(0, NA)
  cols <- grep(paste0("^", s, "_"), names(dep), value = TRUE)
  dep[bad, (cols) := NA]
}
fwrite(dep, file.path("paper", "data", "departments_2015.csv"))
cat("Departments in file:", nrow(dep), "\n")
for (s in names(ELECT)) {
  miss <- dep[is.na(get(paste0(s, "_electores")))]
  cat(sprintf("%s: %d departments with data; missing: %s\n", s,
              sum(!is.na(dep[[paste0(s, "_electores")]])),
              if (nrow(miss)) paste(head(paste(miss$distrito, miss$seccion, sep = "/"), 30), collapse = "; ") else "none"))
  neg <- dep[get(paste0(s, "_NOVOTO")) < 0]
  if (nrow(neg)) cat("  WARNING negative NOVOTO in", nrow(neg), "departments\n")
}

# ------------------------------------------------------------------ national checks
nat <- lapply(names(ELECT), function(s) parse_one(file.path(RAW, ELECT[[s]], "national.json"), s))
names(nat) <- names(ELECT)
chk <- rbindlist(lapply(names(ELECT), function(s) {
  cats <- setdiff(CATS[[s]], "NOVOTO")
  nv <- nat[[s]]$votes[cats]; nv[is.na(nv)] <- 0
  sv <- colSums(dep[, paste0(s, "_", cats), with = FALSE], na.rm = TRUE)
  rbind(data.table(election = s, item = cats, national = as.numeric(nv), sum_departments = as.numeric(sv)),
        data.table(election = s, item = c("electores", "votantes", "mesas"),
                   national = c(nat[[s]]$electores, nat[[s]]$votantes, nat[[s]]$mesas),
                   sum_departments = c(sum(dep[[paste0(s, "_electores")]], na.rm = TRUE),
                                       sum(dep[[paste0(s, "_votantes")]], na.rm = TRUE),
                                       sum(dep[[paste0(s, "_mesas")]], na.rm = TRUE))))
}))
chk[, diff := sum_departments - national]
chk[, national_pct_cast := NA_real_]
for (s in names(ELECT)) {
  idx <- chk$election == s & chk$item %in% CATS[[s]]
  chk[idx, national_pct_cast := round(100 * national / sum(national), 2)]
}
fwrite(chk, file.path(TAB, "ei2015_national_check.csv"))
cat("\n=== National check (national query vs sum of departments) ===\n"); print(chk)

# PASO list totals for verification of the mapping
pl <- fromJSON(file.path(RAW, "paso", "national.json"), simplifyVector = FALSE)$valoresTotalizadosPositivos
lists <- rbindlist(lapply(pl, function(a) rbindlist(lapply(a$listas, function(l)
  data.table(agrupacion = trimws(a$nombreAgrupacion), numero = l$numero, lista = trimws(l$nombre), votos = l$votos)))))
lists[, category := fifelse(is.na(PASO_LISTS[numero]), "OTROS", PASO_LISTS[numero])]
lists[, pct_valid_positive := round(100 * votos / sum(votos), 2)]
fwrite(lists, file.path(TAB, "ei2015_paso_list_mapping.csv"))
cat("\n=== PASO 2015 list -> category (national provisional) ===\n"); print(lists)

# ------------------------------------------------------------------ estimation
rescale <- function(X, tot) {  # X: origin counts, tot: destination electorate
  sh <- X / rowSums(X)
  Z <- round(sh * tot)
  Z[, "NOVOTO"] <- Z[, "NOVOTO"] + (tot - rowSums(Z))
  Z
}
# The provisional count is incomplete in some departments (e.g. general 2015:
# Tucuman Capital 55 of 1267 mesas, CABA Comunas 3 and 12 and Cordoba Cruz del
# Eje 1 mesa). API electors refer to counted mesas only, so a partially counted
# department compares a subset of mesas with the full department. Main
# estimates keep departments where the counted mesas of the two elections are
# within 10% of each other (min/max >= 0.9); an all-units run is a sensitivity.
MIN_COV <- 0.9
CACHE <- file.path("paper", "output", "validation_2015_lphom.rds")
old <- if (file.exists(CACHE)) tryCatch(readRDS(CACHE), error = function(e) NULL) else NULL
run_ei <- function(dest, min_cov = MIN_COV, do_ns = TRUE) {
  dcats <- CATS[[dest]]
  ok <- !is.na(dep$paso_electores) & !is.na(dep[[paste0(dest, "_electores")]])
  m1 <- dep$paso_mesas; m2 <- dep[[paste0(dest, "_mesas")]]
  cov <- pmin(m1, m2) / pmax(m1, m2)
  excl <- dep[ok & cov < min_cov, .(distrito, seccion, paso_mesas = m1[ok & cov < min_cov],
                                    dest_mesas = m2[ok & cov < min_cov])]
  ok <- ok & cov >= min_cov
  d  <- dep[ok]
  X  <- as.matrix(d[, paste0("paso_", ORIG), with = FALSE]); colnames(X) <- ORIG
  Y  <- as.matrix(d[, paste0(dest, "_", dcats), with = FALSE]); colnames(Y) <- dcats
  keep <- colSums(Y) > 0; Y <- Y[, keep, drop = FALSE]
  X  <- rescale(X, rowSums(Y))
  stopifnot(all(X >= 0), all(Y >= 0), all(rowSums(X) == rowSums(Y)))
  t0 <- Sys.time()
  fit <- lphom(as.data.frame(X), as.data.frame(Y), new_and_exit_voters = "raw", verbose = FALSE)
  t1 <- Sys.time()
  key <- c(gen = "general", run = "runoff")[[dest]]
  ns <- NULL
  if (do_ns && !is.null(old) && !is.null(old$res[[key]]$ns) &&
      identical(old$res[[key]]$X, X) && identical(old$res[[key]]$Y, Y)) {
    ns <- old$res[[key]]$ns; cat("(nslphom reused from cache)
")
  } else if (do_ns) {
    ns <- tryCatch(nslphom(as.data.frame(X), as.data.frame(Y), new_and_exit_voters = "raw", verbose = FALSE),
                   error = function(e) { message("nslphom failed: ", conditionMessage(e)); NULL })
  }
  t2 <- Sys.time()
  cat(sprintf("\n[%s] units = %d; lphom %.1fs, nslphom %.1fs\n", dest, nrow(X),
              as.numeric(t1 - t0, units = "secs"), as.numeric(t2 - t1, units = "secs")))
  list(X = X, Y = Y, units = d[, .(dist_id, sec_id, distrito, seccion)], excluded = excl, fit = fit, ns = ns)
}
res <- list(general = run_ei("gen"), runoff = run_ei("run"))
for (s in names(res)) {
  e <- res[[s]]$excluded
  cat(sprintf("
[%s] excluded for count coverage < %.2f: %d departments
", s, MIN_COV, nrow(e)))
  print(e)
  fwrite(e, file.path(TAB, sprintf("ei2015_%s_excluded_departments.csv", s)))
}
# Sensitivity: all units with data (no coverage filter), lphom only
res_all <- list(general = run_ei("gen", 0, FALSE), runoff = run_ei("run", 0, FALSE))

out_mat <- function(M, file) {
  dt <- data.table(origin = rownames(M), round(as.data.frame.matrix(M), 2))
  fwrite(dt, file.path(TAB, file)); dt
}
for (s in names(res)) {
  r <- res[[s]]
  V <- r$fit$VTM[ORIG, colnames(r$Y)]
  cat(sprintf("\n=== %s: lphom VTM (%% of PASO origin row) ===  HETe = %.2f%%\n", s, r$fit$HETe))
  print(round(V, 1))
  out_mat(V, sprintf("ei2015_%s_matrix.csv", s))
  if (!is.null(r$ns)) {
    VN <- r$ns$VTM[ORIG, colnames(r$Y)]
    cat(sprintf("--- nslphom: HETe = %.2f%%; max |lphom - nslphom| = %.1f pp; mean = %.2f pp\n",
                r$ns$HETe, max(abs(V - VN)), mean(abs(V - VN))))
    print(round(VN, 1))
    out_mat(VN, sprintf("ei2015_%s_matrix_nslphom.csv", s))
  }
  VA <- res_all[[s]]$fit$VTM[ORIG, colnames(res_all[[s]]$Y)]
  cat(sprintf("--- all units (n = %d): HETe = %.2f%%; max |filtered - all| = %.1f pp; mean = %.2f pp
",
              nrow(res_all[[s]]$X), res_all[[s]]$fit$HETe, max(abs(V - VA)), mean(abs(V - VA))))
  out_mat(VA, sprintf("ei2015_%s_matrix_allunits.csv", s))
  # origin sizes (for judging stability of small rows)
  sz <- data.table(origin = ORIG, votes_rescaled = colSums(r$X[, ORIG]),
                   pct_of_electorate = round(100 * colSums(r$X[, ORIG]) / sum(r$X), 2))
  fwrite(sz, file.path(TAB, sprintf("ei2015_%s_origin_sizes.csv", s)))
}

# Sanity: apply matrix to national PASO origin totals (rescaled to national
# destination electorate) and compare with national destination shares.
san <- rbindlist(lapply(names(res), function(s) {
  r <- res[[s]]; V <- r$fit$VTM[ORIG, colnames(r$Y)] / 100
  dkey <- c(general = "gen", runoff = "run")[[s]]
  pick <- function(v, cats, el) {
    o <- setNames(as.numeric(v[cats]), cats); o[is.na(o)] <- 0
    o["NOVOTO"] <- el - sum(o[setdiff(cats, "NOVOTO")]); o
  }
  nx <- pick(nat$paso$votes, ORIG, nat$paso$electores)
  ny <- pick(nat[[dkey]]$votes, colnames(r$Y), nat[[dkey]]$electores)
  pred <- as.numeric((nx / sum(nx)) %*% V)
  data.table(election = s, destination = colnames(r$Y),
             predicted_pct_electorate = round(100 * pred, 2),
             actual_pct_electorate = round(100 * ny / sum(ny), 2),
             predicted_pct_valid = NA_real_, actual_pct_valid = NA_real_)[
    , `:=`(diff_pp = predicted_pct_electorate - actual_pct_electorate)]
}))
# shares among valid (positive) votes for the candidate destinations
san[, cand := !destination %in% c("BLANCO", "ANULADOS", "NOVOTO")]
san[, `:=`(predicted_pct_valid = round(100 * predicted_pct_electorate / sum(predicted_pct_electorate[cand]), 2),
           actual_pct_valid = round(100 * actual_pct_electorate / sum(actual_pct_electorate[cand]), 2)), by = election]
san[cand == FALSE, `:=`(predicted_pct_valid = NA, actual_pct_valid = NA)]
san[, cand := NULL]
fwrite(san, file.path(TAB, "ei2015_sanity_national.csv"))
cat("\n=== Sanity: matrix x national PASO vs national result ===\n"); print(san)

# ------------------------------------------------------------------ role-level table
NOMINEE <- c(SCIOLI = "SCIOLI", MACRI = "MACRI", SANZ = "MACRI", CARRIO = "MACRI",
             MASSA = "MASSA", DELASOTA = "MASSA", DELCANO = "DELCANO", ALTAMIRA = "DELCANO",
             STOLBIZER = "STOLBIZER", RODRIGUEZSAA = "RODRIGUEZSAA")
ROLE <- c(SCIOLI = "Dominant Peronist candidate (FpV, uncontested)",
          MACRI = "Internal winner (Cambiemos)", MASSA = "Internal winner (UNA)",
          DELCANO = "Internal winner (FIT)",
          SANZ = "Internal loser (Cambiemos)", CARRIO = "Internal loser (Cambiemos)",
          DELASOTA = "Internal loser (UNA)", ALTAMIRA = "Internal loser (FIT)",
          STOLBIZER = "Third force (uncontested)", RODRIGUEZSAA = "Third force (uncontested)",
          OTROS = "Minor parties eliminated at PASO",
          BLANCO = "PASO blank", ANULADOS = "PASO null/contested", NOVOTO = "PASO non-voter")
roles <- rbindlist(lapply(names(res), function(s) {
  V <- res[[s]]$fit$VTM[ORIG, colnames(res[[s]]$Y)]
  rbindlist(lapply(ORIG, function(o) {
    v <- V[o, ]; own <- NOMINEE[o]
    own_ok <- !is.na(own) && own %in% names(v)
    others <- setdiff(names(v), c("SCIOLI", "MACRI", "BLANCO", "ANULADOS", "NOVOTO", if (own_ok) own))
    data.table(election = s, role = ROLE[[o]], origin = o,
               own_nominee = if (own_ok) unname(own) else NA_character_,
               to_own_nominee = if (own_ok) v[[own]] else NA_real_,
               to_SCIOLI = if (own_ok && own == "SCIOLI") NA_real_ else v[["SCIOLI"]],
               to_MACRI  = if (own_ok && own == "MACRI")  NA_real_ else v[["MACRI"]],
               to_other_candidates = if (length(others)) sum(v[others]) else 0,
               to_blank = v[["BLANCO"]], to_null = v[["ANULADOS"]], to_abstention = v[["NOVOTO"]])
  }))
}))
num <- names(roles)[sapply(roles, is.numeric)]
roles[, (num) := lapply(.SD, round, 1), .SDcols = num]
fwrite(roles, file.path(TAB, "ei2015_roles.csv"))
cat("\n=== Role-level summary (% of PASO origin) ===\n"); print(roles)

# ------------------------------------------------------------------ heatmaps
heat <- function(V, title, file) {
  dt <- as.data.table(as.table(V)); setnames(dt, c("origin", "destination", "pct"))
  dt[, origin := factor(origin, levels = rev(rownames(V)))]
  dt[, destination := factor(destination, levels = colnames(V))]
  p <- ggplot(dt, aes(destination, origin, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.0f", pct), colour = pct > 55), size = 3) +
    scale_fill_gradientn(colours = c("#f7f9fc", "#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#104281"),
                         limits = c(0, 100), name = "% of PASO\norigin") +
    scale_colour_manual(values = c(`FALSE` = "#1f2328", `TRUE` = "white"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = "PASO 2015 origin (rows sum to 100%)", title = title,
         subtitle = "Columns: destination",
         caption = "Ecological inference (lphom) on department-level official provisional results.") +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 0),
          plot.title.position = "plot")
  ggsave(file.path(FIG, file), p, width = 7.5, height = 6, dpi = 200, bg = "white")
}
heat(res$general$fit$VTM[ORIG, colnames(res$general$Y)],
     sprintf("2015 PASO -> general transfers (HETe = %.1f%%)", res$general$fit$HETe), "fig_ei2015_general.png")
heat(res$runoff$fit$VTM[ORIG, colnames(res$runoff$Y)],
     sprintf("2015 PASO -> runoff transfers (HETe = %.1f%%)", res$runoff$fit$HETe), "fig_ei2015_runoff.png")

saveRDS(list(dep = dep, res = res, res_allunits = lapply(res_all, function(r) r[c("X", "Y", "units", "fit")]),
             min_cov = MIN_COV,
             national_check = chk, sanity = san, roles = roles),
        file.path("paper", "output", "validation_2015_lphom.rds"))

# ------------------------------------------------------------------ Bayesian EI (optional)
if (RUN_BAYES) {
  suppressPackageStartupMessages(library(eiPack))
  for (s in names(res)) {
    r <- res[[s]]; dc <- colnames(r$Y)
    df <- data.frame(r$X, r$Y); names(df) <- c(paste0("o_", ORIG), paste0("d_", dc))
    f <- as.formula(sprintf("cbind(%s) ~ cbind(%s)", paste0("d_", dc, collapse = ","),
                            paste0("o_", ORIG, collapse = ",")))
    t0 <- Sys.time()
    # ret.beta = "s" writes one .txt.gz per unit x cell into the WORKING DIRECTORY
    # (~50k files per run), so run it inside a dedicated, git-ignored folder.
    draws_dir <- file.path("paper", "output", "ei_bayes_draws", paste0("2015_", s))
    dir.create(draws_dir, recursive = TRUE, showWarnings = FALSE)
    old_wd <- setwd(draws_dir)
    m <- tryCatch(ei.MD.bayes(f, data = df, total = NULL, sample = 1000, burnin = 2000, thin = 2, ret.beta = "s"),
                  finally = setwd(old_wd))
    b <- lambda.MD(m, columns = paste0("d_", dc))
    q <- apply(b, 2, quantile, c(.05, .5, .95)) * 100
    cn <- colnames(q)
    out <- rbindlist(lapply(seq_along(cn), function(j) {
      p <- strsplit(sub("^lambda\\.", "", cn[j]), "\\.")[[1]]
      data.table(origin = sub("^o_", "", p[1]), destination = sub("^d_", "", p[2]),
                 q05 = q[1, j], median = q[2, j], q95 = q[3, j])
    }))
    out[, lphom := mapply(function(o, d) r$fit$VTM[o, d], origin, destination)]
    num <- c("q05", "median", "q95", "lphom"); out[, (num) := lapply(.SD, round, 1), .SDcols = num]
    fwrite(out, file.path(TAB, sprintf("ei2015_%s_bayes.csv", s)))
    cat(sprintf("\nBayesian EI (%s) done in %.1f min\n", s, as.numeric(Sys.time() - t0, units = "mins")))
    print(dcast(out, origin ~ destination, value.var = "median"))
  }
}
