# =============================================================================
# Validation step 1: build a department-level (distrito x seccion, n = 528)
# dataset with the same categories as the forecast, for the three 2023 elections.
#
# Sources (DINE, provisional count, CC-BY 4.0), downloaded to paper/data/raw/:
#   * PASO 13 Aug 2023: DINE results API, presidential category, per seccion
#     (the mesa-level PASO zip is no longer online) -> paso_api/*.json
#   * General 22 Oct 2023: 2023_generales_1.zip (mesa level)
#   * Runoff 19 Nov 2023: 2023_segundavuelta.zip (mesa level)
# Output: paper/data/departments_2023.csv
# Run from the repository root.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
RAW <- "paper/data/raw"

# ---------------------------------------------------------------- PASO (API JSON)
paso_json <- fromJSON(file.path(RAW, "paso_api/paso2023_presidente_por_seccion.json"),
                      simplifyVector = FALSE)

# Pre-candidate lists are identified by name, then checked against national totals below.
list_to_cat <- function(agr, lista) {
  a <- toupper(agr); l <- toupper(lista)
  if (grepl("LIBERTAD AVANZA", a)) return("MILEI")
  if (grepl("JUNTOS POR EL CAMBIO", a))
    return(if (grepl("FUERZA DEL CAMBIO", l)) "BULLRICH" else "LARRETA")
  if (grepl("UNION POR LA PATRIA|UNIÓN POR LA PATRIA", a))
    return(if (grepl("GRABOIS|PATRIA GRANDE|JUSTA Y SOBERANA", l)) "GRABOIS" else "MASSA")
  if (grepl("HACEMOS", a)) return("SCHIARETTI")
  if (grepl("FRENTE DE IZQUIERDA", a))
    return(if (grepl("UNIR Y FORTALECER", l)) "BREGMAN" else "OTROS")   # B list = Solano
  "OTROS"
}

paso <- rbindlist(lapply(names(paso_json), function(k) {
  d <- paso_json[[k]]$data; ids <- as.integer(strsplit(k, "_")[[1]])
  pos <- rbindlist(lapply(d$valoresTotalizadosPositivos, function(g)
    rbindlist(lapply(g$listas, function(l)
      data.table(cat = list_to_cat(g$nombreAgrupacion, l$nombre), lista = l$nombre,
                 agr = g$nombreAgrupacion, votos = as.numeric(l$votos))))))
  o <- d$valoresTotalizadosOtros; er <- d$estadoRecuento
  blanco <- as.numeric(o$votosEnBlanco)
  nulos  <- as.numeric(o$votosNulos) + as.numeric(o$votosRecurridosComandoImpugnados)
  out <- pos[, .(votos = sum(votos)), by = cat]
  out <- rbind(out, data.table(cat = c("BLANCO", "ANULADOS"), votos = c(blanco, nulos)))
  votantes <- sum(out$votos)                 # votes cast, consistent with the categories
  out <- rbind(out, data.table(cat = "NOVOTO", votos = max(as.numeric(er$cantidadElectores) - votantes, 0)))
  out[, `:=`(distrito_id = ids[1], seccion_id = ids[2])]
  attr(out, "lists") <- pos
  out
}))

# National check of list mapping: print every list and its assigned category
lists_check <- rbindlist(lapply(names(paso_json), function(k) {
  rbindlist(lapply(paso_json[[k]]$data$valoresTotalizadosPositivos, function(g)
    rbindlist(lapply(g$listas, function(l) data.table(agr = g$nombreAgrupacion, lista = l$nombre,
                                                      votos = as.numeric(l$votos))))))
}))[, .(votos = sum(votos)), by = .(agr, lista)][order(-votos)]
lists_check[, cat := mapply(list_to_cat, agr, lista)]
cat("\n== PASO lists (top 12) and assigned category ==\n"); print(head(lists_check, 12))

# ---------------------------------------------------------------- General & runoff (mesa CSV)
read_mesa <- function(zipfile, inner) {
  dest <- file.path(RAW, inner)
  if (!file.exists(dest)) unzip(file.path(RAW, zipfile), files = inner, exdir = RAW)
  fread(dest, select = c("distrito_id", "seccion_id", "mesa_id", "circuito_id", "mesa_electores",
                         "cargo_id", "agrupacion_nombre", "votos_tipo", "votos_cantidad"),
        encoding = "UTF-8", showProgress = FALSE)[cargo_id == 1]
}
gen_cat <- function(agr, tipo) {
  a <- toupper(agr)
  fifelse(tipo == "EN BLANCO", "BLANCO",
  fifelse(tipo != "POSITIVO", "ANULADOS",
  fifelse(grepl("LIBERTAD AVANZA", a), "MILEI",
  fifelse(grepl("JUNTOS POR EL CAMBIO", a), "BULLRICH",
  fifelse(grepl("UNION POR LA PATRIA|UNIÓN POR LA PATRIA", a), "MASSA",
  fifelse(grepl("HACEMOS", a), "SCHIARETTI",
  fifelse(grepl("FRENTE DE IZQUIERDA", a), "BREGMAN", "OTROS")))))))
}
aggregate_mesa <- function(m) {
  m[, cat := gen_cat(agrupacion_nombre, votos_tipo)]
  cat("  votos_tipo values:", paste(unique(m$votos_tipo), collapse = ", "), "\n")
  v <- m[, .(votos = sum(as.numeric(votos_cantidad), na.rm = TRUE)), by = .(distrito_id, seccion_id, cat)]
  e <- unique(m[, .(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores)])[
         , .(electores = sum(as.numeric(mesa_electores))), by = .(distrito_id, seccion_id)]
  t <- v[, .(votantes = sum(votos)), by = .(distrito_id, seccion_id)]
  nv <- merge(e, t)[, .(distrito_id, seccion_id, cat = "NOVOTO", votos = pmax(electores - votantes, 0))]
  rbind(v, nv)
}
cat("\nReading general election (1.1 GB)...\n")
gen <- aggregate_mesa(read_mesa("generales_2023.zip", "2023_Generales/ResultadoElectorales_2023_Generales.csv"))
cat("Reading runoff...\n")
run <- aggregate_mesa(read_mesa("balotaje_2023.zip", "2023_segundavuelta/ResultadosElectorales_2023_SegundaVuelta.csv"))

# ---------------------------------------------------------------- Combine (wide)
wide <- function(x, prefix) {
  w <- dcast(x, distrito_id + seccion_id ~ cat, value.var = "votos", fill = 0, fun.aggregate = sum)
  setnames(w, setdiff(names(w), c("distrito_id", "seccion_id")),
           paste0(prefix, setdiff(names(w), c("distrito_id", "seccion_id"))))
  w
}
dep <- Reduce(function(a, b) merge(a, b, by = c("distrito_id", "seccion_id")),
              list(wide(paso, "paso_"), wide(gen, "gen_"), wide(run, "run_")))
fwrite(dep, "paper/data/departments_2023.csv")

# ---------------------------------------------------------------- Sanity checks
nat <- function(prefix) { s <- colSums(dep[, grep(paste0("^", prefix), names(dep)), with = FALSE]); s }
cat("\n== Departments matched in all three elections:", nrow(dep), "of", length(paso_json), "PASO sections ==\n")
for (p in c("paso_", "gen_", "run_")) { cat("\n", p, "\n"); print(round(nat(p))) }
pos_gen <- nat("gen_")[paste0("gen_", c("MASSA", "MILEI", "BULLRICH", "SCHIARETTI", "BREGMAN"))]
cat("\nGeneral, % of positive votes:\n"); print(round(100 * pos_gen / sum(pos_gen), 2))
pos_run <- nat("run_")[paste0("run_", c("MILEI", "MASSA"))]
cat("\nRunoff, % of positive votes:\n"); print(round(100 * pos_run / sum(pos_run), 2))
