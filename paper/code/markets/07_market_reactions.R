# =============================================================================
# Descriptive event study: market reactions to election nights, 2011-2023.
# Question: is the largest market reaction concentrated on PASO nights?
#
# Measure = SIZE of the reaction, not its sign. Markets react to new
# information; the direction depends on what the result implies for policy.
#   r     = log(first close after the election / last close before it)
#   z     = r / sd(daily log changes over the previous 120 trading days)
#   |z|   = main measure ("how unusual was this move for this market then")
#   Also a 5-trading-day window (z scaled by sqrt(5)).
#
# Series (downloaded to paper/data/raw/markets/ by fetch_markets.py):
#   blue      parallel ("blue") peso rate, sell     - mercados.ambito.com
#   official  official retail rate, sell (context)  - api.bluelytics.com.ar
#   merval_usd Merval index / blue rate             - Yahoo Finance (^MERV)
#   embi      JP Morgan EMBI spread, Argentina      - mercados.ambito.com
# Outputs: paper/output/tables/market_reactions.csv, figures/fig_market_reactions.*,
#          paper/output/key_numbers_markets.tex
# Run from the repository root.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(ggplot2) })
RAW <- "paper/data/raw/markets"; OUT <- "paper/output"

# ---------------------------------------------------------------- series
# Parallel rate: Ambito "dolar informal" (Bluelytics is rounded to whole pesos and
# stale before 2019, so it is not used for the parallel rate). Several quotes per
# day are listed newest first: keep the first (= last of the day).
ai <- as.data.table(fromJSON(file.path(RAW, "blue_ambito.json"))[-1, , drop = FALSE]); setnames(ai, c("f", "compra", "venta"))
ai[, date := as.IDate(f, format = "%d/%m/%Y")]
blue <- ai[, .SD[1], by = date][, .(date, value = as.numeric(gsub(",", ".", gsub("\\.", "", venta))))][!is.na(value)][order(date)]
bl <- as.data.table(fromJSON(file.path(RAW, "blue_bluelytics.json")))
bl[, date := as.IDate(date)]
official <- bl[source == "Oficial", .(date, value = as.numeric(value_sell))][order(date)]

y <- fromJSON(file.path(RAW, "merval_yahoo.json"))$chart$result
merv <- data.table(date = as.IDate(as.POSIXct(y$timestamp[[1]], origin = "1970-01-01", tz = "America/Argentina/Buenos_Aires")),
                   value = y$indicators$quote[[1]]$close[[1]])[!is.na(value)]
merv <- merv[, .(value = last(value)), by = date][order(date)]
merval_usd <- merge(merv, blue, by = "date", suffixes = c("_m", "_b"))[, .(date, value = value_m / value_b)]

am <- fromJSON(file.path(RAW, "riesgo_ambito.json"))
am <- as.data.table(am[-1, , drop = FALSE]); setnames(am, c("fecha", "puntos"))
embi <- am[, .(date = as.IDate(fecha, format = "%d-%m-%Y"),
               value = as.numeric(gsub(",", ".", gsub("\\.", "", puntos))))][!is.na(value)][order(date)]

series <- list(`Peso (parallel rate)` = blue, `Merval (USD)` = merval_usd,
               `Country risk (EMBI)` = embi, `Peso (official rate, context)` = official)
for (s in names(series)) cat(sprintf("%-30s %s to %s, n = %d, share of zero daily changes 2011-23 = %.2f\n", s,
                                     min(series[[s]]$date), max(series[[s]]$date), nrow(series[[s]]),
                                     series[[s]][year(date) %between% c(2011, 2023), mean(diff(value) == 0)]))

# Calendar = Argentine trading days (days the Merval trades). Every series is read
# on this calendar (last available value), so holidays and weekends are handled
# the same way for all of them.
cal <- merv$date
on_cal <- function(x) x[J(date = cal), on = "date", roll = TRUE][!is.na(value)]

# ---------------------------------------------------------------- elections (all Sundays)
ev <- data.table(
  date = as.IDate(c("2011-08-14", "2011-10-23",
                    "2015-08-09", "2015-10-25", "2015-11-22",
                    "2019-08-11", "2019-10-27",
                    "2023-08-13", "2023-10-22", "2023-11-19")),
  type = c("PASO", "General", "PASO", "General", "Runoff", "PASO", "General", "PASO", "General", "Runoff"))
ev[, year := year(date)]

# h = number of Argentine trading days after the election (main: h = 2, which
# absorbs post-election holidays and the one-day publication lag of the EMBI).
reaction <- function(x, d, h = 2, win = 120) {
  x <- on_cal(x[value > 0])
  i_pre <- x[, max(which(date < d))]               # last trading day before election day
  if (!is.finite(i_pre) || i_pre + h > nrow(x)) return(NULL)
  pre <- x[i_pre]; post_h <- x[i_pre + h]
  hist <- x[seq_len(i_pre)][, r := c(NA, diff(log(value)))][!is.na(r)]
  hist <- tail(hist, win)
  if (nrow(hist) < 40 || sd(hist$r) == 0) return(NULL)
  r <- log(post_h$value / pre$value)
  data.table(pre_date = pre$date, post_date = post_h$date, pre = pre$value, post = post_h$value,
             change_pct = 100 * (exp(r) - 1), z = r / (sd(hist$r) * sqrt(h)))
}
res <- rbindlist(lapply(names(series), function(s) rbindlist(lapply(seq_len(nrow(ev)), function(i) {
  rbindlist(lapply(c(1, 2, 5), function(h) {
    out <- reaction(series[[s]], ev$date[i], h)
    if (is.null(out)) return(NULL)
    cbind(series = s, ev[i], horizon = h, out)
  }))
}))))
res[, abs_z := abs(z)]

# Placebo distribution: the same two-day |z| for every Argentine trading day
# 2011-2023 that is not within 10 days of an election. Each event's percentile
# = share of placebo windows with a smaller move.
placebo <- rbindlist(lapply(names(series)[1:3], function(s) {
  x <- on_cal(series[[s]][value > 0])[, r := c(NA, diff(log(value)))]
  out <- vector("list", nrow(x))
  for (i in 121:(nrow(x) - 2)) {
    if (year(x$date[i]) < 2011 || year(x$date[i]) > 2023) next
    if (any(abs(as.numeric(x$date[i] - ev$date)) <= 10)) next
    sdv <- sd(x$r[(i - 119):i], na.rm = TRUE)
    if (!is.finite(sdv) || sdv == 0) next
    out[[i]] <- data.table(series = s, date = x$date[i],
                           abs_z = abs(log(x$value[i + 2] / x$value[i])) / (sdv * sqrt(2)))
  }
  rbindlist(out)
}))
res[, placebo_pct := mapply(function(s, z) 100 * placebo[series == s, mean(abs_z < z)], series, abs_z)]
res[series == "Peso (official rate, context)", placebo_pct := NA]
fwrite(placebo[, .(n_windows = .N, p50 = median(abs_z), p95 = quantile(abs_z, .95), p99 = quantile(abs_z, .99)), by = series],
       file.path(OUT, "tables/market_placebo_summary.csv"))
fwrite(res, file.path(OUT, "tables/market_reactions.csv"))
cat("\n== Placebo percentiles (two-day window) ==\n")
print(dcast(res[horizon == 2 & series != "Peso (official rate, context)"], year + type ~ series, value.var = "placebo_pct")[
  , lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])

cat("\n== Main: two-trading-day reactions (|z| = size of move relative to normal volatility) ==\n")
print(dcast(res[horizon == 2], year + type ~ series, value.var = "abs_z")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("\n== Two-day % change ==\n")
print(dcast(res[horizon == 2], year + type ~ series, value.var = "change_pct")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("\n== Five-day |z| ==\n")
print(dcast(res[horizon == 5], year + type ~ series, value.var = "abs_z")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])

# ---------------------------------------------------------------- figure
main <- res[horizon == 2 & series != "Peso (official rate, context)"]
main[, event := factor(paste(year, type), levels = rev(paste(ev$year, ev$type)))]
main[, series := factor(series, levels = c("Peso (parallel rate)", "Merval (USD)", "Country risk (EMBI)"))]
p <- ggplot(main, aes(abs_z, event, colour = type)) +
  geom_segment(aes(x = 0, xend = abs_z, yend = event), linewidth = .6) +
  geom_point(size = 2.4) +
  geom_vline(xintercept = 2, linetype = 2, colour = "grey50") +
  facet_wrap(~ series, nrow = 1) +
  scale_colour_manual(values = c(PASO = "#d1495b", General = "#3189e0", Runoff = "#7f7f7f"), name = NULL) +
  labs(x = "Size of the two-trading-day move after the election (absolute, in standard deviations of normal moves)", y = NULL,
       caption = "Dashed line: 2 standard deviations. Normal volatility = previous 120 trading days. Direction of moves not shown.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(OUT, "figures/fig_market_reactions.png"), p, width = 10, height = 4.2, dpi = 300)
ggsave(file.path(OUT, "figures/fig_market_reactions.pdf"), p, width = 10, height = 4.2)

# ---------------------------------------------------------------- key numbers
m1 <- res[horizon == 2 & series != "Peso (official rate, context)"]
fmt <- function(x, d = 1) formatC(unname(x), format = "f", digits = d)
largest <- m1[, .SD[which.max(abs_z)], by = series]
K <- c(MktMedianZPaso    = fmt(median(m1[type == "PASO", abs_z])),
       MktMedianZGeneral = fmt(median(m1[type == "General", abs_z])),
       MktMedianZRunoff  = fmt(median(m1[type == "Runoff", abs_z])),
       MktLargestIsPaso  = sum(largest$type == "PASO"),
       MktNSeries        = nrow(largest))
for (s in unique(m1$series)) for (yy in c(2019, 2023)) {
  key <- gsub("[^A-Za-z]", "", paste0("Mkt", s, ifelse(yy == 2019, "Nineteen", "TwentyThree"), "Paso"))
  K[[key]] <- fmt(m1[series == s & year == yy & type == "PASO", change_pct])
}
pz <- function(s, yy, ty) fmt(res[horizon == 2 & series == s & year == yy & type == ty, placebo_pct], 1)
K[["MktPctPesoNineteenPaso"]]    <- pz("Peso (parallel rate)", 2019, "PASO")
K[["MktPctMervalNineteenPaso"]]  <- pz("Merval (USD)", 2019, "PASO")
K[["MktPctEmbiNineteenPaso"]]    <- pz("Country risk (EMBI)", 2019, "PASO")
K[["MktPctPesoNineteenGeneral"]] <- pz("Peso (parallel rate)", 2019, "General")
K[["MktPesoNineteenGeneral"]]    <- fmt(res[horizon == 2 & series == "Peso (parallel rate)" & year == 2019 & type == "General", change_pct])
K[["MktMervalTwentyThreeGeneral"]] <- fmt(res[horizon == 2 & series == "Merval (USD)" & year == 2023 & type == "General", change_pct])
K[["MktNPlacebo"]] <- format(placebo[series == "Merval (USD)", .N], big.mark = ",")
writeLines(c("% Auto-generated by paper/code/markets/07_market_reactions.R",
             sprintf("\\newcommand{\\%s}{%s}", names(K), K)), file.path(OUT, "key_numbers_markets.tex"))
cat("\nLargest two-day reaction per series:\n"); print(largest[, .(series, year, type, change_pct = round(change_pct, 1), abs_z = round(abs_z, 1))])
