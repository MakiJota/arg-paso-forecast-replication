# Data-quality check for the market series: raw values around each election,
# and the share of days with zero change per year (stale / carried-forward data).
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
RAW <- "paper/data/raw/markets"
bl <- as.data.table(fromJSON(file.path(RAW, "blue_bluelytics.json"))); bl[, date := as.IDate(date)]
blue <- bl[source == "Blue", .(date, blue = as.numeric(value_sell))]
y <- fromJSON(file.path(RAW, "merval_yahoo.json"))$chart$result
merv <- data.table(date = as.IDate(as.POSIXct(y$timestamp[[1]], origin = "1970-01-01", tz = "America/Argentina/Buenos_Aires")),
                   merval = y$indicators$quote[[1]]$close[[1]])[, .(merval = last(merval)), by = date]
am <- as.data.table(fromJSON(file.path(RAW, "riesgo_ambito.json"))[-1, , drop = FALSE]); setnames(am, c("f", "p"))
embi <- am[, .(date = as.IDate(f, format = "%d-%m-%Y"), embi = as.numeric(gsub(",", ".", gsub("\\.", "", p))))]
all <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), list(blue, merv, embi))
for (d in c("2015-08-09", "2015-10-25", "2019-08-11", "2023-08-13", "2023-10-22", "2023-11-19")) {
  d <- as.IDate(d); cat("\n== around", format(d), "==\n"); print(all[date >= d - 5 & date <= d + 4])
}
zero_share <- function(x, v) x[!is.na(get(v))][order(date)][, .(zero_change_share = round(mean(diff(get(v)) == 0), 2), n = .N), by = .(year = year(date))]
cat("\n== Share of days with zero change, parallel peso ==\n"); print(zero_share(blue, "blue"))
cat("\n== Share of days with zero change, EMBI ==\n"); print(zero_share(embi, "embi"))
