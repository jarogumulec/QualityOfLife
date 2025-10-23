# ==== Packages ====
library(data.table)
library(dplyr)
library(readxl)

# ==== Načtení všech tabulek ====
wb   <- fread("qualityoflife_wide_WB.csv")
iep  <- fread("qualityoflofe_wide_iep.csv")
imf  <- fread("qualityoflife_wide_imf.csv")

manual <- read_excel("qualityoflife_wide_manual.xlsx") |> as.data.table()
sports <- fread("qualityoflife_wide_world_sports_rankings.csv")

# ==== Základní hygiena: typy a hlavičky ====
req <- c("Country","Year")
stopifnot(all(req %in% names(wb)),
          all(req %in% names(iep)),
          all(req %in% names(imf)),
          all(req %in% names(manual)),
          all(req %in% names(sports)))

# sjednotit typ roku na integer
for (x in list(wb, iep, imf, manual, sports)) x$Year <- as.integer(x$Year)
wb$Year     <- as.integer(wb$Year)
iep$Year    <- as.integer(iep$Year)
imf$Year    <- as.integer(imf$Year)
manual$Year <- as.integer(manual$Year)
sports$Year <- as.integer(sports$Year)

# ==== Sloučení všech zdrojů ====
merged <- Reduce(function(x, y) full_join(x, y, by = c("Country","Year")),
                 list(wb, iep, imf, manual, sports)) %>%
  arrange(Country, Year)

merged_dt <- as.data.table(merged)
setorder(merged_dt, Country, Year)

# ==== Přenesení JEDNORÁZOVÝCH hodnot (pouze tam, kde je přesně 1 hodnota) od r. 1995 ====
value_cols <- setdiff(names(merged_dt), c("Country","Year"))

part_pre95 <- merged_dt[Year < 1995]
part_95p   <- merged_dt[Year >= 1995]

# pro každou zemi a každý indikátor:
part_95p[, (value_cols) := lapply(.SD, function(v) {
  # spočítej počet nenulových (ne-NA) hodnot v dané zemi/indikátoru
  nn <- sum(!is.na(v))
  if (nn == 1L) {
    # když je právě jedna hodnota → replikuj ji do všech let v intervalu
    rep(v[which(!is.na(v))[1]], .N)
  } else {
    # jinak ponech beze změny (žádný LOCF)
    v
  }
}), by = Country, .SDcols = value_cols]

merged_out <- rbindlist(list(part_pre95, part_95p), use.names = TRUE, fill = TRUE)
setorder(merged_out, Country, Year)

# ==== Uložení ====
fwrite(merged_out, "qualityoflife_merged.csv", na = "")

# volitelná kontrola
# summary(merged_out$Year)
# length(unique(merged_out$Country))
